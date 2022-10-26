# Parse Land Registry Data
library(readr)
library(purrr)
library(dplyr)
library(stringr)

source("R/find_onedrive.R")
source("R/address_functions.R")
onedrive <- find_onedrive()

dir.create("tmp")
unzip(file.path(onedrive,"Land Registry/Overseas Ownership/OCOD_FULL_2022_07.zip"),
      exdir = "tmp")

lr <- readr::read_csv("tmp/OCOD_FULL_2022_07.csv", lazy = FALSE)
unlink("tmp", recursive = TRUE)

lr$Id <- seq_len(nrow(lr))
lr <- lr[,c(1:7,40)]
postcode_rx = c("\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b")

lr$n_postcode <- stringi::stri_count_regex(lr$`Property Address`, postcode_rx)
summary(lr$n_postcode)

lr <- lr[!is.na(lr$n_postcode),]

lr_mulitpc <- lr[lr$n_postcode > 1,]
lr <- lr[lr$n_postcode <= 1,]

# Split up the multiple postcodes
split_locs <- stringr::str_locate_all(lr_mulitpc$`Property Address`, postcode_rx)
lr_mulitpc <- split(lr_mulitpc, seq_len(nrow(lr_mulitpc)))
res <- list()

for(i in seq_len(length(lr_mulitpc))){
  
  df_sub <- lr_mulitpc[[i]]
  split_sub <- split_locs[[i]]
  breaks <- c(split_sub[seq(1, nrow(split_sub) - 1),2], nchar(df_sub$`Property Address`))
  starts <- c(1, breaks[seq(1, length(breaks) - 1)] + 1)
  sections <- matrix(
    c(starts,breaks)
    , ncol = 2)
  
  pa <- list()
  for(j in seq(1, nrow(sections))){
    pa[[j]] <- substr(df_sub$`Property Address`,sections[j,1],sections[j,2])
  }
  pa <- unlist(pa)
  df2 <- df_sub[rep(1, times = length(pa)),]
  df2$AddressLine <- pa
  df2$PostalCode <- stringr::str_match(df2$AddressLine, postcode_rx)[,1]
  res[[i]] <- df2
  
}

res_clean <- bind_rows(res)

# Extract the postcodes
res_clean$AddressLine <- str_replace(res_clean$AddressLine, postcode_rx, "")

# Clean up spare brackets and joining words
res_clean$AddressLine <- sub("^and\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\s\\sand\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\sand\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^[[:punct:]] and\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\s[[:punct:]] and\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\)\\s[[:punct:]] and\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\)[[:punct:]] ","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^[[:punct:]]\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^[[:punct:]]","",res_clean$AddressLine)
res_clean$AddressLine <- sub("^\\s","",res_clean$AddressLine)
res_clean$AddressLine <- sub("\\s\\(\\)$","",res_clean$AddressLine)
res_clean$AddressLine <- sub("\\s\\($","",res_clean$AddressLine)

rm(res, split_locs, sections, breaks, i, j, df2, df_sub, pa, split_sub)

lr$AddressLine <- lr$`Property Address`
res_clean$Postcode <- res_clean$PostalCode
res_clean <- res_clean[,names(lr)]
lr <- rbind(lr, res_clean)
rm(res_clean, lr_mulitpc)

text_rem <- readxl::read_excel("data/common_land_terms.xlsx")
text_rem$remove[is.na(text_rem$remove)] = "f"
text_rem1 <- unique(text_rem$term[text_rem$remove == "t"])

text_rem <- data.frame(term = c(text_rem1))
text_rem$nchar <- nchar(text_rem$term)
text_rem <- text_rem[order(text_rem$nchar, decreasing = TRUE),]
text_rem <- text_rem[!is.na(text_rem$term),]

