library(purrr)
library(combinat)
library(dplyr)
library(stringr)
library(furrr)

source("R/address_functions.R")
freehold_pc_land = readRDS("data/UK_freehold_pc_land.Rds")
summary(freehold_pc_land$n_postcode)

freehold_pc_land_multi = freehold_pc_land[freehold_pc_land$n_postcode > 1,]
freehold_pc_land = freehold_pc_land[freehold_pc_land$n_postcode <= 1,]

# Split at the end of eahc postcode
postcode_rx = c("\\b(?:[A-Za-z][A-HJ-Ya-hj-y]?[0-9][0-9A-Za-z]? ?[0-9][A-Za-z]{2}|[Gg][Ii][Rr] ?0[Aa]{2})\\b")


split_locs <- stringr::str_locate_all(freehold_pc_land_multi$`Property Address`, postcode_rx)

freehold_pc_land_multi <- split(freehold_pc_land_multi, seq_len(nrow(freehold_pc_land_multi)))

res <- list()

for(i in seq_len(length(freehold_pc_land_multi))){
  
  df_sub <- freehold_pc_land_multi[[i]]
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

# Clean up spare brackets and joing words
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

# Some of these addresses nolonger refer to land
res_clean$land <- grepl("\\bland\\b", res_clean$AddressLine, ignore.case = TRUE)
summary(res_clean$land)

res_clean_land <- res_clean[res_clean$land,]
res_clean <- res_clean[!res_clean$land,]

# Some places are  address" and land  "

freehold_pc_land$andLand <- grepl("\\band (other )?(adjoining )?(adjacent )?(surrounding )?(associated )?(garden )?land\\b", 
                                  freehold_pc_land$`Property Address`, ignore.case = TRUE)
summary(freehold_pc_land$andLand)

freehold_pc_land_andland <- freehold_pc_land[freehold_pc_land$andLand,]
freehold_pc_land <- freehold_pc_land[!freehold_pc_land$andLand,]

res_clean_land$andLand <- grepl("\\band (other )?(adjoining )?(adjoining )?(surrounding )?(associated )?(garden )?land\\b", 
                                res_clean_land$AddressLine, ignore.case = TRUE)
summary(res_clean_land$andLand)

res_clean_land_andland <- res_clean_land[res_clean_land$andLand,]
res_clean_land <- res_clean_land[!res_clean_land$andLand,]

# Produce possible introductions
compas = c("north","east","south","west",
           "northern","eastern","southern","western",
           "north east","south east","north west","south west",
           "northeast","southeast","northwest","southwest",
           "north-east","south-east","north-west","south-west")

# Common land phrases
# land adjoining
# land lying to the south of
# land on the west side of
# land on the south west side of
# land at the back of 
# land at the rear of 
# land associated with 
# ... and land and buildings at
# ... and land and building on the North side ..
# ... and land adjoining
# ... and other land
# land at 
# land fronting on
# ...  and land at the rear 

#lnd = c("freehold land ","land ") # deal with freehold later
lnd = c("land ","land and buildings ","land and building ")
# Identify the type of land description
# Idea one identified can be removed to get geocodeable address

jn1 = c("lying to the ","lying on the ","lying to the back ","lying to the read ",
        "on the ","to the ","on ")
jn2 = c("at the back ","at the rear ","at rear ","to the rear of",
        "associated with ", "at ","in ","known as ","formerly known as ",
        "being the former site of ",
        "at the side of ","adjoining ","adjacent to",
        "fronting on ","fronting ","in the front ","in front ",
        "forming part ","comprising part ")
end = c("of ","side of","",",")

perms = expand.grid(lnd, jn1, paste0(compas," "),end)
perms = paste0(perms$Var1, perms$Var2, perms$Var3, perms$Var4)

perms2 = expand.grid(lnd, jn2, end)
perms2 = paste0(perms2$Var1, perms2$Var2, perms2$Var3)

perms = c(perms, perms2)

check_match <- function(x, perms){
  y = vapply(perms, grepl, TRUE, x = x, USE.NAMES = FALSE, ignore.case = TRUE)
  y = perms[y]
  ly = length(y)
  
  if(ly == 0){
    return(NA_character_)
  }
  if(ly == 1){
    return(y)
  }
  
  # check for short matches in long matches
  y = data.frame(y = y)
  y$nchar <- nchar(y$y)
  if(length(y[y$nchar == max(y$nchar)]) > 1){
    y <- paste0(y$y, collapse = "|")
    return(y)
  } else {
    y$sub <- vapply(y$y, grepl, TRUE, x = y$y[y$nchar == max(y$nchar)], USE.NAMES = FALSE, ignore.case = TRUE)
    y <- y$y[!y$sub | y$nchar == max(y$nchar)]
  }
  
  if(length(y) > 1){
    y <- paste0(y, collapse = "|")
  }
  y
}

plan(multisession, workers = 18)
freehold_pc_land$land_type = future_map_chr(freehold_pc_land$`Property Address`, check_match, perms = perms, .progress = TRUE)
plan(sequential)

# bar <- as.data.frame(table(freehold_pc_land$land_type))
summary(is.na(freehold_pc_land$land_type))

freehold_pc_land$land_type_multi <- grepl("|",freehold_pc_land$land_type, fixed = TRUE) 

freehold_pc_land_complex <- freehold_pc_land[is.na(freehold_pc_land$land_type) | freehold_pc_land$land_type_multi,]
freehold_pc_land <- freehold_pc_land[!is.na(freehold_pc_land$land_type),]


freehold_pc_land$AddressLine  <- map2_chr(.x = freehold_pc_land$land_type, 
                                          .y = freehold_pc_land$`Property Address`, 
                                          .f = gsub, replacement = "", ignore.case = TRUE)

# TODO: nearly there clean up, split addresses, and send for geocoding
