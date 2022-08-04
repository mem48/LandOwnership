library(purrr)
library(combinat)
library(dplyr)
library(stringr)
library(furrr)
library(stringr)

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

# Clean compas directions
freehold_pc_land$`Property Address` <- gsub("northern","north",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("southern","south",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("eastern","east",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("western","west",freehold_pc_land$`Property Address`, ignore.case = TRUE)

freehold_pc_land$`Property Address` <- gsub("northly","north",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("southernly","south",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("easterly","east",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("westerly","west",freehold_pc_land$`Property Address`, ignore.case = TRUE)

freehold_pc_land$`Property Address` <- gsub("north-east","northeast",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("south-east","southeast",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("north-west","northwest",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("south-west","southwest",freehold_pc_land$`Property Address`, ignore.case = TRUE)

freehold_pc_land$`Property Address` <- gsub("north east","northeast",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("south east","southeast",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("north west","northwest",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("south west","southwest",freehold_pc_land$`Property Address`, ignore.case = TRUE)

# Clean spelling errors
freehold_pc_land$`Property Address` <- gsub("\\badjoing\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\badjoingin\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\badjoinining\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\bADJOINNG\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\badjoning\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)
freehold_pc_land$`Property Address` <- gsub("\\bajoining\\b","adjoining",freehold_pc_land$`Property Address`, ignore.case = TRUE)

# Some have text after the postcode, and removing poscoe reduce string length
postcode <- str_extract_all(freehold_pc_land$`Property Address`, postcode_rx)

postcode[lengths(postcode) == 0] <- NA
summary(lengths(postcode))
postcode <- unlist(postcode)
summary(postcode == freehold_pc_land$PostalCode)

pa <- str_split(freehold_pc_land$`Property Address`, postcode_rx)
summary(lengths(pa))
pa1 <- sapply(pa,`[[`, 1)
pa2 <- sapply(pa[lengths(pa) == 2],`[[`, 2)
unique(pa2)

freehold_pc_land$`Property Address` <- pa1

# Produce possible introductions
compas = c("north","east","south","west",
           "northeast","southeast","northwest","southwest",
           "front","back")

compas2 = expand.grid(compas, compas)
compas2 = compas2[compas2$Var1 != compas2$Var2,]

compas <- c(compas, paste0(compas2$Var1," and ",compas2$Var2), paste0(compas2$Var1," side and ",compas2$Var2))


lnd = c("land ","land and buildings ","land and building ","land an buildings ",
        "garden land ","front garden land ",
        "land and buildings at the back of and ", "land at Access Road ",
        "being land and buildings ","forming part of the ",
        "Land and Apartments ","land and Apartment Block ","",
        "land and barn ","land and barns ",
        "land and flats ","land and garage ","land and garages ",
        "Land and premises ","land and properties ")
# Identify the type of land description
# Idea one identified can be removed to get geocodeable address

jn1 = c("lying to the ","lying on the ","lying to the back ","lying to the read ","lying to ",
        "on the ","to the ","on ","adjoining on the ","adjoining the ","at the ","adjoining ",
        "fronting to the ")
jn2 = c("at the back ","at back ","at the rear ","at rear ","to the rear of",
        "at the back and side ","at the rear and side ",
        "lying to the back ", "lying to the rear ",
        "associated with ","association with ",
        "at ","at, ","in ","and ","","of ", "of the ","off ",
        "known as ","formerly known as ",
        "being the former site of ","being part ",
        "comprising plots ","comprising ",
        "at the side of ","adjoining ","adjoining","adjoining on the ",
        "adjoining at the back ","adjoining the back and south east side ",
        "adjoining the front ",
        "adjacent to",
        "at the front ","fronting on ","fronting ","in the front ","in front ","the front ","at front ",
        "at the front of and garage at the back ",
        "at the front and side ",
        "the front and back ","at the front and back ","front garden ","the roadway in front ",
        "forming part ","comprising part ",
        "forming part of the highway fronting ",
        "forming part of the highway at the back ",
        "forming part of the highway and lying to the front ",
        "forming part of the roadway in front ",
        "forming part of the road at the back ",
        "forming part of the site ",
        "forming part of the nature area at the back ",
        "forming part of the garden at the back ",
        "at the front of and parking areas at the back ",
        "part of ",
        "numbered ")
end = c("of ","side ","side of ","side and back of ","side of and at the back of ","",",","and back of ","and at the back of ",
        "and West sides of and at the back of ","side and at the back of ")

perms = expand.grid(lnd, jn1, paste0(compas," "),end)
perms = paste0(perms$Var1, perms$Var2, perms$Var3, perms$Var4)

perms2 = expand.grid(lnd, jn2, end)
perms2 = paste0(perms2$Var1, perms2$Var2, perms2$Var3)

perms_all = c(perms, perms2)
perms_all = unique(perms_all)
perms_all = perms_all[perms_all != ""]

message(Sys.time()) # 18:52
plan(multisession, workers = 28)
freehold_pc_land$land_type = future_map_chr(freehold_pc_land$`Property Address`, check_match2, perms = perms_all, .progress = TRUE)
plan(sequential)
message(Sys.time())

# bar <- as.data.frame(table(freehold_pc_land$land_type))
summary(is.na(freehold_pc_land$land_type))

freehold_pc_land$land_type_multi <- grepl("|",freehold_pc_land$land_type, fixed = TRUE) 

freehold_pc_land_complex <- freehold_pc_land[is.na(freehold_pc_land$land_type) | 
                                               freehold_pc_land$land_type_multi,]
freehold_pc_land_simple <- freehold_pc_land[!is.na(freehold_pc_land$land_type) & 
  !freehold_pc_land$land_type_multi,]


freehold_pc_land_simple$AddressLine  <- map2_chr(.x = freehold_pc_land_simple$land_type, 
                                          .y = freehold_pc_land_simple$`Property Address`, 
                                          .f = gsub, replacement = "", ignore.case = TRUE)


# TODO: nearly there clean up, split addresses, and send for geocoding
foo = freehold_pc_land_simple$AddressLine[grepl("\\bside\\b",freehold_pc_land_simple$AddressLine, ignore.case = FALSE)]
# A few left  but into diminishing returns

# Clena out trailing (
freehold_pc_land_simple$AddressLine <- gsub("\\($","",freehold_pc_land_simple$AddressLine,)

# Most of these are Land near address so one point per title make more sence
freehold_pc_land_simple$AddressLine <- map_chr(freehold_pc_land_simple$AddressLine, clean_numbers)



