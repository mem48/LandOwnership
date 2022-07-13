# Parse Land Registry Data
library(readr)
library(purrr)

source("R/find_onedrive.R")
onedrive <- find_onedrive()

dir.create("tmp")
unzip(file.path(onedrive,"Land Registry/UK Ownership/CCOD_FULL_2022_07.zip"),
      exdir = "tmp")

lr <- readr::read_csv("tmp/CCOD_FULL_2022_07.csv")

freehold <- lr[lr$Tenure == "Freehold",]
freehold <- freehold[,c(1,3:7)]

# Need to clean up addres to match Bing API needs
# AddressLine,	        Locality,	  AdminDistrict,	PostalCode,	CountryRegion
# 120 Herrington Road, 	Washington, 	           ,	NE99 9ZZ,	  United Kingdom

names(freehold)[6] <- "PostalCode"
names(freehold)[3] <- "AdminDistrict"
freehold$CountryRegion <- "United Kingdom"

# Remove postcodes from the Address
freehold$AddressLine <- map2_chr(freehold$`Property Address`, freehold$PostalCode,
                 function(x,y){
                   gsub(paste0("(",y,")"),"",x, fixed = TRUE)
                 } )

freehold_pc <- freehold[!is.na(freehold$PostalCode),]
freehold_nopc <- freehold[is.na(freehold$PostalCode),]

# Check for "land" \\b is a word boundary
freehold_pc_land <- freehold_pc[grepl("\\bland\\b", freehold_pc$AddressLine),]

# Common phrases
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


