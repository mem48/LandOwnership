library(sf)
library(dplyr)
library(tmap)
tmap_mode("view")

dat = readRDS("data/bing_final/bing_geocoded_good.Rds")
datm = readRDS("data/bing_final/bing_geocoded_medium.Rds")

dat = rbind(dat, datm)

source("R/find_onedrive.R")
#source("R/address_functions.R")
onedrive <- find_onedrive()

dir.create("tmp")
unzip(file.path(onedrive,"Land Registry/UK Ownership/CCOD_FULL_2022_07.zip"),
      exdir = "tmp")

lr <- readr::read_csv("tmp/CCOD_FULL_2022_07.csv", lazy = FALSE)
unlink("tmp", recursive = TRUE)

lr <- lr[,c("Title Number","Tenure","Property Address",
            "Proprietor Name (1)","Company Registration No. (1)",
            "Proprietorship Category (1)")]
names(lr) <- c("Title","Tenure","Property_Address",
               "Proprietor","Company_No",
               "Category")

dat = dat[,c("Title.Number",
             "formattedAddress","entityType")]
names(dat) <- c("Title","geocoded_address","geocode_type","geometry")

dat2 = left_join(dat, lr, by = "Title")
qtm(dat2[sample(1:nrow(dat), 1000),])

st_write(dat2,"data/tilegeojson/uk_owners.geojson", delete_dsn = TRUE)
st_write(dat2[sample(1:nrow(dat), nrow(dat)/100),],"data/tilegeojson/uk_owners_sample.geojson", delete_dsn = TRUE)
