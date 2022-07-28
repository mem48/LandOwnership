#Read in Bing Resutls and parse
library(dplyr)
library(sf)
library(tmap)
tmap_mode("view")

fls <- list.files("data/geocoded/")
fls <- fls[!grepl("_old",fls)]

fls_fail <- fls[grepl("_failed",fls)]
fls <- fls[!grepl("_failed",fls)]

# Read in worked

res <- list()
for(i in seq(1, length(fls))){
  res[[i]] <- readRDS(paste0("data/geocoded/",fls[i]))
}
res <- bind_rows(res)

res_fail <- list()
for(i in seq(1, length(fls_fail))){
  res_fail[[i]] <- readRDS(paste0("data/geocoded/",fls_fail[i]))
}
res_fail <- bind_rows(res_fail)

message(nrow(res)," geocoded")
message(nrow(res_fail)," failed")

bounds <- readRDS("data/EnglandWalesBuff.Rds")
bounds <- st_transform(bounds, 4326)
# Split out by quality
# Goode one are entityType == Address, confidence == High, and within England and Wales

res$check_id <- 1:nrow(res)

res_good <- res[bounds,]
res_good <- res_good[res_good$entityType == "Address",]
res_good <- res_good[res_good$confidence == "High",]

res_medium <- res[bounds,]
res_medium <- res_medium[res_medium$entityType == "Address",]
res_medium <- res_medium[res_medium$confidence == "Medium",]

# Medium ones are mixed bag of good and bad
summary(res_good$postalCode.x == res_good$postalCode.y)
summary(res_medium$postalCode.x == res_medium$postalCode.y)
foo = res_good[res_good$postalCode.x != res_good$postalCode.y,]
bar = res_medium[res_medium$postalCode.x != res_medium$postalCode.y,]

res_poor <- res[!res$check_id %in% c(res_good$check_id, res_medium$check_id),]

saveRDS(res_good,"data/bing_final/bing_geocoded_good.Rds")
saveRDS(res_medium,"data/bing_final/bing_geocoded_medium.Rds")
saveRDS(res_poor,"data/bing_final/bing_geocoded_poor.Rds")
saveRDS(res_fail,"data/bing_final/bing_geocoded_fail.Rds")

