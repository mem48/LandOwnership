source("R/bing_api.R")
library(sf)

nm = "UK_freehold_pc_single_short_batch_1" # The batch to do

path = paste0("data/for_geocoding/",nm,".csv")

geocode_res = bing_geocode(path)
geocode_status = bing_geocode_status(geocode_res$id)

results = bing_geocode_download(geocode_res$id)
results = bing_to_sf(results)

saveRDS(results,paste0("data/geocoded/",nm,".Rds"))