# Send a fiel to be geocoded
library(sf)
library(tmap)
source("R/bing_api.R")

files_todo <- list.files("data/for_geocoding", pattern = ".csv")
files_todo <- gsub(".csv","",files_todo, fixed = TRUE)
files_done <- list.files("data/geocoded")
files_done <- gsub(".Rds","",files_done, fixed = TRUE)

# Skip any done already
files_todo <- files_todo[!files_todo %in% files_done]
files_todo <- files_todo[1]

message("Today's file is: ", files_todo)
dat = read.csv(paste0("data/for_geocoding/",files_todo,".csv"))

message(Sys.time(), " starting")
res = bing_geocode_batch(dat)
message(Sys.time(), " finished")
res_missing = res[is.na(res$latitude),]
res = res[!is.na(res$latitude),]
res = sf::st_as_sf(res, coords = c("longitude","latitude"), crs = 4326)

saveRDS(res,paste0("data/geocoded/",files_todo,".Rds"))
saveRDS(res_missing,paste0("data/geocoded/",files_todo,"_failed.Rds"))

tmap_mode("view")
tm_shape(res) +
  tm_dots(col = "entityType", popup.vars = names(res)[1:15])
table(res$confidence)
table(res$entityType)

#23:14 start
# 22% in 8 min 100% in 36 min
# 50000 in 3 hours

# for(i in seq_len(length(files_todo))){
#   message(Sys.time()," ",files_todo[i])
#   # Send
#   res1 = bing_geocode(paste0("data/for_geocoding/",files_todo[i],".csv"))
#   # Check Status
#   Sys.sleep(8)
#   
#   for(j in 1:50){
#     res2 = bing_geocode_status(res1$id)
#     check = res2$resources[[1]]$status
#     if(is.null(check)){
#       check = "Fail"
#     }
#     
#     if(check == "Completed"){
#       break
#     } else {
#       message(Sys.time(), " Sleeping for 5 seconds")
#       Sys.sleep(5)
#     }
#     if(j == 50){
#       stop("Waiting too long - please check")
#     }
#   }
#   
#   res3 = bing_geocode_download(res1$id)
#   res3 = bing_to_sf(res3)
#   saveRDS(res3, paste0("data/geocoded/",files_todo[i],".Rds"))
# 
#   rm(res1, res2, res3)
#   
# }




# Api limited to 50