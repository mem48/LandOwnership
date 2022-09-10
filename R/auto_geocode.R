library(sf)
library(tmap)
source("R/bing_api.R")
path <- "C:/Users/malco/OneDrive - University of Leeds/Data/Land Ownership"

for(i  in 1:100){
  # Sleep until 4am
  t_now <- lubridate::now("GMT")
  t_start <- lubridate::today("GMT") + 1 + lubridate::hours(3)
  t_diff <- difftime(t_start, t_now, units="secs")
  t_diff <- round(as.numeric(t_diff))
  
  message(Sys.time()," sleeping for ",t_diff," seconds")
  Sys.sleep(t_diff)
  
  # Do the Geocoding
  
  files_todo <- list.files(paste0(path,"/for_geocoding"), pattern = ".csv")
  files_todo <- gsub(".csv","",files_todo, fixed = TRUE)
  files_done <- list.files(paste0(path,"/geocoded"))
  files_done <- gsub(".Rds","",files_done, fixed = TRUE)
  files_done <- files_done[!grepl("_failed",files_done)]
  
  message("Done ",length(files_done)," of ",length(files_todo)," batches")
  
  # Skip any done already
  files_todo <- files_todo[!files_todo %in% files_done]
  if(length(files_todo) == 0){
    stop("No files left to do")
  }
  files_todo <- files_todo[1]
  
  message("Today's file is: ", files_todo)
  dat = read.csv(paste0(path,"/for_geocoding/",files_todo,".csv"))
  if(nrow(dat) < 50000){
    message("Short file today only ",nrow(dat)," addresses")
    
  }
  message(Sys.time(), " starting")
  res = bing_geocode_batch(dat)
  message(Sys.time(), " finished")
  res_missing = res[is.na(res$latitude),]
  res = res[!is.na(res$latitude),]
  res = sf::st_as_sf(res, coords = c("longitude","latitude"), crs = 4326)
  
  saveRDS(res,paste0(path,"/geocoded/",files_todo,".Rds"))
  saveRDS(res_missing,paste0(path,"/geocoded/",files_todo,"_failed.Rds"))
  
  rm(res, res_missing, dat, files_done, files_todo, t_now, t_start, t_diff)
}

