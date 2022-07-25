files_done <- list.files("data/geocoded", full.names = TRUE)

library(dplyr)
library(sf)
library(tmap)
tmap_mode("view")

res <- list()

for(i in seq_len(length(files_done))){
  res[[i]] <- readRDS(files_done[i])
}

res_all <- bind_rows(res)

qtm(res_all, dots.col = "Confidence")
