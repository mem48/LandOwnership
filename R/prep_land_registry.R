library(sf)
library(tmap)
library(data.table)

source("R/find_onedrive.R")
onedrive = find_onedrive()

path = file.path(onedrive,"Land Registry/INSPIRE Polygons")
zips = list.files(path)

polys <- list()

for(i in 1:length(zips)){
  dir.create("tmp")
  message(zips[i])
  unzip(file.path(onedrive,"Land Registry/INSPIRE Polygons",zips[i]),
        exdir = "tmp")
  poly <- read_sf("tmp/Land_Registry_Cadastral_Parcels.gml")
  poly <- st_transform(poly, 4326)
  polys[[i]] <- poly
  unlink("tmp", recursive = TRUE)
  rm(poly)
}

names(polys) <- gsub(".zip","",zips)

polys2 <- rbindlist(polys, idcol = "local_authority")
polys2 <- st_as_sf(polys2)

saveRDS(polys2, "data/INSPIRE_polygons_raw.Rds")

polys2 <- polys2[!duplicated(polys2$geometry),]
polys2 <- polys2[,c("local_authority","INSPIREID","geometry")]

st_write(polys2,"data/tilegeojson/inspire.geojson")

summary(duplicated(poly$INSPIREID))



poly <- st_transform(poly, 4326)


qtm(poly)

