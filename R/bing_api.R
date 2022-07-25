bing_delete_data <- function(dataSourceName, accessId, key = Sys.getenv("Bing_master_key")){
  
  url <- paste0("http://spatial.virtualearth.net/REST/v1/data/",
               accessId,
               "/",
               dataSourceName,
               "?",
               "key=",
               key)
  res <- httr::GET(url)
  content <- httr::content(res, type = "text")

}

bing_get_data_sources <- function(key = Sys.getenv("Bing_master_key")){
  url <- paste0("http://spatial.virtualearth.net/REST/v1/data?key=",
                key)
  res <- httr::GET(url)
  content <- httr::content(res, type = "text")
  content <- xml2::read_xml(content)
  content <- xml2::as_list(content)
  return(content$service$workspace$title)
}

bing_upload_data <- function(path,
                             dataSourceName = NULL, 
                             input = "csv",
                             loadOperation = "complete", 
                             master_key = Sys.getenv("Bing_master_key")){
  # if(is.null(accessId)){
  #   accessId <- paste(sample(c(letters, LETTERS, 0:9), 20, TRUE), collapse = "")
  #   message("No accessId provided, So I created one: ",accessId)
  # }
  
  if(is.null(dataSourceName)){
    dataSourceName <- strsplit(path,"/", fixed = TRUE)
    dataSourceName <- dataSourceName[[1]]
    dataSourceName <- dataSourceName[length(dataSourceName)]
    dataSourceName <- strsplit(dataSourceName,".", fixed = TRUE) 
    dataSourceName <- dataSourceName[[1]]
    dataSourceName <- dataSourceName[1]
    message("No dataSourceName provided, So I created one: ",dataSourceName)
  }
  
  url <- paste0("http://spatial.virtualearth.net/REST/v1/Dataflows/LoadDataSource?dataSourceName=",
                dataSourceName,
                "&loadOperation=",
                loadOperation,
                "&input=",
                input,
                "&key=",
                master_key)
  res <- httr::POST(url, body = httr::upload_file(path))
  content <- httr::content(res, type = "text")
  content <- jsonlite::fromJSON(content)
  if(content$statusCode != 201){
    warning("Failed")
  } else {
    message("Success")
  }
  return(content)
}

bing_geocode <- function(path, query_key = Sys.getenv("Bing_query_key")){
  
  checkmate::assert_file_exists(path)
  
  url <- paste0("http://spatial.virtualearth.net/REST/v1/Dataflows/Geocode?input=csv&output=json&key=",
  query_key)
  
  res <- httr::POST(url, body = httr::upload_file(path))
  content <- httr::content(res, type = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(content)
  if(content$statusCode != 201){
    warning("Failed")
  } else {
    message("Success")
    jobID <- content$resourceSets$resources[[1]]
    return(jobID)
  }
  return(content)
  
}


bing_geocode_status <- function(jobID, query_key = Sys.getenv("Bing_query_key")){
  
  url <- paste0("http://spatial.virtualearth.net/REST/v1/Dataflows/Geocode/",
                jobID,
                "?output=json&key=",
                query_key)
  
  res <- httr::GET(url)
  content <- httr::content(res, type = "text", encoding = "UTF-8")
  content <- jsonlite::fromJSON(content)
  if(content$statusCode != 200){
    warning("Failed")
  } else {
    message("Success")
    return(content$resourceSets)
  }
  
  return(content)
}



bing_geocode_download <- function(jobID, query_key = Sys.getenv("Bing_query_key")){
  
  url <- paste0("https://spatial.virtualearth.net/REST/v1/dataflows/Geocode/",
                jobID,
                "/output/succeeded?key=",
                query_key)
  
  res <- httr::GET(url)
  content <- httr::content(res, type = "text", encoding = "UTF-8")
  # content <- substr(content, 34, nchar(content))
  # foo = read.table(text = content, sep = ",")
  
  content <- strsplit(content,"\n", fixed = TRUE)
  content <- content[[1]]
  content <- content[seq(2,length(content))]
  #foo <- substr(content, nchar(content), nchar(content))
  #content <- paste0(content,"\n")
  
  dir.create(file.path(tempdir(),"bing"))
  writeLines(content, file.path(tempdir(),"bing/geocodes.csv"))
  tab <- read.csv(file.path(tempdir(),"bing/geocodes.csv"))
  unlink(file.path(tempdir(),"bing"), recursive = TRUE)
  
  # tab <- read.table(textConnection(content), sep = ",", fileEncoding = "UTF-8")
  # tab <- read.table(textConnection(content[19]), sep = ",", )
  
  
  names(tab) <-  c("Id","AddressLine","AdminDistrict","PostalCode",
  "CountryRegion","Latitude","Longitude","EntityType",
  "MatchCodes","Confidence","SouthLatitude", "WestLongitude",
  "NorthLatitude", "EastLongitude", "StatusCode","FaultReason",
  "TraceId")
  
  return(tab)
}


bing_to_sf <- function(tab){
  tab <- sf::st_as_sf(tab, coords = c("Longitude","Latitude"), crs = 4326)
  tab$TraceId <- NULL
  return(tab)
}


build_url <- function(routerUrl, query) {
  secs <- unlist(query, use.names = TRUE)
  secs <- sapply(secs, utils::URLencode, reserved = TRUE)
  secs <- paste0(names(secs), "=", secs)
  secs <- paste(secs, collapse = "&")
  secs <- paste0(routerUrl, "?", secs)
  secs
}


bing_geocode_single <- function(countryRegion = "GB",
                               adminDistrict = NULL,
                               postalCode = NULL, 
                               locality = NULL, 
                               addressLine = NULL,
                               includeNeighborhood = 0, 
                               include = "ciso2", 
                               maxResults = 5,
                               cultureCode = "en-GB",
                               key = Sys.getenv("Bing_query_key")){
  

  url <- "http://dev.virtualearth.net/REST/v1/Locations"

  query <- list(
    countryRegion = countryRegion,
    adminDistrict = adminDistrict,
    postalCode = postalCode,
    locality = locality,
    addressLine = addressLine,
    includeNeighborhood = includeNeighborhood,
    include = include,
    maxResults = maxResults,
    key = key,
    c = cultureCode
  )
  
  
  
  url <- build_url(url, query)
  text <- curl::curl_fetch_memory(url)
  text <- rawToChar(text$content)
  
  asjson <- try(RcppSimdJson::fparse(text),
                silent = TRUE
  )
  
  if("try-error" %in% class(asjson)){
    message("Geocode failed: json parse failed")
    return(data.frame(addressLine = NA,
                      adminDistrict = NA,
                      adminDistrict2 = NA,
                      countryRegion = NA,
                      formattedAddress = NA,
                      locality = NA,
                      postalCode = NA,
                      countryRegionIso2 = NA ,
                      latitude = NA,
                      longitude = NA,
                      confidence = NA,
                      entityType = NA,
                      matchCodes = NA))
  }
  
  
  if(asjson$statusCode != 200){
    message("Geocode failed: ", asjson$statusDescription)
    return(data.frame(addressLine = NA,
                      adminDistrict = NA,
                      adminDistrict2 = NA,
                      countryRegion = NA,
                      formattedAddress = NA,
                      locality = NA,
                      postalCode = NA,
                      countryRegionIso2 = NA ,
                      latitude = NA,
                      longitude = NA,
                      confidence = NA,
                      entityType = NA,
                      matchCodes = NA))
    
  }
  
  asjson <- asjson$resourceSets
  
  if(length(asjson$resources) > 1){
    stop("Muliple resultss for ",addressLine)
  }
  
  res <- asjson$resources[[1]]
  
  if(is.null(res)){
    message("Geocode failed: no result returned")
    return(data.frame(addressLine = NA,
                      adminDistrict = NA,
                      adminDistrict2 = NA,
                      countryRegion = NA,
                      formattedAddress = NA,
                      locality = NA,
                      postalCode = NA,
                      countryRegionIso2 = NA ,
                      latitude = NA,
                      longitude = NA,
                      confidence = NA,
                      entityType = NA,
                      matchCodes = NA))
  }
  
  if(length(res$address) > 1){
    res_address <- lapply(res$address, as.data.frame)
    res_address <- dplyr::bind_rows(res_address)
  } else {
    res_address <- res$address[[1]]
    res_address <- as.data.frame(res_address)
  }
  
  if(length(res$point) > 1){
    res_point <- lapply(res$point, as.data.frame)
    res_point <- dplyr::bind_rows(res_point)
  } else {
    res_point <- res$point[[1]]
    res_point <- as.data.frame(res_point)
  }
  
  if(length(res_point$coordinates) > 0){
    res_address$latitude <- res_point$coordinates[seq(1,length(res_point$coordinates), 2)]
    res_address$longitude <- res_point$coordinates[seq(2,length(res_point$coordinates), 2)]
  } else {
    res_address$latitude <- NA
    res_address$longitude <- NA
  }
  
  res_address$confidence <- res$confidence
  res_address$entityType <- res$entityType
  res_address$matchCodes <- res$matchCodes
  
  return(res_address)
  
  
}

bing_geocode_single_try <- function(countryRegion = "GB",
                                    adminDistrict = NULL,
                                    postalCode = NULL, 
                                    locality = NULL, 
                                    addressLine = NULL,
                                    includeNeighborhood = 0, 
                                    include = "ciso2", 
                                    maxResults = 5,
                                    cultureCode = "en-GB",
                                    key = Sys.getenv("Bing_query_key")){
  
  r <- try(bing_geocode_single(countryRegion = countryRegion,
                                 adminDistrict = adminDistrict,
                                 postalCode = postalCode, 
                                 locality = locality, 
                                 addressLine = addressLine,
                                 includeNeighborhood = includeNeighborhood, 
                                 include = include, 
                                 maxResults = maxResults,
                                 cultureCode = cultureCode,
                                 key = key), silent = TRUE)
  
  if("try-error" %in% class(r)){
    message("\nFailed on ",addressLine)
    return(NULL)
  } else {
    return(r)
  }
  
  
}


bing_geocode_batch <- function(dat){
  
  vars <- c("countryRegion","adminDistrict","postalCode","locality",
            "addressLine","includeNeighborhood","include","maxResults","key")
  vars <- names(dat)[names(dat) %in% vars]
  
  if(length(vars) < 4){
    stop(" not enough valid columns")
  }
  dat$matchID <- as.character(seq_len(nrow(dat)))
  
  # r <- purrr::pmap_dfr(dat[,vars], bing_geocode_single)
  r <- map_df_progress(dat[,vars], bing_geocode_single_try, .id = "matchID")
  
  r2 <- dplyr::left_join(dat, r, by = "matchID")
  
  return(r2)
  
}

map_df_progress <- function(.x, .f, ..., .id = NULL) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = nrow(.x), force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::pmap_dfr(.x, f, ..., .id = .id)
}


# foo = bing_geocode_batch(dat[1:15,])
# 
# 
# dat = read.csv("data/for_geocoding/UK_freehold_pc_single_short_batch_1.csv", skip = 1)
# names(dat) = c(c("id","addressLine",
#                  "adminDistrict","postalCode",
#                  "countryRegion","Latitude",
#                  "Longitude","EntityType",
#                  "MatchCodes","Confidence",
#                  "BoundingBox.SouthLatitude","BoundingBox.WestLongitude",
#                  "BoundingBox.NorthLatitude","BoundingBox.EastLongitude",
#                  "StatusCode","FaultReason",                           
#                  "TraceId"))
# dat <- dat[,1:5]
# dat$countryRegion = "GB"
# dat$id <- as.character(dat$id)
