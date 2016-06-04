
library(RCurl)
library(jsonlite)
library(plyr)
library(ggplot2)
library(ggmap)
mapsURL <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

#my_api <- "AIzaSyC2YlDWnRRyDalApkny42181XQPmUhuTAU"
#my_api <- "AIzaSyApghFW9gHFboZlstE4opcBwmVpmC_odjc"
my_api <- "AIzaSyCQi7vuokYHAqFz9VmL5lGyqDFPAKu1GtU"

geoCode <- function(address) {
  u <- mapsURL(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplifyVector = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, formatted_address))
  } else {
    return(c(NA,NA, NA))
  }
}


getLocations <- function(type,geo,keyword ="", 
                         key=my_api,radius = 2500) {
  root="https://maps.googleapis.com/maps/api/place/radarsearch/json?"
  if (keyword == "")
    keyword0 <- ""
  else
    keyword0 <- paste0("&keyword=",keyword)
  u <- paste0(root,"location=",geo[1],",",geo[2],"&radius=",radius,"&type=",type,keyword0,"&key=",key)
  jsonLink <- URLencode(u)
  jsonFile <- getURL(jsonLink)
  jsonTree <- fromJSON(jsonFile, simplifyVector = FALSE)
  if (jsonTree$status == "OK") {
    return(jsonTree)
  }
  else
    msg = paste("Error:",jsonTree$error_message)
    stop(msg)
}

parseLocations <- function(tree) {
  vec = c()
  for (i in 1:length(tree$results)) {
    vec[i] <-tree$results[[i]]$place_id
  }
  res <- analyzeLocations(vec)
  return(res)
}

analyzeLocations <- function(vec) {
  show(paste("total",length(vec)))
  lat = c()
  lng = c()
  rating = c()
  base="https://maps.googleapis.com/maps/api/place/details/json?placeid="
  key <- paste0("&key=",my_api)
  j <- 1
  for (i in 1:length(vec)) {
    u <- paste0(base,vec[i],key)
    jsonLink <- URLencode(u)
    #jsonFile <- getURL(jsonLink,crlf = TRUE)
    jsonTree <- fromJSON(jsonLink, simplifyVector = FALSE)
    if (jsonTree$status == "OK") {
      if (!is.null(jsonTree$result$rating)) {
        rating[j] <-jsonTree$result$rating
        lat[j] <- jsonTree$result$geometry$location$lat
        lng[j] <- jsonTree$result$geometry$location$lng
        j <- j+1
      }
    }
    else
      next
  }
 # show(priceLevel)
  show(rating)
  #vec <- c(priceLevel,rating)
  #return(c(rating,lat,lng))
  locationsTable = data.frame(rating = rating, lat = lat, lon = lng)
}

showLocationsOnMap <- function(geo,locationsInfo) {
  map <- get_map(location = c( lon =as.numeric(geo[2]), lat = as.numeric(geo[1])), maptype = "satellite", zoom = 14)
  ggmap(map) + geom_point(data = locationsInfo, aes(x=lon, y=lat, color=rating)) + scale_colour_gradientn(colours=rainbow(4))
}

lookFor <- function(type, location) {
  geo <- geoCode(location)
  locations <- getLocations(type,geo)
  locationsInfo <- parseLocations(locations)
  showLocationsOnMap(geo,locationsInfo)
}


lookFor("restaurant","new york city")

#ex1 <- tubeUrl("vvnqfcXKGec")
#j <- fromJSON(ex1)