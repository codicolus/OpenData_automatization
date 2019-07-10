# This script processes the raster-interpolation of the Badewetter-Index Schweiz (swiss beach-weather-index)
# The interpolation used is a simple inverse distance weighting algorithm (IDW)
# The beach-weather-index is calculated at all automated weather stations of the
# Federal Office of Climatology and Meteorology (MeteoSwiss)
# Raster-Interpolation of the Badewetter-Index Schweiz
# The application can be found at: https://badewetter-index-schweiz.opendata.iwi.unibe.ch/
# Created by: Christoph von Matt
# Date: 10.07.2019
# Licence: CC-BY-SA

#libraries
library(rjson)
library(leafletR)

###################################################################################################
# Section 0: Auxiliary Functions

# Print list
print_list <- function(vector_of_strings){
  for(i in 1:length(vector_of_strings)){
    print(paste(i, ": " , vector_of_strings[i], sep=""))
  }
}

# Returns index of a column
get_index <- function(name, colnames=col_names){
  if(length(grep(name, col_names)) > 0){
    return(grep(name, col_names))
  }
  
  return(NA)
}
# Returns indizes of search columns
get_indizes <- function(names, colnames=col_names){
  if(length(names) == 1){
    return(get_index(names))
  }else{
    ind_out <- rep(NA, length(names))
    
    for(i in 1:length(names)){
      ind_out[i] <- get_index(names[i])
    }
    
    not_found <- which(is.na(ind_out))
    if(length(not_found) >= 1){
      print("Only returning found columns!")
      print("The following columns were not found:")
      print_list(names[not_found])
    }
    
    return(ind_out[which(!is.na(ind_out))])
  }
}


###################################################################################################
# Section 1: SET-UP DATA
path <- "data"

# Read generated badewetter-index file
badeindex_data <- read.csv(paste(path, "index/badeindex.csv", sep = "/"), header = T, na.strings = "-")
col_names <- colnames(badeindex_data)

# subset columns for badewetter-index interpolation
indizes <- get_indizes(c("Latitude", "Longitude", "Index"))
index_subset <- badeindex_data[,indizes]

# Convert file to GeoJSON  
lat_lon <- get_indizes(c("Latitude", "Longitude"))
toGeoJSON(badeindex_data, "badewetter", path, lat.lon = lat_lon, overwrite = T)
