# Reproject all files into the same coordinate system

# libraries
library(sf)
library(raster)
library(rjson)
library(tools)

###################################################################################################
# Section 0: Auxiliary Functions



###################################################################################################
# Section 1: SETTINGS

# TODO: check which projections are most practical to use in R
# possible destination crs:
# EPSG: 21781 (swiss projection), 3857 (pseudo-mercator), 2056 (LV95), 4326 (WGS84)
path <-  "data"
# Choose Destination CRS
which_crs <- "LV95"

# CRS-names and EPSG-codes
possible_crs <- c("Swiss Projection", "Pseudo-Mercator", "LV95", "WGS84")
epsg <- c(21781, 3857, 2056, 4326)
index_crs <- which(possible_crs == which_crs)

# Set-up destination projection specifications
dest_crs <- possible_crs[index_crs]
dest_epsg <- epsg[index_crs]


# Check if Map-Resources exist
map_resources <- list.files(paste(path, "map_resources", sep = "/"))
if(length(map_resources) == 0){
  print("No Map-Resources available!")
  #quit(status = 99)
}

# Check if all filetypes are GeoJSON
if(!all(file_ext(map_resources) == "geojson")){
  print("Not all files provided are of type GeoJSON!")
  #quit(status = 99)
}













