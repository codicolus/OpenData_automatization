# Reproject all files into the same coordinate system

# libraries
library(sf)
#library(rjson)
library(tools)
library(geojsonsf)

###################################################################################################
# Section 0: Auxiliary Functions

# Function converts dataset to correct encoding
# takes and returns sf-object
convert_to_correct_encoding <- function(sf_object, type_of_data){
  no_sf <- as.data.frame(sf_object)
  
  # convert to correct encoding
  if(type_of_data == "kantone_lines"){
    Encoding(no_sf$ID1) <- "UTF-8"
  }
  
  if(type_of_data == "swissLakes"){
    Encoding(no_sf$ID1) <- "UTF-8"
  }
  
  if(type_of_data %in% c("flussdaten_updated", "flussdaten")){
    Encoding(no_sf$name) <- "UTF-8"
  }
  
  
  # if(any(type_of_data == c("lakes","kantone_lines"))){
  #   
  #   no_sf$ID1 <- iconv(no_sf$ID1, from="ISO-8859-1", to="UTF-8")
  #   
  # }
  
  return(st_as_sf(no_sf))
  
}

# Function to identify coordinate system from geometry-coordinates
# only for epsg: 21781, 2056, 4326
check_coordinate_system <- function(sf_object){
  proj_sf <- st_crs(sf_object)
  
  no_sf <- as.data.frame(sf_object)
  
  coords <- st_coordinates(no_sf[,"geometry"])
  
  if((max(coords[,1]) > 600000)&(max(coords[,2]) > 200000)){
    
    return(epsg[1])
    
  }else if((max(coords[,1] > 1600000))&(max(coords[,2]) > 1200000)){
    
    return(epsg[3])
    
  }else if((max(coords[,1] > 8))&(max(coords[,2] > 47))){
    
    return(epsg[4])
    
  }else{
    print("Failed to match potential coordinate system")
    return(0)
  }
}
###################################################################################################
# Section 1: SETTINGS

# TODO: check which projections are most practical to use in R
# possible destination crs:
# EPSG: 21781 (swiss projection), 3857 (pseudo-mercator), 2056 (LV95), 4326 (WGS84)
path <-  "data"
# Choose Destination CRS
which_crs <- "LV95"

# TODO: save river data in map_resources folder?

# CRS-names and EPSG-codes
possible_crs <- c("Swiss Projection", "Pseudo-Mercator", "LV95", "WGS84")
epsg <- c(21781, 3857, 2056, 4326)
index_crs <- which(possible_crs == which_crs)

# Set-up destination projection specifications
dest_crs <- possible_crs[index_crs]
dest_epsg <- epsg[index_crs]

# Reading map-resources files
map_resources <- list.files(paste(path, "map_resources", sep = "/"))

# Check if Map-Resources exist
if(length(map_resources) == 0){
  print("No Map-Resources available!")
  #quit(status = 99)
}

# Check if all filetypes are GeoJSON
if(!all(file_ext(map_resources) %in% c("geojson", "json"))){
  print("Not all files provided are either of type GeoJSON or JSON!")
  #quit(status = 99)
}

names <- rep(NA, length(map_resources))
for(i in 1:length(map_resources)){
  correct <- strsplit(map_resources[i], "[.]")[[1]][1]
  names[i] <- correct
}

# Read-in all map-resources
for(i in 1:length(map_resources)){
  assign(names[i], geojson_sf(paste(path, "map_resources", map_resources[i], sep = "/")))
}

# Correct Encoding if necessary (to UTF-8)
# TODO: change index name to either badewetter_index.geojson or index.geojson
# TODO: get river_data input!
for(name in names){
  if(name != "badewetter"){
    assign(name, convert_to_correct_encoding(get(name), name))
  }
}

# check if coordinate values (geometry) correspond to coord_string
# if not: adjust coordinate string
# then transform to common cordinate reference system
CRS <- sp::CRS

# get coordinates from geometry
detected_crs <- as.list(rep(NA, length(names)))
for(i in 1:length(names)){
  detected_crs[[i]] <- CRS(st_crs(check_coordinate_system(get(names[i])))$proj4string)
}

# assumed CRS
assumed_crs <- as.list(rep(NA, length(names)))
for(i in 1:length(names)){
  assumed_crs[[i]] <- CRS(st_crs(get(names[i]))$proj4string)
}

# Check if coordinate systems match
for(i in 1:length(names)){
  if(raster::compareCRS(detected_crs[[i]], assumed_crs[[i]])){
    print("NO MATCH!")
  }
}

# TODO: get coordinate strings of every sf_object st_crs()
# then compare to CRS("+init=epsg:xxxx")
# to compare CRS-strings
crs_data = st_crs(21781)
crs_fromsf = st_crs(badewetter)
raster::compareCRS(CRS(crs_data$proj4string), CRS(crs_fromsf$proj4string))



# if false:
st_set_crs(x, epsg_code)

# TODO:
# if all set: then transform to common CRS st_transform + st_set_crs(x, new_CRS)
# OR CONSIDER UPDATING ONLY VALUES ONCE THE FILES ARE CREATED?
# E.G. BADEWETTER INDEX REMAINS BUT VALUES ARE UPDATED BY READING AS SF + UPDATING COLUMNS?

# test <- readLines("data/map_resources/badewetter.geojson")
# test[991]
# Encoding(test[991]) <- "ISO-8859-1"
# 
# test <- readLines("data/map_resources/kantone_lines.geojson")
# test[7]
# Encoding(test[7]) <- "UTF-8"

# TODO: find useful check for origin coordinate system
# WGS = klar
# LV95 = klar
# LV95 = klar (+ 1 000 000 ?)



