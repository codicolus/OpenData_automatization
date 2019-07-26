# This script calculates the badewetter-index using measurement data of the automated weather stations of the
# Federal Office of Climatology and Meteorology (MeteoSwiss)
# Calculation of the Badewetter-Index Schweiz (swiss beach-weather index)
# The application can be found at: https://badewetter-index-schweiz.opendata.iwi.unibe.ch/
# Created by: Christoph von Matt
# Date: 08.07.2019
# Licence: CC-BY-SA

# TODO: If Badewetter-Index cannot be calculated at a specific station: interpolate between 3 nearest stations?

######## General Information ##########
# This script executes 1 tasks:
#   1) Calculation of the Badewetter-Index Schweiz (Swiss beach-weather index)
#
# The script outputs the following files:
#   1) "badewetter_index.csv"

# libraries / functions
source("index_auxiliary.R")
library(leafletR)

###################################################################################################
# Section 0: Auxiliary Functions

# to add
add_Name_CRS <- function(filepath, name, epsg){
  # Read-in file
  file <- as.matrix(readLines(filepath))
  
  # Determine at which position the FeatureCollection-statement occurs
  at_which <- grep("*FeatureCollection*", file)
  
  # prepare new file with 2 added lines: name + CRS
  new_file <- matrix(NA, nrow = dim(file)[1]+2, ncol = 1)
  
  # Prepare name + crs strings
  name_to_write <- paste('  "name": ', paste("\"", name, "\"", sep = ""), ",", sep = "")
  crs_string <- paste(paste('  "crs": {"type": "EPSG", "properties": {"code": ', as.character(epsg), sep = ""), "}},", 
                 sep = "")
  
  # Insert new lines at correct positions
  # TODO: correct file assignment
  new_file[1:at_which,] <- file[1:at_which]
  new_file[at_which+1] <- name_to_write
  new_file[at_which+2] <- crs_string
  new_file[(at_which+3):length(new_file)] <- file[(at_which+1):length(file)]
  #new_file <- new_file[1:(length(new_file)-2)]
  #writeLines(new_file, con=filepath)
  #write(new_file, file = filepath)
  return(new_file)
}

###################################################################################################
# Section 1: Calculation of the Badewetter-Index

# SETTINGS
# set-up values
path = "data"

# Setting Max-values + weights for calculation
# Temperature (degC)
temp_max <-  45
temp_wgt <-  0.4
# Precipitation (mm)
prec_wgt <- 0.2
# Sunshine Durance (min)
sun_max <- 10
sun_wgt <- 0.05
# Global Radiation (W/m^2)
glob_max <- 1000
glob_wgt <- 0.05
# RH / Relative Humidity (%)
feu_wgt <- 0.15
# Wind (km/h)
wind_max <- 25
wind_wgt <- 0.15

# Read-in data
joined_data <- read.csv(paste(path, "meteoswiss_data/weatherdata_joined.csv", sep = "/"), na.strings = "-", encoding = "ISO-8859-1", header = T, 
                        check.names = F)

# get dimensions r-rows, c-cols
dims <- dim(joined_data)
r <- dims[1]
c <- dims[2]

# Badewetter-Index Calculation
index <- rep(NA, r)

col_names <- colnames(joined_data)

# BADEWETTER-INDEX CALCULATION
for(i in 1:r){
  temp <- as.numeric(joined_data[i,grep("Temp+", col_names)])
  print(temp)
  prec <- as.numeric(joined_data[i,grep("Nieder+", col_names)])
  sun <- as.numeric(joined_data[i,grep("Sonnen+", col_names)])
  glob <- as.numeric(joined_data[i,grep("Global+", col_names)])
  feu <- as.numeric(joined_data[i,grep("Luftfeu+", col_names)])
  wind <- as.numeric(joined_data[i,grep("Windgesch+", col_names)])
  
  # Index is only calculated if two most important variables (temperature + precipitation) are available
  if(!(is.na(temp) || is.na(prec))){
    
    #index[i] <- 0.4*temp + 0.2*prec + 0.05*sun + 0.05*glob + 0.15*feu + 0.15*wind
    
    index[i] <- temp_cont(temp, temp_wgt, temp_max) + prec_cont(prec, prec_wgt) + sun_cont(sun, sun_wgt, sun_max) + 
      glob_cont(glob, glob_wgt, glob_max) + feu_cont(feu, feu_wgt) + wind_cont(wind, wind_wgt, wind_max)
  }
}

# Possible Index-Range with given specifications for max-variables + weights
#maxindex
maxi <- temp_cont(temp_max, temp_wgt, temp_max) + prec_cont(0, prec_wgt) + sun_cont(sun_max, sun_wgt, sun_max) + 
  glob_cont(glob_max, glob_wgt, glob_max) + feu_cont(90, feu_wgt) + wind_cont(0, wind_wgt, wind_max)
(maxi*100)
#minindex
mini <- temp_cont(-10, temp_wgt, temp_max) + prec_cont(1, prec_wgt) + sun_cont(0, sun_wgt, sun_max) + 
  glob_cont(0, glob_wgt, glob_max) + feu_cont(0, feu_wgt) + wind_cont(wind_max, wind_wgt, wind_max)
(mini*100)

# Standardization
index <- 1/maxi*index

# Scale Index up + add to data
index <- as.integer(index*100)
joined_data[,c+1] <- index
colnames(joined_data)[c+1] <- "Index"

# update colnames
col_names <- colnames(joined_data)

###################################################################################################
# Section 2: WRITING END-PRODUCT FILES FOR USE IN QGIS

# Create directory
dir.create(paste(path, "index", sep="/"), showWarnings = F)

#Write csv
write.csv(joined_data, paste(path, "index/badeindex.csv", sep = "/"), row.names = F, na = "-", fileEncoding = "ISO-8859-1")

# Create GeoJSON from station data
# Convert file to GeoJSON  
lat_lon <- c(grep("Lat+", col_names), grep("Lon+", col_names))
toGeoJSON(joined_data, "badewetter", path, lat.lon = lat_lon, overwrite = T)

# Create MAP_RESOURCES_DIRECTORY
dir.create(paste(path, "map_resources", sep="/"), showWarnings = F)

# Add name + CRS
correct_geojson <- add_Name_CRS(paste(path, "badewetter.geojson", sep = "/"), "badewetter", 21781)
# Write GeoJSON-Badewetter-File
write(correct_geojson, file = paste(path, "/map_resources/badewetter.geojson", sep = "/"))

# Write csvt-file with type of each column (required for reading table in QGIS)
if(!file.exists(paste(path, "index/badeindex.csvt", sep = "/"))){
  
  col_type <- c("\"String\", \"String\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\"")
  write(col_type, paste(path, "index/badeindex.csvt", sep = "/"), ncolumns = length(col_names))
  
}

rm(list=ls())

