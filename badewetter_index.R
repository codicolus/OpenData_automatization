# This script calculates the badewetter-index using measurement data of the automated weather stations of the
# Federal Office of Climatology and Meteorology (MeteoSwiss)
# Calculation of the Badewetter-Index Schweiz (swiss beach-weather index)
# The application can be found at: https://badewetter-index-schweiz.opendata.iwi.unibe.ch/
# Created by: Christoph von Matt
# Date: 08.07.2019
# Licence: CC-BY-SA

######## General Information ##########
# This script executes 1 tasks:
#   1) Calculation of the Badewetter-Index Schweiz (Swiss beach-weather index)
#
# The script outputs the following files:
#   1) "badewetter_index.csv"

# libraries / functions
source("index_auxiliary.R")

###################################################################################################
# Section 1: Calculation of the Badewetter-Index

# set-up values
path = "data"
# Settings
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
# TODO: Correct time to UTC+2 (summer) / UTC+1 (winter)
joined_data <- read.csv(paste(path, "weather_data_joined.csv", sep = "/"), na.strings = "-", encoding = "ISO-8859-1", header = T, 
                        check.names = T)

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
  
  # Index is currently only calculated if all required variables are available
  # TODO: could be changed as index will get lower when variables are missing
  # BUT ATTENTION: index should not be calculated when crucial variable like temperature or precipitation is missing
  # TODO: would have to be adjusted also in auxiliary functions --> condition for when value is NA
  if(!(is.na(temp) || is.na(prec) ||is.na(sun) ||is.na(glob) ||is.na(feu) ||is.na(wind))){
    
    #index[i] <- 0.4*temp + 0.2*prec + 0.05*sun + 0.05*glob + 0.15*feu + 0.15*wind
    
    index[i] <- temp_cont(temp, temp_wgt, temp_max) + prec_cont(prec, prec_wgt) + sun_cont(sun, sun_wgt, sun_max) + 
      glob_cont(glob, glob_wgt, glob_max) + feu_cont(feu, feu_wgt) + wind_cont(wind, wind_wgt, wind_max)
  }
}

# Possible Index-Range with given specifications for max-variables + weights
# TODO: could / should be standardized such that values are always in the range 0-100
#maxindex
maxi <- temp_cont(temp_max, temp_wgt, temp_max) + prec_cont(0, prec_wgt) + sun_cont(sun_max, sun_wgt, sun_max) + 
  glob_cont(glob_max, glob_wgt, glob_max) + feu_cont(90, feu_wgt) + wind_cont(0, wind_wgt, wind_max)
(maxi*100)
#minindex
mini <- temp_cont(-10, temp_wgt, temp_max) + prec_cont(1, prec_wgt) + sun_cont(0, sun_wgt, sun_max) + 
  glob_cont(0, glob_wgt, glob_max) + feu_cont(0, feu_wgt) + wind_cont(wind_max, wind_wgt, wind_max)
(mini*100)

# Standardization
# index <- 1/maxi*index

# Scale Index up + add to data
index <- as.integer(index*100)
joined_data[,c+1] <- index
colnames(joined_data)[c+1] <- "Index"

# update colnames
col_names <- colnames(joined_data)

###################################################################################################
# Section 2: WRITING END-PRODUCT FILES FOR USE IN QGIS
#Write csv
write.csv(joined_data, paste(path, "badeindex.csv", sep = "/"), row.names = F, na = "-", fileEncoding = "ISO-8859-1")

# defining what variable type each column is (required for reading in QGIS)
col_type <- c("\"String\", \"String\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\", \"Real\"")

write(col_type, paste(path, "badeindex.csvt", sep = "/"), ncolumns = length(cols))

