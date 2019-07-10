# Reproject all files into the same coordinate system

# libraries
library(sf)
library(raster)
library(rjson)

###################################################################################################
# Section 0: Auxiliary Functions



###################################################################################################
# Section 1: SETTINGS

# TODO: check which projections are most practical to use in R
# possible destination crs:
# EPSG: 21781 (swiss projection), 3857 (pseudo-mercator), 2056 (LV95), 4326 (WGS84)
path <-  "data"
# Choose Destination CRS
which_crs <- "WGS84"

# CRS-names and EPSG-codes
possible_crs <- c("Swiss Projection", "Pseudo-Mercator", "LV95", "WGS84")
epsg <- c(21781, 3857, 2056, 4326)
index_crs <- which(possible_crs == which_crs)

# Set-up destination projection specifications
dest_crs <- possible_crs[index_crs]
dest_epsg <- epsg[index_crs]

# Set folder-names if they exist
#
if(file.exists(paste(path, "")))













# For each file check if coordinate reference is correct
