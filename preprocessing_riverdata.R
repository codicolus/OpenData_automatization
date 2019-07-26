# This script is for the preprocessing of river + lake measurements from the Federal Office of Environment
# Preprocessing for the swiss beach-weather index (Badewetter-Index Schweiz)
# The application can be found at: https://badewetter-index-schweiz.opendata.iwi.unibe.ch/
# Created by: Christoph von Matt
# Date: 08.07.2019
# Licence: CC-BY-SA

######## General Information ##########
# This script executes 2 preprocessing tasks:
#   1) Query of current river + lake measurement data (JSON-file) from the Federal Office of Environment (FOEN)
#   2) For each feature: Scraping the following parameters:
#     - time, last measured temperature, mean temperature (24h), max temperature (24h), height of station, catchment-size
#
# The script outputs the following files:
#   1) "flussdaten_updated.json"

# libraries
library(httr)
library(XML)
library(rjson)
library(stringr)
library(snow)

###################################################################################################
# Section 0: Auxiliary Functions
extract_webURL <- function(attribute_containing_URL){
  attribute2HTML <- htmlParse(attribute_containing_URL)
  
  # Returns the href-attribute of the given string
  return(xpathApply(attribute2HTML, "//a/@href")[[1]][1])
}

# Reads the BAFU-HTML-table of measurements of a station site
# Returns a table
read_station_HTMLtable <- function(url){
  
  # Opening web-page
  station_site <- GET(url)
  station_site <- htmlParse(station_site, encoding = "utf-8")
  station_site <- xmlRoot(station_site)
  
  # Return table
  return(readHTMLTable(station_site, which = 1, header = F))
  
}

# Scrapes current temperature data from river-station
scrape_time_temperature <- function(data_table){
  
  column <- grep("Temper+", unlist(data_table[1,]))
  
  # Time
  time = str_extract(data_table[1,column], "[0-9]+:[0-9]+")
  
  # Temperatures
  temperatures <- as.numeric(as.vector(data_table[2:4, column]))
  
  # Returns Time,  Letzer Messwert, Mittelwert über letzte 24h, Maximum letzte 24h
  return(list(time, temperatures))
}

# Extracts values from string + turns it into numeric
get_attributes_as_numeric <- function(string_with_value){
  return(as.numeric(strsplit(string_with_value, " ")[[1]][1]))
}


# Scrapes meta-data from riverstation
scrape_metadata <- function(data_table){
  
  row_height <- grep("Stationshöhe", data_table[,1])
  row_catchment <- grep("Grösse des Einzugsgebietes", data_table[,1])
  
  # output
  out <- rep(NA, 2)
  
  # Returns Stationshöhe, Grösse des Einzugsgebiets
  values <- as.vector(data_table[c(row_height, row_catchment), 2])
  
  for (i in 1:2){
    out[i] <-get_attributes_as_numeric(values[i])
  }
  return(out)
}

# Function which updates feature
# created for parallelization with lapply
update_feature <- function(feature){
  # get current properties
  ft_props <- feature$properties
  # Get Web-URL
  ft_url <- extract_webURL(ft_props$description)
  
  # Get Temperature Values: 1 = current, 2 = mean 24h, 3 = max 24h
  time_temperatures <- scrape_time_temperature(read_station_HTMLtable(ft_url))
  # Get Metadata: 1 = Stationshöhe (m.ü.M.), 2 = Einzugsgebiet (km^2)
  stat_meta <- scrape_metadata(read_station_HTMLtable(ft_url))
  
  # Add the additional elements to the feature properties
  # time
  ft_props$time <- time_temperatures[[1]]
  # Current Temperature
  ft_props$current <- time_temperatures[[2]][1]
  # Mittlere Temperature 24h
  ft_props$mittlereT24h <- time_temperatures[[2]][2]
  # Maximale Temperatur 24h
  ft_props$maxT24h <- time_temperatures[[2]][3]
  
  # Stationshöhe
  ft_props$statHeight <- stat_meta[1]
  # Einzugsgebiet
  ft_props$catchment <- stat_meta[2]
  
  
  # Update features
  feature$properties <- ft_props
  
  return(feature)
  
}


###################################################################################################
# Section 1: Fetching newest river- data
# TODO: include river-temperatures in the interpolation of the badewetter-index too?
# -->or just use the actual temperatures as additional conditon which the user could specify on its own
# --> such that then the bade-index is alterd such that only regions are displayed where also the river/lake temperatures
# are adequately for bathing

# TODO: create an executable function out of the whole script
# TODO: if running on Linux: coordination with BASH-Script preferable
# Outfile
path <- "data"

# url
file <- "http://data.geo.admin.ch/ch.bafu.hydroweb-messstationen_temperatur/ch.bafu.hydroweb-messstationen_temperatur_de.json"

# read-in JSON-format
rivers_json <- fromJSON(file=file, simplify = T) # TODO: really necessary to simplify?
river_features <- rivers_json$features


# Updated feature-list
updated_features <- list()

# Add Temperature and Metadata-Information for every feature in data-set
# PARALLELIZED
print("Feature data fetching initialised...")
print(paste("Total features being processed:", length(river_features)))

# CREATE CLUSTER WITH ALL AVAILABLE CORES
cl<-makeCluster(parallel::detectCores(),type="SOCK")

# Export all necessary functions
clusterExport(cl=cl, list("extract_webURL", "htmlParse", "xpathApply", "scrape_time_temperature",
                          "read_station_HTMLtable", "GET", "xmlRoot", "readHTMLTable", "str_extract",
                          "scrape_metadata", "get_attributes_as_numeric"))

# PARALLELIZATION + UPDATING FUNCTIONS
system.time(
  updated_features <- clusterApply(cl, river_features, update_feature)
)
# Stop Cluster
stopCluster(cl)
rm("cl")


# replace features with updated ones
rivers_json$features <- updated_features

# Convert update list to JSON
out_json <- toJSON(rivers_json)

# TODO: could also be other data format

# Create directory
dir.create(paste(path, "riverdata", sep="/"), showWarnings = F)

# write Updated JSON
write(out_json, file=paste(path, "riverdata/flussdaten_updated.json", sep = "/"))

rm(list=ls())
