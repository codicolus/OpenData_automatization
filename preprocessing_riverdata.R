# This script is for the preprocessing of river + lake measurements from the Federal Office of Environment
# Preprocessing for the swiss beach-weather index (Badewetter-Index Schweiz)
# The application can be found at: https://badewetter-index-schweiz.opendata.iwi.unibe.ch/
# Created by: Christoph von Matt
# Date: 08.07.2019
# Licence: CC-BY-SA

# libraries
library(httr)
library(XML)
library(rjson)
library(stringr)

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
  station_site <- htmlParse(station_site, encoding = "UTF-8")
  station_site <- xmlRoot(station_site)
  
  # Return table
  return(readHTMLTable(station_site, which = 1, header = F))
  
}

# Scrapes current temperature data from river-station
scrape_temperature <- function(data_table){
  
  # Returns Letzer Messwert, Mittelwert über letzte 24h, Maximum letzte 24h
  return(as.numeric(as.vector(data_table[1:3, "V4"])))
}

# Extracts values from string + turns it into numeric
get_attributes_as_numeric <- function(string_with_value){
  return(as.numeric(strsplit(string_with_value, " ")[[1]][1]))
}


# Scrapes meta-data from riverstation
scrape_metadata <- function(data_table){
  
  # output
  out <- rep(NA, 2)
  
  # Returns Stationshöhe, Grösse des Einzugsgebiets
  values <- as.vector(data_table[6:7, "V2"])
  
  for (i in 1:2){
    out[i] <-get_attributes_as_numeric(values[i])
  }
  return(out)
}


###################################################################################################
# Section 1: Fetching newest river- data
# TODO: scrape last measurement value of each river (if it doesn't take too long)
# TODO: include river-temperatures in the interpolation of the badewetter-index too?
# -->or just use the actual temperatures as additional conditon which the user could specify on its own
# --> such that then the bade-index is alterd such that only regions are displayed where also the river/lake temperatures
# are adequately for bathing

# url
file <- "http://data.geo.admin.ch/ch.bafu.hydroweb-messstationen_temperatur/ch.bafu.hydroweb-messstationen_temperatur_de.json"

# read-in JSON-format
rivers_json <- fromJSON(file=file)
river_features <- rivers_json$features


# Updated feature-list
updated_features <- list()

# Add Temperature and Metadata-Information for every feature in data-set
# Loop over all features
for (i in 1:length(river_features)){
  
  print(i)
  
  # get current feature
  feature <- river_features[[i]]
  
  # get current properties
  ft_props <- feature$properties
  # Get Web-URL
  ft_url <- extract_webURL(ft_props$description)
  
  # Get Temperature Values: 1 = current, 2 = mean 24h, 3 = max 24h
  temperatures <- scrape_temperature(read_station_HTMLtable(ft_url))
  # Get Metadata: 1 = Stationshöhe (m.ü.M.), 2 = Einzugsgebiet (km^2)
  stat_meta <- scrape_metadata(read_station_HTMLtable(ft_url))
  
  # Add the additional elements to the feature properties
  # Current Temperature
  ft_props$current <- temperatures[1]
  # Mittlere Temperature 24h
  ft_props$mittlereT24h <- temperatures[2]
  # Maximale Temperatur 24h
  ft_props$maxT24h <- temperatures[3]
  
  # Stationshöhe
  ft_props$statHeight <- stat_meta[1]
  # Einzugsgebiet
  ft_props$catchment <- stat_meta[2]
  
  
  # Update features
  feature$properties <- ft_props
  
  # add updated feature to new feature-list
  updated_features[[i]] <- feature
  
}

# Convert update list to JSON
updated_features <- toJSON(updated_features)

# TODO: could also be other data format
# write Updated JSON
write(updated_features, file="data/flussdaten_updated.json")