# This script is for the preprocessing of the measurements of the automated weather stations of the
# Federal Office of Climatology and Meteorology (MeteoSwiss)
# Preprocessing for the swiss beach-weather index (Badewetter-Index Schweiz)
# The application can be found at: https://badewetter-index-schweiz.opendata.iwi.unibe.ch/
# Created by: Christoph von Matt
# Date: 08.07.2019
# Licence: CC-BY-SA

######## General Information ##########
# This script executes 2 preprocessing tasks:
#   1) get current measurements from the automatic weather station dataset of MeteoSwiss
#   2) (If necessary) preparation of MeteoSwiss-Station's metadata
#   3) Join created/existing metadata with stations data
#   4) Calculation of the Badewetter-Index
#
# The script outputs the following files:
#   1) "weather_data_joined.csv"
#   2) if not existing: "weather_metadata.csv"

# libraries
library(tidyverse)
library(data.table)
library(bit64)

# Pathes
# data folder (destination folder)
path <- "data"
# Measurements
measurements_path <- "https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/VQHA80.csv"
# meta_path <- "data/metadata_wetterstationen.txt"
meta_path <- "https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/info/VQHA80_de.txt"

# Create directory
dir.create(paste(path, "meteoswiss_data", sep="/"), showWarnings = F)

###################################################################################################
# Section 0: Auxiliary Functions

# gets metadata_starting column
get_start_line <- function(lines){
  # colnames
  col_names <- c("stn", "Name", "Länge/Breite", "KM-Koordinaten", "Höhe")
  
  # Get all indizes which match
  indizes_per_colname <- list()
  
  for (i in 1:length(col_names)){
    indizes_per_colname[[i]] <- grep(col_names[i], lines)
  }
  
  # Return the indizes which all have in common
  return(Reduce(intersect, indizes_per_colname))
}

# Function returns the line where the metadata-table ends
get_end_line <- function(lines, start_line, lines_expected){
  
  # empty lines used to find break after table
  empty_lines <- which(lines == "")
  
  # expected end
  exp_end <- start_line + lines_expected
  
  # get index of most probable end-line
  # gets the first index which is greated/equal to expected end
  end_line <- empty_lines[min(which(empty_lines >= exp_end))]
  
  return(end_line - 1)
}

# Prepare Meta-Data such that it can be processed
# subsets lines and cleans empty lines in-between (if there are any)
get_cleaned_subset <- function(lines, start_line, end_line){
  
  # subset data
  line_subset <- lines[start_line:end_line]
  
  # check if there are still empty lines
  if(length(which(line_subset == "")) != 0){
    empty_lines = which(line_subset == "")
    
    # filter empty lines out
    line_subset[-empty_lines]
  }
  
  return(line_subset)
}

# read in temporary meta-data
read_meta_correctly <- function(path){
  
  #define fix-width lengths
  #define the length of each fixed-width column
  lengths <- c(
    str_length("stn                                       "),
    str_length("Name                         "),
    str_length("Länge/Breite                              "),
    str_length("KM-Koordinaten                            "),
    str_length("Höhe")
  )
  
  # Colnames within dataset (hard-coded = must be known in advance!)
  col_names <- c("Station", "Name", "Länge/Breite", "Koordinaten", "Höhe")
  
  # read-in metadata
  data <- read_fwf(path, col_positions = fwf_widths(lengths, col_names = col_names),
                   trim_ws = T, skip = 2, locale = locale(encoding = "ISO-8859-1"))
  
  return(data)
}


# function for preparing latitude longitude values
# Latitude
get_lon <- function(coords, splitchar){
  return(as.numeric(str_split(coords, splitchar)[[1]][1]))
}
# Longitude
get_lat <- function(coords, splitchar){
  return(as.numeric(str_split(coords, splitchar)[[1]][2]))
}

get_coords_list <- function(coords, splitchar){
  
  nr_pairs <- dim(coords)[1]
  
  # Prepare for transforming coordinates
  lonlat <- matrix(NA, nrow = nr_pairs, ncol = 2)
  for(i in 1:nr_pairs){
    lonlat[i,1] <- get_lon(coords[i,1], splitchar)
    lonlat[i,2] <- get_lat(coords[i,1], splitchar)
  }
  
  return(lonlat)
}

# Function which checks for summertime / wintertime correction
# solutions found under stackoverflow
# https://stackoverflow.com/questions/8879864/how-to-find-summertime-adjustment-for-a-given-date-and-timezone-in-r
# TODO: handling times when clocks are set to winter / summer-times? Problem at 00:00 / 02:00 ?
difference_to_UTC <- function(){
  # timezone + current time
  tz = Sys.timezone()
  current_time = Sys.time()
  
  # UTC-time
  utc <- as.POSIXct(format(current_time,  tz = 'UTC'),  tz = tz)
  
  
  return(round(as.numeric(current_time - utc)))
}

# functions to split timestring
get_year <-  function(timestring){
  return(substr(timestring, 0, 4))
}
get_month <- function(timestring){
  return(substr(timestring, 5, 6))
}
get_day <- function(timestring){
  return(substr(timestring, 7, 8))
}
get_hour <- function(timestring){
  return(substr(timestring, 9, 10))
}
get_minutes <- function(timestring){
  return(substr(timestring, 11, 12))
}
get_date <- function(timestring){
  return(substr(timestring, 0, 8))
}
get_time <- function(timestring){
  return(substr(timestring, 9, 12))
}

# Function to adjust timestring for UTC
localtime <- function(timestring){
  #get UTC difference
  utc_diff <- difference_to_UTC()
  # adjust hour for difference
  hour_adjusted <- as.character(as.numeric(get_hour(timestring)) + utc_diff)
  
  if(nchar(hour_adjusted) == 1){
    hour_adjusted <- paste("0", hour_adjusted, sep = "")
  }
  
  # Prepare date + adjusted time for POSIXct-format
  date <- str_c(get_year(timestring), get_month(timestring), get_day(timestring), sep = "-")
  time_adjusted <- str_c(hour_adjusted, get_minutes(timestring), "00", sep = ":")
  
  return(paste(date, time_adjusted, sep = " "))
}

###################################################################################################
# Section 1: Fetch newest measurements of the automated weather stations of MeteoSwiss + prepare for join

# 1.1 Read and Process newest Meteo-Station data
# TODO: correct for UTC-time! (also dependent on current date!) e.g. use today() then determine whether summer or winter
# time to use

(ms_data <- as.data.frame(fread(measurements_path, na.strings = "-")))
cols_orig <- colnames(ms_data)

cols_new <- c("Station", "Time", "Temperatur (°C)", "Niederschlag (mm)", "Sonnenschein (min)", 
              "Globalstrahlung (W/m^2)", "Luftfeuchtigkeit (%)", "Taupunkt (°C)", "Windrichtung (°)", 
              "Windgeschwindigkeit (km/h)", "Böenspitze (km/h)", "Luftdruck auf Stationshöhe (QFE, hPa)", 
              "Luftdruck auf Meeresniveau (QFF, hPa)", "Luftdruck reduziert auf Meereshöhe mit Standard-Atmosphäre (QNH, hPa)",
              "Geopotential 850hPa (gpm)", "Geopotential 700hPa (gpm)", "Windrichtung vekt (°)",
              "Windgeschw. Turm (km/h)", "Böenspitze Turm (km/h)", "Lufttemperatur Instr 1 (°C)",
              "RH Turm (%)", "Taupunkt Turm (°C)")

# Define shortcut variable names through new Column names
colnames(ms_data) <- cols_new
# Subset required for Badewetter-Index
subset_cols <- cols_new[c(1:7, 10)]
badewetter_subset <- ms_data[, subset_cols]

# Correct for UTC-time (summer: UTC+2, winter: UTC+1)
time_adjusted <- localtime(badewetter_subset[1, "Time"])
time_adjusted <- rep(time_adjusted, dim(badewetter_subset)[1])
badewetter_subset[, "Time"] <- time_adjusted

rm("cols_new", "cols_orig")
###################################################################################################
# Section 2: Process Metadata

# FETCH METADATA + PROCESS (if necessary)
# Check if file already exists
if(!file.exists(paste(path, "meteoswiss_data/weather_metadata.csv", sep = "/"))){
  
  print("Metadata-File fetching and processing initialised...")
  
  lines <- readLines(meta_path)
  
  # Get required indizes
  start_line <- get_start_line(lines)
  expected_lines <- dim(ms_data)[1]
  end_line <- get_end_line(lines, start_line, expected_lines)
  
  # Get cleaned subset of lines
  lines <- get_cleaned_subset(lines, start_line, end_line)
  
  # Temporary file-storage
  write(lines, paste(path, "meteoswiss_data/temporary_meta.txt", sep = "/"))
  rm("lines", "start_line", "expected_lines", "end_line")
  
  # Read-in temporary-file
  meta_data <- read_meta_correctly(paste(path, "meteoswiss_data/temporary_meta.txt", sep = "/"))
  
  # Longitude Latitude Preparation
  lonlat <- get_coords_list(meta_data[,"Koordinaten"], "/")
  
  # Tidy data
  meta_data <- meta_data %>% select(Station, Name, Koordinaten, Höhe) %>%
    mutate(Longitude = lonlat[,1],
           Latitude = lonlat[,2]) %>%
      select(-Koordinaten)
  
  # write processed meta-data
  write.csv(meta_data, paste(path, "meteoswiss_data/weather_metadata.csv", sep = "/"), row.names = F, fileEncoding = "ISO-8859-1")
  
  # remove temporary file
  file.remove(paste(path, "meteoswiss_data/temporary_meta.txt", sep = "/"))
  
  rm("lonlat")
  
}else{
  meta_data <- read_csv(paste(path, "meteoswiss_data/weather_metadata.csv", sep = "/"), locale = locale(encoding = "ISO-8859-1"))
}


###################################################################################################
# Section 3: Join Measurement-Data + Metadata

# Join data
joined <- right_join(meta_data, badewetter_subset, by="Station")

# convert to data-frame (columns not tibble compatible)
joined <- as.data.frame(joined)

# Write final table as CSV
write.csv(joined, paste(path, "meteoswiss_data/weatherdata_joined.csv", sep = "/"), row.names = F, na = "-", fileEncoding = "ISO-8859-1")

rm(list=ls())
