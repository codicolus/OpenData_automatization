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
#
# The script outputs the following files:
#   1) "weather_data_joined.csv"
#   2) ....

# libraries
library(tidyverse)
library(data.table)
library(bit64)

###################################################################################################
# Section 1: Fetch newest measurements of the automated weather stations of MeteoSwiss + prepare for join

# 1.1 Read and Process newest Meteo-Station data
# TODO: correct for UTC-time! (also dependent on current date!) e.g. use today() then determine whether summer or winter
# time to use
url <- "https://data.geo.admin.ch/ch.meteoschweiz.messwerte-aktuell/VQHA80.csv"

(ms_data <- as.data.frame(fread(url, na.strings = "-")))
cols_orig <- colnames(ms_data)

cols_new <- c("Station", "Time", "Temperatur (°C)", "Niederschlag (mm)", "Sonnenschein (min)", 
              "Globalstrahlung (W/m^2)", "Luftfeuchtigkeit (%)", "Taupunkt (°C)", "Windrichtung (°)", 
              "Windgeschwindigkeit (km/h)", "Böenspitze (km/h)", "Luftdruck auf Stationshöhe (QFE, hPa)", 
              "Luftdruck auf Meeresniveau (QFF, hPa)", "Luftdruck reduziert auf Meereshöhe mit Standard-Atmosphäre (QNH, hPa)",
              "Geopotential 850hPa (gpm)", "Geopotential 700hPa (gpm)", "Windrichtung vekt (°)",
              "Windgeschw. Turm (km/h)", "Böenspitze Turm (km/h)", "Lufttemperatur Instr 1 (°C)",
              "RH Turm (%)", "Taupunkt Turm (°C)")