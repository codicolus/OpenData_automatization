# auxiliary functions for Badewetter-App / beach-weather app preprocessing
# This script includes auxiliary functions used for the processing of the Badewetter-App Schweiz
# Created by: Christoph von Matt
# Date: 08.07.2019
# Licence: CC-BY-SA

# BEACH-WEATHER-RELATED AUXILIARY FUNCTIONS
############################################
# Functions to calculate the contribution of temperatur, precipitation, sunshine, globalstrahlung, feuchtigkeit, wind
# Temperature Contribution
temp_cont <- function(value, wgth, max){
  # if temperature is below zero then value set to zero as other factors are dominant
  # then the variable is "standardized" according to meaningful values
  # returned is the standardized value * weight
  #min = 0
  #max = 45
  
  if(value < 0){
    value = 0
  }
  
  std <- 1 / max * value
  
  return(std*wgth)
}

# Precipitation Contribution
prec_cont <- function(value, wgth){
  # Principle: contribution only when no rain!
  if(value > 0){
    return(0)
  }
  
  return(wgth)
}

# Sunshine Contribution
sun_cont <- function(value, wgth, max){
  # min = 0
  # max = 10
  
  std <- 1 / max * value
  
  return(std*wgth)
}

# Global Radiation Contribution
glob_cont <- function(value, wgth, max){
  #max = 1000
  
  std <- 1 / max * value
  
  return(std*wgth)
}

# Relative Humidity Contribution
feu_cont <- function(value, wgth){
  max = 100
  
  std <- 1 / max * value
  
  # for extremely high RH the weight is halved
  if(value > 90){
    wgth = wgth * 0.5
  }
  
  return(std * wgth)
}

# Wind Speed Contribution
wind_cont <- function(value, wgth, max){
  # max acceptable wind speed = 25
  # the higher the speed the less less it contributes to the index
  # max = 25
  
  if(value > max){
    value = max
  }
  
  std <- 1 / max * value
  
  return((1-std)*wgth)
}

