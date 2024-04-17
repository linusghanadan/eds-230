#' Profit from Harvesting Almonds
#' 
#' Calculate almond profit anomaly based on almond farm acreage, price per ton of almonds, required labor hours for harvesting a ton of almonds, and the equation for predicted tons per acre almond yield put forth in Lobell et al. (2006), which uses minimum temperature during February and total precipitation in January
#' 
#' @param typical_yield typical expected almond yield for a normal season, in tons per acre
#' 
#' @param farm_acres area (in acres) of almond farm
#'
#' @param price market price (in USD) for a ton of almonds
#'
#' @param required_labor_hours number of worker-hours required for a ton of almonds to be harvested
#'
#' @param hourly_wage hourly wage for workers who will harvest almonds
#' 
#' @param climate_txt a .txt file with time series climate data. Must have columns named "day", "month", "year", "wy" (water year), "tmax_c" (maximum temperature degrees Celsius), "tmin_c" (minimum temperature degrees Celsius), and "precip" (mm) 
#'
#' @return a value for profit based on typical almond yield, almond farm acreage, price per ton of almonds, required labor hours for harvesting a ton of almonds, and predicted tons per acre almond yield anomaly

almond_profit <- function(typical_yield = 0.5, farm_acres = 50, price = 100, hourly_wage = 30, climate_txt = "data/clim.txt") {

  # attach tidyverse for data wrangling
  require(tidyverse)
  
  # convert time series climate data text file to a dataframe
  climate_df <- read.csv(climate_txt, sep = " ")
  
  # return a vector of the minimum temperature recorded during each February in the climate dataframe
  temp_vec <- climate_df %>% 
    group_by(month, year) %>% # group by month and year 
    summarise(min = min(tmin_c), na.rm = TRUE) %>% # return a dataframe of the minimum temperature recorded in each month and year
    filter(month == 2) %>% # filter for observations in February
    pull(min) # pull the minimum temperature during each February as a vector
  
  # return a vector of the sum of precipitation recorded during each January in the climate dataframe
  precip_vec <- climate_df %>% 
    group_by(month, year) %>% # group by month and year
    summarise(sum = sum(precip)) %>% # return a dataframe of the total precipitation in each month and year
    filter(month == 1) %>% # filter for observations in January
    pull(sum) # pull the precipitation total during each January as a vector
  
  # calculate mean almond yield over the range of our vectors
  yield_anomaly <- (-0.015 * mean(temp_vec)) - (0.0046 * mean(temp_vec)^2) - (0.07 * mean(precip_vec)) + (0.0043 * mean(precip_vec)^2) + 0.28
  
  # calculate predicted qty for specific farm based on acreage, typical yield, and yield anomaly
  predicted_farm_qty <- (typical_yield + yield_anomaly) * farm_acres
  
  # calculate total required labor hours for specific farm based on predicted quantity of almonds
  # assumption that a worker can harvest 20 almonds in an hour
  required_labor_hours <- predicted_farm_qty * 0.05
  
  # calculate profit based on price, predicted farm quanity, and labor wage
  profit <- (price * predicted_farm_qty) - (hourly_wage * required_labor_hours)
  
  # return the dataframe
  return(profit)
}

