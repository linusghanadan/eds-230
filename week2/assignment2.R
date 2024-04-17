#' Almond Yield Anomaly
#' 
#' Calculate almond yield anomaly based on the equation in Lobell et al. (2006), which uses minimum temperature during February and total precipitation in January
#'
#' @param climate_txt a .txt file with time series climate data. Must have columns named "day", "month", "year", "wy" (water year), "tmax_c" (maximum temperature degrees Celsius), "tmin_c" (minimum temperature degrees Celsius), and "precip" (mm) 
#'
#' @return a dataframe of the minimum, maximum, and mean almond yield anomalies based on the given data, measured in ton/acre

almond_yield_anomaly <- function(climate_txt) {
  
  # attach tidyverse for data wrangling
  library(tidyverse)
  
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
  
  # calculate almond yield anomaly over the range of our vectors
  almond_yield <- (-0.015 * temp_vec) - (0.0046 * temp_vec^2) - (0.07 * precip_vec) + (0.0043 * precip_vec^2) + 0.28
  
  # assign the minimum, maximum, and mean yield anomalies
  min_yield <- min(almond_yield)
  max_yield <- max(almond_yield)
  mean_yield <- mean(almond_yield)
  
  # bind the maximum, minimum, and mean yield anomaly in a dataframe
  almond_yield_df <- data.frame(
    crop = c("almond"),
    min_yield = c(min_yield),
    max_yield = c(max_yield),
    mean_yield = c(mean_yield)
  )
  
  # return the dataframe
  return(almond_yield_df)
}

almond_yield_anomaly("../data/clim.txt")


