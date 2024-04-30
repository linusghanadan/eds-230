#' Compute combined performance metric for streamflow data
#'
#' @param m column of dataframe with streamflow model predicted daily flow
#' @param o column of dataframe with observed daily flow
#' @param month column in dataframe with month
#' @param wy column in dataframe with water year
#'
#' @return list of mean absolute error, correlation coefficient, and the combined metric
#' @export
#'
#' @examples


compute_monthly_metrics <- function(m, o, month, wy) {
  
  # bind the input vectors into a dataframe
  flow = cbind.data.frame(m, o, month, wy)
  
  # group by monthly flow and water year and find the sum of flow values
  flow_monthly <- flow %>% 
    group_by(month, wy) %>% 
    summarize(model = sum(m),
              observed = sum(o))
  
  # calculate absolute errors
  err <- abs(flow_monthly$observed - flow_monthly$model)
  
  # standardize on a scale from 0 - 1, flipping the ranking by subtracting from 1 so that lower values of absolute error are closer to 1 (better performance)
  err_normal <- 1 - ((err - min(err)) / (max(err) - min(err)))
  
  # take the mean
  mae <- mean(err_normal)
  
  # calculate correlation coefficient
  cor <- cor(m, o)
  
  # combine the two metrics by addition since they are both on a scale from 0-1, with 1 meaning better model performance for both
  combined <- mae + cor
  
  return(list(mae = mae, cor = cor, combined = combined))
}