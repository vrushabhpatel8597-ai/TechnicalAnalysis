# Standard Deviation (stdev)

stdev <- function(data) {
  
  if (!is.numeric(data) || length(data) == 0) {
    stop("data must be a non-empty numeric vector")
  }
  
  mean_value <- sum(data) / length(data)
  
  diff_values <- data - mean_value
  
  squared_diff <- diff_values * diff_values
  
  variance <- sum(squared_diff) / length(squared_diff)
  
  standard_deviation <- sqrt(variance)
  
  return(standard_deviation)
}


# Test the stdev function

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

stdev_result <- stdev(data)

print(stdev_result)