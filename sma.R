# Simple Moving Average (SMA)

sma <- function(data, period) {
  
  if (!is.numeric(data)) {
    stop("data must be a numeric vector")
  }
  
  if (length(period) != 1 ||
      !is.numeric(period) ||
      is.na(period) ||
      period < 1 ||
      period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  sma_values <- numeric(length(data) - period + 1)
  
  for (i in seq_len(length(sma_values))) {
    current_window <- data[i:(i + period - 1)]
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}


# Test the SMA function

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

sma_result <- sma(data, period = 3)

print(sma_result)