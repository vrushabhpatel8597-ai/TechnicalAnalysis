# Relative Strength Index (RSI)

rsi <- function(data, period) {
  
  if (!is.numeric(data) || length(data) == 0) {
    stop("data must be a non-empty numeric vector")
  }
  
  if (length(period) != 1 ||
      !is.numeric(period) ||
      is.na(period) ||
      period < 1 ||
      period != as.integer(period)) {
    stop("period must be a positive integer")
  }
  
  if (length(data) <= period) {
    stop("Data length must be greater than the period")
  }
  
  diff_values <- diff(data)
  
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  for (i in seq_along(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }
  
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  rsi_values <- rep(NA_real_, length(data))
  
  for (i in (period + 1):length(data)) {
    
    avg_gain <- (
      avg_gain * (period - 1) + gains[i - 1]
    ) / period
    
    avg_loss <- (
      avg_loss * (period - 1) + losses[i - 1]
    ) / period
    
    if (avg_loss == 0) {
      rsi_values[i] <- 100
    } else {
      rs <- avg_gain / avg_loss
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(rsi_values)
}


# Test the RSI function

data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

rsi_result <- rsi(data, period = 5)

print(rsi_result)
