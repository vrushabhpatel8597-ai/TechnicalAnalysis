# Stochastic RSI (StochRSI)


# Internal SMA helper
sma_for_stoch <- function(data, period) {
  
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


# Internal RSI helper
rsi_for_stoch <- function(data, period) {
  
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


# Stochastic RSI function
stoch_rsi <- function(data, period, k_period, d_period) {
  
  rsi_values <- rsi_for_stoch(data, period)
  
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  if (length(valid_rsi) < k_period) {
    stop("There are not enough RSI values for the selected k_period")
  }
  
  min_rsi <- min(valid_rsi)
  max_rsi <- max(valid_rsi)
  
  if (max_rsi == min_rsi) {
    k_values <- rep(0, length(valid_rsi))
  } else {
    k_values <- (
      valid_rsi - min_rsi
    ) / (max_rsi - min_rsi)
  }
  
  k_line <- sma_for_stoch(k_values, k_period)
  
  if (length(k_line) < d_period) {
    stop("There are not enough %K values for the selected d_period")
  }
  
  d_line <- sma_for_stoch(k_line, d_period)
  
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}


# Test the StochRSI function

data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

stoch_rsi_result <- stoch_rsi(
  data,
  period = 5,
  k_period = 3,
  d_period = 3
)

print(stoch_rsi_result)
