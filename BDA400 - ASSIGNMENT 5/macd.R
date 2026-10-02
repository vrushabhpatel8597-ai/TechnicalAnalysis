# Moving Average Convergence Divergence (MACD)

# Internal EMA calculation used by the MACD function
ema_for_macd <- function(data, period) {
  
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  
  ema_values[1] <- data[1]
  
  if (length(data) > 1) {
    for (i in 2:length(data)) {
      ema_values[i] <-
        (data[i] - ema_values[i - 1]) * multiplier +
        ema_values[i - 1]
    }
  }
  
  return(ema_values)
}


macd <- function(data, short_period, long_period, signal_period) {
  
  if (!is.numeric(data) || length(data) == 0) {
    stop("data must be a non-empty numeric vector")
  }
  
  periods <- c(short_period, long_period, signal_period)
  
  if (any(!is.numeric(periods)) ||
      any(is.na(periods)) ||
      any(periods < 1) ||
      any(periods != as.integer(periods))) {
    stop("All periods must be positive integers")
  }
  
  short_ema <- ema_for_macd(data, short_period)
  long_ema <- ema_for_macd(data, long_period)
  
  macd_line <- short_ema - long_ema
  
  signal_line <- ema_for_macd(macd_line, signal_period)
  
  histogram <- macd_line - signal_line
  
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}


# Test the MACD function

data <- c(100, 105, 110, 115, 120, 125, 130)

macd_result <- macd(
  data,
  short_period = 3,
  long_period = 5,
  signal_period = 2
)

print(macd_result)
