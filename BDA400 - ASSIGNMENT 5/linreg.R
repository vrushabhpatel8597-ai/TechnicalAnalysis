# Linear Regression (linreg)

linreg <- function(regressionSource,
                   regressionLength,
                   regressionOffset) {
  
  if (!is.numeric(regressionSource) ||
      length(regressionSource) == 0) {
    stop("regressionSource must be a non-empty numeric vector")
  }
  
  if (length(regressionLength) != 1 ||
      !is.numeric(regressionLength) ||
      is.na(regressionLength) ||
      regressionLength < 1 ||
      regressionLength != as.integer(regressionLength)) {
    stop("regressionLength must be a positive integer")
  }
  
  if (length(regressionOffset) != 1 ||
      !is.numeric(regressionOffset) ||
      is.na(regressionOffset) ||
      regressionOffset < 0 ||
      regressionOffset != as.integer(regressionOffset)) {
    stop("regressionOffset must be a non-negative integer")
  }
  
  n <- length(regressionSource)
  
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  source_subset <- regressionSource[start_index:end_index]
  
  index_values <- seq_len(length(source_subset))
  
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)
  
  mean_index <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)
  
  numerator <- sum(
    (index_values - mean_index) *
      (source_subset - mean_source)
  )
  
  denominator <- sum(
    (index_values - mean_index)^2
  )
  
  if (denominator == 0) {
    stop("The regression denominator cannot be zero")
  }
  
  slope <- numerator / denominator
  
  intercept <- mean_source - slope * mean_index
  
  predicted_values <- slope * index_values + intercept
  
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}


# Test the linreg function

regression_source <- c(10, 12, 14, 16, 18, 20, 22)

linreg_result <- linreg(
  regressionSource = regression_source,
  regressionLength = 5,
  regressionOffset = 0
)

print(linreg_result)
