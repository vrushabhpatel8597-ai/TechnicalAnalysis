# Crossunder function

crossunder <- function(arr1, arr2) {
  
  if (!is.numeric(arr1) || !is.numeric(arr2)) {
    stop("Both arrays must be numeric vectors")
  }
  
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  if (length(arr1) == 0) {
    stop("The arrays cannot be empty")
  }
  
  crossunder_signals <- rep("False", length(arr1))
  
  crossunder_signals[1] <- "None"
  
  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      
      if (arr1[i] < arr2[i] &&
          arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }
  
  return(crossunder_signals)
}


# Test the crossunder function

arr1 <- c(3, 4, 5, 2, 1)
arr2 <- c(2, 2, 2, 2, 2)

crossunder_signals <- crossunder(arr1, arr2)

print(crossunder_signals)