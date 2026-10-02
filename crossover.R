# Crossover function

crossover <- function(arr1, arr2) {
  
  if (!is.numeric(arr1) || !is.numeric(arr2)) {
    stop("Both arrays must be numeric vectors")
  }
  
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  if (length(arr1) == 0) {
    stop("The arrays cannot be empty")
  }
  
  crossover_signals <- rep("None", length(arr1))
  
  if (length(arr1) > 1) {
    for (i in 2:length(arr1)) {
      
      if (arr1[i] > arr2[i] &&
          arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
        
      } else if (
        arr1[i] < arr2[i] &&
        arr1[i - 1] >= arr2[i - 1]
      ) {
        crossover_signals[i] <- "Down"
        
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }
  
  return(crossover_signals)
}


# Test the crossover function

arr1 <- c(1, 2, 3, 2, 1)
arr2 <- c(2, 2, 2, 2, 2)

crossover_signals <- crossover(arr1, arr2)

print(crossover_signals)