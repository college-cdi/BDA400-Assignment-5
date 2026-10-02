crossover <- function(arr1, arr2){
  # Check if both arrays have the same length
  if (length(arr1) != length(arr2)) {
    stop("both arrays should have the same length")
  }
  # Initialize an array to store crossover signals
  crossover_signals <- rep("None", length(arr1))
  # Check for crossover at each data point
  for (i in 2:length(arr1)) {
    if (arr1[i] >= arr2[i] &&
        arr1[i - 1] < arr2[i - 1]) {
      crossover_signals[i] <- "up"
  } else if (arr1[i] < arr2[i]
    && arr1[i - 1] >= arr2[i - 1]) {
    crossover_signals[i] <- "Down"
  } else {
    crossover_signals[i] <- "None"
  }
  }
  return(crossover_signals)
}
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
crossover_signals <-
  crossover(arr1, arr2)
print(crossover_signals)