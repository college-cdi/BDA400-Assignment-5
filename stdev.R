stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / 
    length(data)
  # Calculate the differences between data points and the mean
  diff_values <-
    numeric(length(data))
  for (i in 1:length(data)) {
    diff_values[i] <- data[i] -
      mean_value
  }
  # Calculate the squared differences
  squared_diff <-
    numeric(length(diff_values))
  for (i in 1:length(diff_values))
  {
    squared_diff[i] <-
      diff_values[i] * diff_values[i]
  }
  # Calculate the variance
  variance <- sum(squared_diff) / 
    length(squared_diff)
  # Calculate the standard deviation
  standard_deviation <- 
    sqrt(variance)
  return(standard_deviation)
}
# Sample data
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# Calculate standard deviation
stdev_result <- stdev(data)
print(stdev_result)