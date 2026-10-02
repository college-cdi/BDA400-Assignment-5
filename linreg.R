linreg <-
function(regressionSource, regressionLength, regressionOffset){
  # Calculate the total number of elements
  n <- length(regressionSource)
  # Check if regreesionLength is greater than the number of elements
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements")
  }
  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffet must be less tahn regressionLength")
  }
  # Calculate the starting and ending index
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  # Extraxt the revelent portion of regressionSource
  source_subset <-
    regressionSource[start_index:end_index]
  # Calculate the index value for the regression points
  index_values <- 1:length(source_subset)
  # Calculate sums
  sum_index <- sum(index_values)
  sum_source <- sum(source_subset)
  # Calculate means
  mean_index <- sum_index / length(index_values)
  mean_source <- sum_source / length(source_subset)
  # Calculate numerator and denominator
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  # Calculate slope and intercept
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  # Calculate predicted values
  predicted_values <- slope * index_values + intercept
  # Return results
  results <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  return(results)
}
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
linreg_result <- linreg(
  data,
  regressionLength = 5,
  regressionOffset = 0)
print(linreg_result)