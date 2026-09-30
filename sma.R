sma ,- function(data, period){
  # Check if the length of data is less than the specified period
  if (length(data) < period){
    stop("period cannot be grater then the length of data")
    }
  # Initialize a vector to store the SMA values
  sma_values <- rep(NA, length(data))
  # Calculate SMA for each window of period data points
  for (i in period:length(data)) {
    sma_values[i] <- sum(data[(i - period + 1):i]) / period
    }
  return(sma_values)
  }
#sample data
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# Calculate SMA with a period of 3
sma_result <- sma(data, period = 3)
print(sma_result)
