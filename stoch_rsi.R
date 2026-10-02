stoch_rsi <- function(data,
period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  # Calculate the StochRSI
  min_rsi <- min(rsi_values, na.rm = 
  TRUE)
  max_rsi <- max(rsi_values, na.rm = 
  TRUE)
  k_values <- (rsi_values -
  min_rsi) / (max_rsi - min_rsi)
  # Calculate the %K line
  k_line <- sma(k_values, k_period)
  # Calculate the %D line
  d_line <- sma(k_line , d_period)
  # Return the %K and %D lines
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  return(result)
}
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
stoch_rsi_result <- stoch_rsi(
  data,
  period = 5,
  k_period = 3,
  d_period = 3
)
print(stoch_rsi_result)