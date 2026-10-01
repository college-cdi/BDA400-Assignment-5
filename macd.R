macd <- function(data,
short_period, long_period,
signal_period) {
  # Calculate short-term EMA
  short_ema <- ema(data,
  short_period)
# Calculate long-term EMA
long_ema <- ema(data,
  long_period)
# Calculate MCAD line
macd_line <- short_ema - long_ema
# Calculate signal line
signal_line <- ema(macd_line, signal_period)
# Calculate histogram
histogram <- macd_line - signal_line
# Return MACD line, signal line, and histogram
result <- list(
MACD_Line = macd_line,
Signal_Line = signal_line,
Histogram = histogram
)
return(result)
}
data <- c(100, 102, 101, 104, 103, 105, 108)
macd_result <- macd(
  data,
  short_period = 3,
  long_period = 5,
  signal_period = 2
)
print(macd_result)