# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Indicator: Simple Moving Average (SMA)
# Student: Sameea Ahmed
# File: sma.R
#
# SMA(period) at position i = mean(data[i : (i + period - 1)])
# Implemented using only R's standard core functions (no libraries).
# =============================================================================

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }

  # Initialize a vector to store the SMA values.
  # There are (length(data) - period + 1) possible windows of size 'period'.
  sma_values <- numeric(length(data) - period + 1)

  # Calculate SMA for each window of 'period' data points
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    # Store the mean value in the sma_values array
    sma_values[i] <- mean_value
  }

  return(sma_values)
}

# -----------------------------------------------------------------------
# Example usage (per assignment instructions). This block only runs when
# the file is executed directly (e.g. Rscript sma.R), not when it is
# source()'d by another script or by the grader's test harness.
# -----------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  sma_result <- sma(data, period = 3)
  print(sma_result)
}
