# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Indicator: Exponential Moving Average (EMA)
# Student: Sameea Ahmed
# File: ema.R
#
# EMA(i) = (Price(i) - EMA(i-1)) * multiplier + EMA(i-1)
# multiplier = 2 / (period + 1)
# The first EMA value is seeded with the first data point.
# Implemented using only R's standard core functions (no libraries).
# =============================================================================

ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)

  # Initialize an empty array to store EMA values (same length as data)
  ema_values <- numeric(length(data))

  # Loop through the data array
  for (i in 1:length(data)) {
    # Calculate EMA for the first data point (seed value)
    if (i == 1) {
      ema_values[i] <- data[i]
    } else {
      # Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }

  return(ema_values)
}

# -----------------------------------------------------------------------
# Example usage (per assignment instructions). Runs only when this file
# is executed directly, not when it is source()'d by another script.
# -----------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  ema_result <- ema(data, period = 3)
  print(ema_result)
}
