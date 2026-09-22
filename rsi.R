# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Indicator: Relative Strength Index (RSI)
# Student: Sameea Ahmed
# File: rsi.R
#
# RSI = 100 - 100 / (1 + RS), RS = avg_gain / avg_loss, smoothed using
# Wilder's smoothing method as specified in the assignment pseudocode.
# The first 'period' values are NA because there is not yet enough history
# to compute a smoothed average gain/loss.
# Implemented using only R's standard core functions (no libraries).
# =============================================================================

rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- diff(data)

  # Initialize two vectors to store the gains and losses (a day that is not
  # a gain contributes 0 to gains, and vice versa for losses)
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))

  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }

  # Calculate the average gains and average losses for the first 'period'
  # data points
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])

  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, length(data))

  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period

    rs <- avg_gain / avg_loss
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }

  return(rsi_values)
}

# -----------------------------------------------------------------------
# Example usage (per assignment instructions). Runs only when this file
# is executed directly, not when it is source()'d by another script.
# -----------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
  rsi_result <- rsi(data, period = 5)
  print(rsi_result)
}
