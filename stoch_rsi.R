# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Indicator: Stochastic RSI (StochRSI)
# Student: Sameea Ahmed
# File: stoch_rsi.R
#
# StochRSI normalizes RSI to a 0-1 range using the min/max RSI, then smooths
# it into a %K line and a %D line (a further SMA of %K), per the assignment
# pseudocode. Reuses rsi() and sma() from rsi.R and sma.R - no libraries.
# =============================================================================

# stoch_rsi() depends on rsi() and sma() - source them if not already loaded
if (!exists("rsi")) {
  source("rsi.R")
}
if (!exists("sma")) {
  source("sma.R")
}

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)

  # Calculate the StochRSI
  # (na.rm = TRUE because the first 'period' RSI values are NA - there is
  # not yet enough history to compute them - and should not be treated as
  # the series minimum/maximum)
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)

  # Calculate the %K line (StochRSI) - a further smoothing of k_values
  k_line <- sma(k_values, k_period)

  # Calculate the %D line (3-day, or d_period, simple moving average of %K)
  d_line <- sma(k_line, d_period)

  # Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )

  return(result)
}

# -----------------------------------------------------------------------
# Example usage (per assignment instructions). Runs only when this file
# is executed directly, not when it is source()'d by another script.
# -----------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
  stoch_rsi_result <- stoch_rsi(data, period = 5, k_period = 3, d_period = 3)
  print(stoch_rsi_result)
}
