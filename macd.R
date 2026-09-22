# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Indicator: Moving Average Convergence Divergence (MACD)
# Student: Sameea Ahmed
# File: macd.R
#
# MACD line   = EMA_short - EMA_long
# Signal line = EMA(MACD line, signal_period)
# Histogram   = MACD line - Signal line
#
# This function reuses the ema() function implemented in ema.R, per the
# assignment's own pseudocode (short_ema = ema(data, short_period), etc.).
# It does not use any external libraries.
# =============================================================================

# macd() depends on ema() - source it if not already loaded in this session
if (!exists("ema")) {
  source("ema.R")
}

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)

  # Calculate the MACD line
  macd_line <- short_ema - long_ema

  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)

  # Calculate the histogram (the difference between the MACD line and the
  # signal line)
  histogram <- macd_line - signal_line

  # Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )

  return(result)
}

# -----------------------------------------------------------------------
# Example usage (per assignment instructions). Runs only when this file
# is executed directly, not when it is source()'d by another script.
# -----------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(100, 105, 110, 115, 120, 125, 130)
  macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)
  print(macd_result)
}
