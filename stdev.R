# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Indicator: Standard Deviation (stdev)
# Student: Sameea Ahmed
# File: stdev.R
#
# sigma = sqrt( sum((x_i - mean(x))^2) / n )
# Implemented using only R's standard core functions (no libraries, and no
# built-in sd() function).
# =============================================================================

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)

  # Calculate the differences between the data points and the mean
  diff_values <- data - mean_value

  # Calculate the squared differences
  squared_diff <- diff_values^2

  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)

  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)

  return(standard_deviation)
}

# -----------------------------------------------------------------------
# Example usage (per assignment instructions). Runs only when this file
# is executed directly, not when it is source()'d by another script.
# -----------------------------------------------------------------------
if (sys.nframe() == 0) {
  data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  stdev_result <- stdev(data)
  print(stdev_result)
}
