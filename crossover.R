# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Indicator: Crossover function
# Student: Sameea Ahmed
# File: crossover.R
#
# Crossover[i] = TRUE if arr1[i] > arr2[i] AND arr1[i-1] <= arr2[i-1]
#              = FALSE otherwise
# (arr1 has just moved from at-or-below arr2 to above arr2 - an uptrend
# signal). Implemented using only R's standard core functions (no libraries).
#
# Note on design choice: the assignment's formula and worked example
# ("Crossover(arr1, arr2) = [False, False, ..., True, False, ..., False]")
# specify a logical (TRUE/FALSE) output array, so that is what this function
# returns. The assignment's pseudocode block instead shows "Up"/"Down"/"None"
# string labels (which actually mixes in the crossunder condition as well),
# which conflicts with the stated formula/example - I followed the formula
# and worked example, since the instructions note the pseudocode is only an
# informal guide and "there are multiple ways to implement these functions".
# =============================================================================

crossover <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossover signals
  crossover_signals <- logical(length(arr1))
  crossover_signals[1] <- FALSE  # no previous point to compare against

  # Check for crossovers at each data point
  for (i in 2:length(arr1)) {
    if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
      crossover_signals[i] <- TRUE
    } else {
      crossover_signals[i] <- FALSE
    }
  }

  return(crossover_signals)
}

# -----------------------------------------------------------------------
# Example usage (per assignment instructions). Runs only when this file
# is executed directly, not when it is source()'d by another script.
# -----------------------------------------------------------------------
if (sys.nframe() == 0) {
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
  crossover_signals <- crossover(arr1, arr2)
  print(crossover_signals)
}
