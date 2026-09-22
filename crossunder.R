# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Indicator: Crossunder function
# Student: Sameea Ahmed
# File: crossunder.R
#
# Crossunder[i] = TRUE if arr1[i] < arr2[i] AND arr1[i-1] >= arr2[i-1]
#               = FALSE otherwise
# (arr1 has just moved from at-or-above arr2 to below arr2 - a downtrend
# signal). Implemented using only R's standard core functions (no libraries).
#
# Note on design choice: as with crossover.R, this returns a logical
# (TRUE/FALSE) vector to match the assignment's formula and worked example
# ("Crossunder(arr1, arr2) = [False, False, ..., True, False, ..., False]"),
# rather than the "True"/"False" string labels shown in the pseudocode block,
# since the instructions note the pseudocode is only an informal guide.
# =============================================================================

crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }

  # Initialize a vector to store the crossunder signals
  crossunder_signals <- logical(length(arr1))
  crossunder_signals[1] <- FALSE  # no previous point to compare against

  # Check for crossunder signals at each data point
  for (i in 2:length(arr1)) {
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- TRUE
    } else {
      crossunder_signals[i] <- FALSE
    }
  }

  return(crossunder_signals)
}

# -----------------------------------------------------------------------
# Example usage (per assignment instructions). Runs only when this file
# is executed directly, not when it is source()'d by another script.
# -----------------------------------------------------------------------
if (sys.nframe() == 0) {
  arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
  arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
  crossunder_signals <- crossunder(arr1, arr2)
  print(crossunder_signals)
}
