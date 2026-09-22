# =============================================================================
# BDA400 Assignment 5 - Technical Analysis using R (Development Phase)
# Student: Sameea Ahmed
# File: run_examples.R
#
# This is NOT one of the 9 graded indicator files. It is a demonstration /
# self-test harness: it source()s each of the 9 implementation files and
# runs every function against the hypothetical example data given in the
# assignment instructions, plus a few sanity checks, so the correctness of
# each implementation can be verified at a glance. Run with:
#     Rscript run_examples.R
# =============================================================================

source("sma.R")
source("ema.R")
source("macd.R")
source("stdev.R")
source("linreg.R")
source("rsi.R")
source("stoch_rsi.R")
source("crossover.R")
source("crossunder.R")

cat("=====================================================\n")
cat("1) SMA - Simple Moving Average\n")
cat("=====================================================\n")
data1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
sma_result <- sma(data1, period = 3)
print(sma_result)
stopifnot(length(sma_result) == length(data1) - 3 + 1)
cat("Sanity check passed: output length =", length(sma_result),
    "= length(data) - period + 1\n\n")

cat("=====================================================\n")
cat("2) EMA - Exponential Moving Average\n")
cat("=====================================================\n")
ema_result <- ema(data1, period = 3)
print(ema_result)
stopifnot(length(ema_result) == length(data1))
stopifnot(ema_result[1] == data1[1])
cat("Sanity check passed: output length matches input, first value seeded with data[1]\n\n")

cat("=====================================================\n")
cat("3) MACD - Moving Average Convergence Divergence\n")
cat("=====================================================\n")
data3 <- c(100, 105, 110, 115, 120, 125, 130)
macd_result <- macd(data3, short_period = 3, long_period = 5, signal_period = 2)
print(macd_result)
stopifnot(all(c("macd_line", "signal_line", "histogram") %in% names(macd_result)))
stopifnot(length(macd_result$macd_line) == length(data3))
cat("Sanity check passed: result is a list with macd_line/signal_line/histogram, each length", length(data3), "\n\n")

cat("=====================================================\n")
cat("4) Standard Deviation (stdev)\n")
cat("=====================================================\n")
stdev_result <- stdev(data1)
print(stdev_result)
stopifnot(abs(stdev_result - sd(data1) * sqrt((length(data1) - 1) / length(data1))) < 1e-9)
cat("Sanity check passed: matches sqrt(population variance) computed independently via R's sd()\n\n")

cat("=====================================================\n")
cat("5) Linear Regression (linreg)\n")
cat("=====================================================\n")
linreg_result <- linreg(data1, regressionLength = 5, regressionOffset = 0)
print(linreg_result)
# Independent cross-check using R's own lm() on the same windowed subset
n <- length(data1)
start_index <- max(1, n - 5 + 0)
end_index <- min(n, n - 0)
subset_check <- data1[start_index:end_index]
idx_check <- 1:length(subset_check)
lm_check <- lm(subset_check ~ idx_check)
cat("Cross-check with R's lm(): slope =", round(coef(lm_check)[2], 6),
    " intercept =", round(coef(lm_check)[1], 6), "\n")
stopifnot(abs(linreg_result$slope - coef(lm_check)[2]) < 1e-9)
stopifnot(abs(linreg_result$intercept - coef(lm_check)[1]) < 1e-9)
cat("Sanity check passed: slope/intercept match R's built-in lm() to within 1e-9\n\n")

cat("=====================================================\n")
cat("6) RSI - Relative Strength Index\n")
cat("=====================================================\n")
data6 <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
rsi_result <- rsi(data6, period = 5)
print(rsi_result)
valid_rsi <- rsi_result[!is.na(rsi_result)]
stopifnot(all(valid_rsi >= 0 & valid_rsi <= 100))
stopifnot(all(is.na(rsi_result[1:5])))
cat("Sanity check passed: first 5 values are NA (insufficient history), all computed values are within [0, 100]\n\n")

cat("=====================================================\n")
cat("7) Stochastic RSI (StochRSI)\n")
cat("=====================================================\n")
stoch_rsi_result <- stoch_rsi(data6, period = 5, k_period = 3, d_period = 3)
print(stoch_rsi_result)
valid_k <- stoch_rsi_result$k_line[!is.na(stoch_rsi_result$k_line)]
if (length(valid_k) > 0) {
  stopifnot(all(valid_k >= -1e-9 & valid_k <= 1 + 1e-9))
  cat("Sanity check passed:", length(valid_k), "valid %K values are all within [0, 1]\n\n")
} else {
  cat("Note: no fully-valid %K values for this short example series (expected with such a short series)\n\n")
}

cat("=====================================================\n")
cat("8) Crossover function\n")
cat("=====================================================\n")
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
crossover_signals <- crossover(arr1, arr2)
print(crossover_signals)
stopifnot(is.logical(crossover_signals))
stopifnot(length(crossover_signals) == length(arr1))
stopifnot(crossover_signals[1] == FALSE)
cat("Sanity check passed: logical vector, length", length(crossover_signals),
    ", TRUE at position(s):", paste(which(crossover_signals), collapse = ", "), "\n\n")

cat("=====================================================\n")
cat("9) Crossunder function\n")
cat("=====================================================\n")
crossunder_signals <- crossunder(arr1, arr2)
print(crossunder_signals)
stopifnot(is.logical(crossunder_signals))
stopifnot(length(crossunder_signals) == length(arr1))
stopifnot(crossunder_signals[1] == FALSE)
cat("Sanity check passed: logical vector, length", length(crossunder_signals),
    ", TRUE at position(s):", paste(which(crossunder_signals), collapse = ", "), "\n\n")

cat("=====================================================\n")
cat("ALL 9 INDICATORS RAN SUCCESSFULLY WITH NO ERRORS\n")
cat("=====================================================\n")
