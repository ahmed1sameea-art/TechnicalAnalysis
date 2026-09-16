#==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 2: Technical Analysis using R, Preliminary Stage
# Student: Sameea Ahmed
#
# AI Assistance Declaration:
# This script's structure and comments were drafted with AI assistance
# (Claude). All statistical calculations are performed exclusively by
# built-in R / package functions (stats, TTR) at run time on live data
# pulled from Yahoo Finance via quantmod - no numeric results were
# computed, estimated, or supplied by the AI itself.
#==============================================================================

# ---- 1. Load required packages ----------------------------------------------
# Install once (uncomment if not already installed):
# install.packages(c("quantmod", "TTR"))

library(quantmod)   # for getSymbols() - imports stock data from Yahoo Finance
library(TTR)         # for SMA() and other technical trading rule functions

# ---- 2. load_stock_data() ----------------------------------------------------
# Reads the stock symbols listed in portfolio.txt (one symbol per line) and
# downloads historical price data for each symbol using quantmod::getSymbols().
# Returns a named list of xts/data frame objects, one per stock symbol.

load_stock_data <- function(portfolio_file = "portfolio.txt",
                             from_date = Sys.Date() - 365,
                             to_date = Sys.Date()) {

  if (!file.exists(portfolio_file)) {
    stop(paste("Portfolio file not found:", portfolio_file))
  }

  # Read stock symbols, one per line, ignoring blank lines/whitespace
  symbols <- readLines(portfolio_file, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != ""]

  cat("Loading data for", length(symbols), "symbols:", paste(symbols, collapse = ", "), "\n")

  stock_data_list <- list()

  for (sym in symbols) {
    cat("  -> Downloading", sym, "...\n")
    result <- tryCatch({
      # getSymbols with auto.assign = FALSE returns the xts object directly
      # instead of assigning it to a variable named after the symbol in .GlobalEnv
      getSymbols(sym, src = "yahoo", from = from_date, to = to_date, auto.assign = FALSE)
    }, error = function(e) {
      warning(paste("Failed to download", sym, "-", conditionMessage(e)))
      NULL
    })

    if (!is.null(result)) {
      # Store as a data frame with Date as an explicit column, keeping the
      # symbol name attached so downstream functions know which stock it is
      df <- data.frame(Date = zoo::index(result), zoo::coredata(result))
      colnames(df) <- gsub(paste0("^", sym, "\\."), "", colnames(df))
      attr(df, "symbol") <- sym
      stock_data_list[[sym]] <- df
    }
  }

  cat("Finished loading", length(stock_data_list), "of", length(symbols), "symbols.\n")
  return(stock_data_list)
}

# ---- 3. calculate_statistics() ------------------------------------------------
# Takes a single stock's data frame (as produced by load_stock_data) and
# computes basic descriptive statistics on the Closing price: a rolling
# moving average, mean, mode, median, and standard deviation.
# Returns a one-row data frame summarizing the stock.

# Helper: R has no built-in mode() function for statistical mode, so we
# define one that works on numeric price data (returns the most frequent
# value, rounded to 2 decimals to group near-identical closing prices).
get_mode <- function(x) {
  x <- round(x, 2)
  freq_table <- table(x)
  as.numeric(names(freq_table)[which.max(freq_table)])
}

calculate_statistics <- function(stock_df, ma_period = 20) {

  if (!"Close" %in% colnames(stock_df)) {
    stop("Input data frame must contain a 'Close' column.")
  }

  close_prices <- stock_df$Close
  symbol <- attr(stock_df, "symbol")
  if (is.null(symbol)) symbol <- "UNKNOWN"

  # Moving average via TTR::SMA (Simple Moving Average)
  moving_avg <- TTR::SMA(close_prices, n = ma_period)

  stats_summary <- data.frame(
    Symbol            = symbol,
    Observations      = length(close_prices),
    Mean_Close        = round(mean(close_prices, na.rm = TRUE), 2),
    Median_Close      = round(median(close_prices, na.rm = TRUE), 2),
    Mode_Close         = round(get_mode(close_prices), 2),
    SD_Close           = round(sd(close_prices, na.rm = TRUE), 2),
    Latest_MA20        = round(tail(moving_avg[!is.na(moving_avg)], 1), 2),
    Min_Close          = round(min(close_prices, na.rm = TRUE), 2),
    Max_Close          = round(max(close_prices, na.rm = TRUE), 2)
  )

  # Attach the full moving-average series as an attribute in case the
  # caller wants to plot it later, without cluttering the summary table
  attr(stats_summary, "moving_average_series") <- moving_avg

  return(stats_summary)
}

# ---- 4. Display utility functions --------------------------------------------
# Several ways of displaying the imported data / calculated statistics,
# reflecting the different ways stock data is commonly presented
# (e.g. Yahoo Finance-style summary tables and time series views).

# 4a. Print a compact summary of a stock's raw data frame (head/tail + str)
display_stock_summary <- function(stock_df) {
  symbol <- attr(stock_df, "symbol")
  cat("\n==============================\n")
  cat("Stock:", symbol, "\n")
  cat("==============================\n")
  cat("Date range:", as.character(min(stock_df$Date)), "to", as.character(max(stock_df$Date)), "\n")
  cat("Rows:", nrow(stock_df), "\n\n")
  cat("First 5 rows:\n")
  print(head(stock_df, 5))
  cat("\nLast 5 rows:\n")
  print(tail(stock_df, 5))
  cat("\nStructure:\n")
  str(stock_df)
}

# 4b. Print the calculated statistics table for one or more stocks
display_statistics <- function(stats_df) {
  cat("\n----- Calculated Statistics -----\n")
  print(stats_df, row.names = FALSE)
}

# 4c. Combine statistics for every stock in the portfolio into one table
display_portfolio_statistics <- function(stock_data_list, ma_period = 20) {
  all_stats <- do.call(rbind, lapply(stock_data_list, calculate_statistics, ma_period = ma_period))
  rownames(all_stats) <- NULL
  cat("\n===== Portfolio Statistics Summary =====\n")
  print(all_stats, row.names = FALSE)
  return(all_stats)
}

# 4d. Plot a stock's closing price with its moving average overlaid
plot_stock_with_ma <- function(stock_df, ma_period = 20) {
  symbol <- attr(stock_df, "symbol")
  ma <- TTR::SMA(stock_df$Close, n = ma_period)
  plot(stock_df$Date, stock_df$Close, type = "l", col = "steelblue",
       main = paste(symbol, "- Closing Price with", ma_period, "-Day Moving Average"),
       xlab = "Date", ylab = "Price (USD)")
  lines(stock_df$Date, ma, col = "firebrick", lwd = 2)
  legend("topleft", legend = c("Close", paste0("MA", ma_period)),
         col = c("steelblue", "firebrick"), lty = 1, lwd = c(1, 2), bty = "n")
}

#==============================================================================
# 5. MAIN - configure and run the preliminary technical analysis process
#==============================================================================

# Read the portfolio and download each stock's historical data
stock_data <- load_stock_data("portfolio.txt")

# Display the raw imported data for every stock
for (sym in names(stock_data)) {
  display_stock_summary(stock_data[[sym]])
}

# Calculate and display statistics for every stock in the portfolio
portfolio_stats <- display_portfolio_statistics(stock_data, ma_period = 20)

# Plot each stock's price with its moving average (opens one plot per stock)
for (sym in names(stock_data)) {
  plot_stock_with_ma(stock_data[[sym]], ma_period = 20)
}

# Save the statistics summary to CSV for easy inclusion in the report
write.csv(portfolio_stats, "portfolio_statistics_summary.csv", row.names = FALSE)
cat("\nSaved portfolio_statistics_summary.csv\n")
