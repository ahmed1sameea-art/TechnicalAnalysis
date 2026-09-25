#==============================================================================
# BDA400 - Data Science Tools and Techniques
# Assignment 6: Technical Analysis using R, Visualization Phase (15%)
# Student: Sameea Ahmed
# File:    Assignment6/app.R  (R Shiny portfolio visualization dashboard)
#
# Project: third stage of the 3-part Technical Analysis project.
#   Stage 1 (A2) - load portfolio + basic statistics (technical_analysis.R)
#   Stage 2 (A5) - indicator functions written from scratch (sma.R, ema.R,
#                  macd.R, rsi.R, crossover.R, crossunder.R, ...)
#   Stage 3 (A6) - THIS FILE: an interactive Shiny dashboard that fetches
#                  stock data from Yahoo Finance, plots it as a line /
#                  candlestick / area / OHLC chart, overlays the Stage 2
#                  indicators (Moving Averages, RSI, MACD) with on/off
#                  toggles, applies trading rules and annotates the bars
#                  with BUY / SELL / HOLD signals.
#
# How to run (from RStudio):
#   1. Open this file and click "Run App"   - or -
#   2. In the console:  shiny::runApp("Assignment6")  from the repo root.
#   The app looks for the Assignment 5 indicator files in the repo root
#   (one folder up). If they cannot be found it falls back to the
#   equivalent TTR functions so the dashboard still runs.
#
# AI Assistance Declaration:
#   The structure, UI layout and comments of this script were drafted with
#   AI assistance (Claude) and reviewed by the student. Every number shown
#   in the dashboard (prices, indicator values, signals, returns) is
#   calculated by R at run time from the downloaded data - no numeric
#   results were computed, estimated or supplied by the AI itself.
#==============================================================================


#==============================================================================
# STEP 1: DATA COLLECTION AND SETUP
#==============================================================================

# ---- 1a. Data source ---------------------------------------------------------
# Primary source : Yahoo Finance, accessed through quantmod::getSymbols().
# Second source  : a CSV file uploaded by the user (e.g. a file exported from
#                  Yahoo Finance / any broker with Date, Open, High, Low,
#                  Close, Volume columns). Useful when offline or when Yahoo
#                  rate-limits requests.

# ---- 1b. Install (if missing) and load the necessary packages ---------------
required_packages <- c("shiny", "ggplot2", "quantmod")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)   # same as install.packages("shiny") etc.
  }
}
library(shiny)      # web application framework
library(ggplot2)    # grammar-of-graphics plotting
library(quantmod)   # getSymbols() for Yahoo Finance + OHLC helpers (loads xts/zoo/TTR)
library(grid)       # base R: used to stack the price / RSI / MACD panels

# ---- 1c. Load the indicator functions implemented in Assignment 5 -----------
# The previous stage of the project wrote sma(), ema(), macd(), rsi(),
# crossover() and crossunder() from scratch. They are re-used here so the
# dashboard is built on our own implementations.
find_file <- function(fname) {
  candidates <- c(fname, file.path("..", fname), file.path("indicators", fname))
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) hit[1] else NA_character_
}

a5_files <- c("sma.R", "ema.R", "macd.R", "rsi.R", "crossover.R", "crossunder.R")
a5_paths <- vapply(a5_files, find_file, character(1))

if (all(!is.na(a5_paths))) {
  # ema.R must be loaded before macd.R (macd() calls ema())
  for (p in a5_paths) source(p, local = FALSE)
  INDICATOR_SOURCE <- "Assignment 5 functions (sma.R, ema.R, macd.R, rsi.R, crossover.R, crossunder.R)"
} else {
  # Fallback: equivalent TTR implementations with the same interface
  warning("Assignment 5 indicator files not found - using TTR equivalents.")
  sma <- function(data, period) as.numeric(na.omit(TTR::SMA(data, n = period)))
  ema <- function(data, period) {
    as.numeric(TTR::EMA(data, n = 1, ratio = 2 / (period + 1)))
  }
  macd <- function(data, short_period, long_period, signal_period) {
    m <- ema(data, short_period) - ema(data, long_period)
    s <- ema(m, signal_period)
    list(macd_line = m, signal_line = s, histogram = m - s)
  }
  rsi <- function(data, period) as.numeric(TTR::RSI(data, n = period))
  crossover  <- function(a, b) c(FALSE, a[-1] >  b[-1] & a[-length(a)] <= b[-length(b)])
  crossunder <- function(a, b) c(FALSE, a[-1] <  b[-1] & a[-length(a)] >= b[-length(b)])
  INDICATOR_SOURCE <- "TTR package (fallback - Assignment 5 files not found)"
}

# ---- 1d. Portfolio symbols (from Assignment 2's portfolio.txt) --------------
portfolio_path <- find_file("portfolio.txt")
PORTFOLIO <- if (!is.na(portfolio_path)) {
  syms <- trimws(readLines(portfolio_path, warn = FALSE))
  toupper(syms[syms != ""])
} else {
  c("AAPL", "MSFT", "GOOGL", "AMZN", "TSLA")
}

# ---- 1e. Fetch historical stock data (with error handling) ------------------
# Downloads OHLCV data for one symbol from Yahoo Finance. Results are cached
# in memory so switching widgets does not re-download the same data.
.data_cache <- new.env()

fetch_stock_data <- function(stock_symbol, start_date, end_date) {
  key <- paste(stock_symbol, start_date, end_date, sep = "_")
  if (exists(key, envir = .data_cache)) return(get(key, envir = .data_cache))

  stock_data <- tryCatch(
    # harmless warnings (e.g. "contains missing values") are silenced here;
    # the missing values themselves are handled in clean_ohlc()
    suppressWarnings(getSymbols(stock_symbol, src = "yahoo",
                                from = start_date, to = end_date, auto.assign = FALSE)),
    error = function(e) stop(paste0("Could not download '", stock_symbol,
                                    "' from Yahoo Finance (check the ticker and your ",
                                    "internet connection). Details: ", conditionMessage(e)))
  )
  stock_data <- clean_ohlc(stock_data)
  assign(key, stock_data, envir = .data_cache)
  stock_data
}

# Reads an uploaded CSV (Date, Open, High, Low, Close[, Volume]) into xts.
read_csv_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- tolower(gsub("[^A-Za-z]", "", names(df)))
  need <- c("date", "open", "high", "low", "close")
  if (!all(need %in% names(df))) {
    stop("CSV must contain the columns: Date, Open, High, Low, Close (Volume optional).")
  }
  if (!"volume" %in% names(df)) df$volume <- NA_real_
  dates <- as.Date(df$date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y"))
  num <- function(v) suppressWarnings(as.numeric(gsub("[$,]", "", v)))
  x <- xts(cbind(Open = num(df$open), High = num(df$high), Low = num(df$low),
                 Close = num(df$close), Volume = num(df$volume)),
           order.by = dates)
  clean_ohlc(x)
}

# Handles missing / bad data: standard column names, carries the last
# known price forward over gaps, drops rows that are still incomplete,
# and removes duplicate dates.
clean_ohlc <- function(x) {
  if (is.null(x) || NROW(x) == 0) stop("No rows of data were returned.")
  x <- x[, 1:min(5, NCOL(x))]
  x <- cbind(Op(x), Hi(x), Lo(x), Cl(x),
             if (has.Vo(x)) Vo(x) else xts(rep(NA_real_, NROW(x)), index(x)))
  colnames(x) <- c("Open", "High", "Low", "Close", "Volume")
  x <- x[!duplicated(index(x)), ]
  x[, 1:4] <- na.locf(x[, 1:4], na.rm = FALSE)     # fill gaps with last price
  x <- x[complete.cases(x[, 1:4]), ]
  if (NROW(x) == 0) stop("All rows contained missing prices.")
  x
}

# Converts daily data to the requested time frame (Daily / Weekly / Monthly).
apply_time_frame <- function(x, time_frame) {
  out <- switch(time_frame,
                "Weekly"  = to.weekly(x,  indexAt = "endof", OHLC = TRUE),
                "Monthly" = to.monthly(x, indexAt = "endof", OHLC = TRUE),
                x)
  colnames(out) <- c("Open", "High", "Low", "Close", "Volume")
  out
}


#==============================================================================
# HELPER FUNCTIONS: INDICATORS AND TRADING RULES
#==============================================================================

# A5's sma() returns (n - period + 1) values; pad the front with NA so it
# lines up with the price series.
pad_front <- function(v, n) c(rep(NA_real_, n - length(v)), v)

moving_average <- function(close, period, type = "SMA") {
  if (length(close) < period) return(rep(NA_real_, length(close)))
  if (type == "EMA") {
    v <- ema(close, period)
    v[seq_len(period - 1)] <- NA   # hide the EMA warm-up period
    v
  } else {
    pad_front(sma(close, period), length(close))
  }
}

# A5's crossover()/crossunder() cannot handle NA (warm-up) values, so they
# are only evaluated on the positions where both series exist.
safe_cross <- function(a, b, fn) {
  if (length(b) == 1) b <- rep(b, length(a))
  out <- logical(length(a))
  idx <- which(!is.na(a) & !is.na(b))
  if (length(idx) >= 2) out[idx] <- fn(a[idx], b[idx])
  out
}

# Computes every indicator on the full (buffered) series.
compute_indicators <- function(x, p) {
  close <- as.numeric(Cl(x))
  n <- length(close)
  ind <- data.frame(Date = as.Date(index(x)),
                    Open = as.numeric(Op(x)), High = as.numeric(Hi(x)),
                    Low = as.numeric(Lo(x)),  Close = close)
  ind$ShortMA <- moving_average(close, p$short_n, p$ma_type)
  ind$LongMA  <- moving_average(close, p$long_n,  p$ma_type)
  ind$RSI <- if (n > p$rsi_n + 1) rsi(close, p$rsi_n) else NA_real_
  if (n > p$macd_slow) {
    m <- macd(close, p$macd_fast, p$macd_slow, p$macd_signal)
    warm <- seq_len(min(n, p$macd_slow + p$macd_signal - 2))
    m$macd_line[warm] <- NA; m$signal_line[warm] <- NA; m$histogram[warm] <- NA
    ind$MACD <- m$macd_line; ind$MACDSignal <- m$signal_line; ind$MACDHist <- m$histogram
  } else {
    ind$MACD <- ind$MACDSignal <- ind$MACDHist <- NA_real_
  }
  ind
}

# ---- STEP 4 (core): trading rules --------------------------------------------
# State  = Buy / Sell / Hold condition on every bar (as in the handout:
#          short MA > long MA -> "Buy", short MA < long MA -> "Sell").
# Signal = the bar on which the state CHANGES (the actual trade trigger),
#          detected with our crossover()/crossunder() functions.
generate_signals <- function(ind, p) {
  n <- nrow(ind)
  buy <- sell <- logical(n)
  state <- rep("Hold", n)

  ma_state <- ifelse(ind$ShortMA > ind$LongMA, "Buy",
                     ifelse(ind$ShortMA < ind$LongMA, "Sell", "Hold"))
  ma_state[is.na(ma_state)] <- "Hold"
  ma_buy  <- safe_cross(ind$ShortMA, ind$LongMA, crossover)
  ma_sell <- safe_cross(ind$ShortMA, ind$LongMA, crossunder)

  if (p$strategy == "Moving Average Crossover") {
    # If the short-term MA crosses above the long-term MA -> Buy
    # If the short-term MA crosses below the long-term MA -> Sell
    # Otherwise -> Hold
    state <- ma_state; buy <- ma_buy; sell <- ma_sell

  } else if (p$strategy == "RSI Overbought / Oversold") {
    # Buy when RSI climbs back above the oversold level,
    # Sell when RSI drops back below the overbought level.
    state <- ifelse(ind$RSI < p$rsi_low, "Buy", ifelse(ind$RSI > p$rsi_high, "Sell", "Hold"))
    buy  <- safe_cross(ind$RSI, p$rsi_low,  crossover)
    sell <- safe_cross(ind$RSI, p$rsi_high, crossunder)

  } else if (p$strategy == "MACD Signal-Line Crossover") {
    # Buy when the MACD line crosses above its signal line, Sell when below.
    state <- ifelse(ind$MACD > ind$MACDSignal, "Buy",
                    ifelse(ind$MACD < ind$MACDSignal, "Sell", "Hold"))
    buy  <- safe_cross(ind$MACD, ind$MACDSignal, crossover)
    sell <- safe_cross(ind$MACD, ind$MACDSignal, crossunder)

  } else if (p$strategy == "MA Crossover + RSI Filter") {
    # MA crossover, but ignore Buys when RSI is already overbought and
    # ignore Sells when RSI is already oversold.
    state <- ma_state
    buy  <- ma_buy  & !is.na(ind$RSI) & ind$RSI < p$rsi_high
    sell <- ma_sell & !is.na(ind$RSI) & ind$RSI > p$rsi_low
  }
  state[is.na(state)] <- "Hold"
  ind$State  <- state
  ind$Signal <- ifelse(buy, "Buy", ifelse(sell, "Sell", "Hold"))
  ind
}

# Simple long-only back-test of the signals inside the visible window:
# enter on a Buy (next bar), exit on a Sell. Calculated by R at run time.
backtest <- function(df) {
  if (nrow(df) < 2) return(NULL)
  pos <- numeric(nrow(df)); holding <- 0
  for (i in seq_len(nrow(df))) {
    if (df$Signal[i] == "Buy")  holding <- 1
    if (df$Signal[i] == "Sell") holding <- 0
    pos[i] <- holding
  }
  ret <- c(0, diff(df$Close) / head(df$Close, -1))
  strat <- c(0, head(pos, -1)) * ret        # act on the bar after the signal
  list(strategy = (prod(1 + strat) - 1) * 100,
       buyhold  = (tail(df$Close, 1) / df$Close[1] - 1) * 100,
       n_buy = sum(df$Signal == "Buy"), n_sell = sum(df$Signal == "Sell"),
       in_market = mean(pos) * 100)
}


#==============================================================================
# STEP 2: VISUALIZING STOCK DATA - Shiny app skeleton (UI)
#==============================================================================

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML("
    .kpi{background:#fff;border:1px solid #dee2e6;border-radius:8px;padding:10px 14px;margin-bottom:10px}
    .kpi .lbl{font-size:12px;color:#6c757d;text-transform:uppercase;letter-spacing:.04em}
    .kpi .val{font-size:22px;font-weight:600}
    .well{background:#f8f9fa}
    h4.sec{margin-top:14px;font-size:15px;font-weight:600;color:#2c3e50}
  "))),
  titlePanel(div("Portfolio Technical Analysis Dashboard",
                 tags$small(style = "display:block;font-size:14px;color:#6c757d;",
                            "BDA400 Assignment 6 - Sameea Ahmed"))),

  sidebarLayout(
    sidebarPanel(width = 3,
      # ---- Interactive widgets: data -----------------------------------------
      h4(class = "sec", "Data"),
      radioButtons("data_source", "Data source:",
                   choices = c("Yahoo Finance", "Upload CSV"), inline = TRUE),
      conditionalPanel("input.data_source == 'Yahoo Finance'",
        selectizeInput("stock_symbol", "Stock symbol:", choices = PORTFOLIO,
                       selected = PORTFOLIO[1],
                       options = list(create = TRUE, placeholder = "Type any ticker"))
      ),
      conditionalPanel("input.data_source == 'Upload CSV'",
        fileInput("csv_file", "CSV (Date, Open, High, Low, Close, Volume):",
                  accept = c(".csv", "text/csv"))
      ),
      dateRangeInput("date_range", "Select Date Range:",
                     start = "2023-01-01", end = "2023-07-01", max = Sys.Date()),
      selectInput("time_frame", "Select Time Frame:",
                  choices = c("Daily", "Weekly", "Monthly")),
      selectInput("chart_type", "Chart type:",
                  choices = c("Candlestick", "Line", "Area", "OHLC Bars")),

      # ---- Interactive widgets: indicators (STEP 3 toggles) ------------------
      h4(class = "sec", "Technical indicators"),
      checkboxGroupInput("technical_indicators", NULL,
                         choices = c("Moving Averages", "RSI", "MACD"),
                         selected = c("Moving Averages", "RSI", "MACD")),
      conditionalPanel("input.technical_indicators.indexOf('Moving Averages') > -1",
        fluidRow(
          column(6, numericInput("short_n", "Short MA", 20, min = 2, max = 200)),
          column(6, numericInput("long_n",  "Long MA",  50, min = 3, max = 400))
        ),
        radioButtons("ma_type", NULL, c("SMA", "EMA"), inline = TRUE)
      ),
      conditionalPanel("input.technical_indicators.indexOf('RSI') > -1",
        numericInput("rsi_n", "RSI period", 14, min = 2, max = 100)
      ),
      conditionalPanel("input.technical_indicators.indexOf('MACD') > -1",
        fluidRow(
          column(4, numericInput("macd_fast", "Fast", 12, min = 2)),
          column(4, numericInput("macd_slow", "Slow", 26, min = 3)),
          column(4, numericInput("macd_signal", "Signal", 9, min = 2))
        )
      ),

      # ---- Interactive widgets: trading rules (STEP 4) -----------------------
      h4(class = "sec", "Trading rules"),
      selectInput("strategy", "Strategy:",
                  choices = c("Moving Average Crossover", "RSI Overbought / Oversold",
                              "MACD Signal-Line Crossover", "MA Crossover + RSI Filter")),
      conditionalPanel("input.strategy.indexOf('RSI') > -1",
        sliderInput("rsi_levels", "RSI oversold / overbought:",
                    min = 5, max = 95, value = c(30, 70))
      ),
      checkboxInput("show_signals", "Annotate bars with signals", TRUE),
      radioButtons("annotation_mode", NULL,
                   choices = c("Signal changes only" = "events",
                               "Every bar (Buy/Sell/Hold)" = "all"),
                   selected = "events"),
      tags$small(class = "text-muted", paste("Indicators:", INDICATOR_SOURCE))
    ),

    mainPanel(width = 9,
      tabsetPanel(id = "tabs",
        tabPanel("Price Chart",
          br(),
          uiOutput("kpis"),
          uiOutput("chart_ui")
        ),
        tabPanel("Trading Signals",
          br(),
          uiOutput("backtest_ui"),
          h4(class = "sec", "Signal log (bars where a trade is triggered)"),
          tableOutput("signal_table")
        ),
        tabPanel("Portfolio Overview",
          br(),
          p(class = "text-muted",
            "All symbols in portfolio.txt over the selected date range, rebased to 100 ",
            "on the first day, with the current Moving Average Crossover state."),
          plotOutput("portfolio_chart", height = "420px"),
          tableOutput("portfolio_table")
        ),
        tabPanel("Data",
          br(),
          downloadButton("download_data", "Download chart data (CSV)"),
          br(), br(),
          tableOutput("data_table")
        )
      )
    )
  )
)


#==============================================================================
# SERVER LOGIC (STEPS 2, 3 and 4)
#==============================================================================

server <- function(input, output, session) {

  # Collect indicator / rule parameters into one list (with validation).
  params <- reactive({
    short_n <- max(2, as.integer(input$short_n %||% 20))
    long_n  <- max(short_n + 1, as.integer(input$long_n %||% 50))
    list(short_n = short_n, long_n = long_n, ma_type = input$ma_type,
         rsi_n = max(2, as.integer(input$rsi_n %||% 14)),
         macd_fast = max(2, as.integer(input$macd_fast %||% 12)),
         macd_slow = max(as.integer(input$macd_fast %||% 12) + 1,
                         as.integer(input$macd_slow %||% 26)),
         macd_signal = max(2, as.integer(input$macd_signal %||% 9)),
         rsi_low = input$rsi_levels[1] %||% 30, rsi_high = input$rsi_levels[2] %||% 70,
         strategy = input$strategy)
  })

  # ---- STEP 1 (in the app): fetch data -------------------------------------
  # Extra history ("buffer") is downloaded before the start date so the
  # 50-bar MA, RSI and MACD are already warmed up on the first visible bar.
  raw_data <- reactive({
    req(input$date_range)
    validate(need(input$date_range[1] < input$date_range[2],
                  "The start date must be before the end date."))
    p <- params()
    bars_needed <- max(p$long_n, p$macd_slow + p$macd_signal, p$rsi_n) + 5
    days_per_bar <- switch(input$time_frame, Daily = 1.5, Weekly = 7.5, Monthly = 31)
    buffer_start <- as.Date(input$date_range[1]) - ceiling(bars_needed * days_per_bar)

    if (input$data_source == "Upload CSV") {
      validate(need(!is.null(input$csv_file), "Upload a CSV file to begin."))
      x <- tryCatch(read_csv_data(input$csv_file$datapath),
                    error = function(e) validate(need(FALSE, conditionMessage(e))))
      label <- tools::file_path_sans_ext(input$csv_file$name)
    } else {
      sym <- toupper(trimws(input$stock_symbol))
      validate(need(nzchar(sym), "Choose or type a stock symbol."))
      x <- withProgress(message = paste("Downloading", sym, "from Yahoo Finance..."), {
        tryCatch(fetch_stock_data(sym, buffer_start, as.Date(input$date_range[2]) + 1),
                 error = function(e) validate(need(FALSE, conditionMessage(e))))
      })
      label <- sym
    }
    list(x = x, label = label)
  })

  # Indicators + signals on the buffered series, then filter to date range.
  chart_data <- reactive({
    rd <- raw_data(); p <- params()
    x <- apply_time_frame(rd$x, input$time_frame)
    ind <- compute_indicators(x, p)
    ind <- generate_signals(ind, p)
    # Filter data based on date range
    filtered_data <- subset(ind, Date >= as.Date(input$date_range[1]) &
                                 Date <= as.Date(input$date_range[2]))
    validate(need(nrow(filtered_data) >= 2,
                  "Not enough data in the selected date range / time frame."))
    filtered_data$Signal[1] <- "Hold"   # no trigger on the first visible bar
    list(df = filtered_data, label = rd$label)
  })

  # ---- KPI cards -----------------------------------------------------------
  output$kpis <- renderUI({
    # any data problem is reported once, under the chart, not here too
    cd <- tryCatch(chart_data(), error = function(e) NULL)
    req(cd)
    df <- cd$df
    last <- tail(df, 1)
    chg <- (last$Close / df$Close[1] - 1) * 100
    kpi <- function(lbl, val, col = "#2c3e50")
      column(3, div(class = "kpi", div(class = "lbl", lbl),
                    div(class = "val", style = paste0("color:", col), val)))
    state_col <- c(Buy = "#18bc9c", Sell = "#e74c3c", Hold = "#7b8a8b")[last$State]
    fluidRow(
      kpi(paste(cd$label, "last close"), sprintf("$%.2f", last$Close)),
      kpi("Change in range", sprintf("%+.2f%%", chg), ifelse(chg >= 0, "#18bc9c", "#e74c3c")),
      kpi("RSI (latest)", ifelse(is.na(last$RSI), "n/a", sprintf("%.1f", last$RSI))),
      kpi("Current state", last$State, state_col)
    )
  })

  # Chart height grows when the RSI / MACD panels are switched on.
  output$chart_ui <- renderUI({
    n_sub <- sum(c("RSI", "MACD") %in% input$technical_indicators)
    plotOutput("stock_chart", height = paste0(460 + 170 * n_sub, "px"))
  })

  # ---- STEP 2 + 3 + 4: the stock chart ---------------------------------------
  output$stock_chart <- renderPlot({
    cd <- chart_data()
    filtered_data <- cd$df
    p_par <- params()
    x_lim <- range(filtered_data$Date)
    bar_w <- max(0.4, 0.35 * as.numeric(median(diff(filtered_data$Date))))

    # Create ggplot based on user input
    p <- ggplot(data = filtered_data, aes(x = Date, y = Close))

    # Add various chart types based on user input
    if (input$chart_type == "Line") {
      p <- p + geom_line(colour = "#2c3e50", linewidth = 0.7)
    } else if (input$chart_type == "Area") {
      p <- p + geom_area(fill = "#3498db", alpha = 0.25) +
        geom_line(colour = "#2980b9", linewidth = 0.7) +
        # zoom the y axis to the price range (plus room for the annotations)
        coord_cartesian(ylim = range(c(filtered_data$Low, filtered_data$High)) +
                          c(-0.18, 0.10) * diff(range(c(filtered_data$Low, filtered_data$High))))
    } else if (input$chart_type == "Candlestick") {
      filtered_data$Dir <- ifelse(filtered_data$Close >= filtered_data$Open, "Up", "Down")
      p <- p +
        geom_segment(data = filtered_data,
                     aes(x = Date, xend = Date, y = Low, yend = High, colour = Dir),
                     linewidth = 0.4) +
        geom_rect(data = filtered_data,
                  aes(xmin = Date - bar_w, xmax = Date + bar_w,
                      ymin = pmin(Open, Close), ymax = pmax(Open, Close), fill = Dir),
                  colour = NA, inherit.aes = FALSE) +
        scale_fill_manual(values = c(Up = "#18bc9c", Down = "#e74c3c"), guide = "none") +
        scale_colour_manual(values = c(Up = "#18bc9c", Down = "#e74c3c"), guide = "none")
    } else if (input$chart_type == "OHLC Bars") {
      p <- p +
        geom_segment(aes(x = Date, xend = Date, y = Low, yend = High), colour = "#34495e") +
        geom_segment(aes(x = Date - bar_w, xend = Date, y = Open, yend = Open), colour = "#34495e") +
        geom_segment(aes(x = Date, xend = Date + bar_w, y = Close, yend = Close), colour = "#34495e")
    }

    # ---- STEP 3: add indicators as additional layers ----------------------
    if ("Moving Averages" %in% input$technical_indicators) {
      # Add Moving Averages (overlaid directly on the price chart)
      ma_long <- rbind(
        data.frame(Date = filtered_data$Date, Value = filtered_data$ShortMA,
                   Series = paste0(p_par$ma_type, "(", p_par$short_n, ")")),
        data.frame(Date = filtered_data$Date, Value = filtered_data$LongMA,
                   Series = paste0(p_par$ma_type, "(", p_par$long_n, ")")))
      p <- p + ggnewscale_line(ma_long)
    }

    # ---- STEP 4: annotate the bars with the trading signals ---------------
    if (isTRUE(input$show_signals)) {
      rng <- diff(range(c(filtered_data$Low, filtered_data$High)))
      if (input$annotation_mode == "all") {
        # Label every bar with its Buy / Sell / Hold state (handout style).
        # Shown as a signal strip (B / S / H) along the bottom of the chart
        # so the letters never overlap the candles.
        strip_y <- min(filtered_data$Low) - 0.14 * rng
        p <- p +
          geom_tile(data = filtered_data,
                    aes(x = Date, y = strip_y, height = 0.05 * rng, width = bar_w * 2),
                    fill = c(Buy = "#18bc9c", Sell = "#e74c3c", Hold = "#bdc3c7")[filtered_data$State],
                    alpha = 0.35, inherit.aes = FALSE) +
          geom_text(data = filtered_data,
                    aes(x = Date, y = strip_y, label = substr(State, 1, 1)),
                    colour = c(Buy = "#0e6655", Sell = "#922b21", Hold = "#7f8c8d")[filtered_data$State],
                    size = 2.4, fontface = "bold", inherit.aes = FALSE) +
          annotate("text", x = min(filtered_data$Date), y = strip_y + 0.05 * rng,
                   label = "State per bar: B = Buy, S = Sell, H = Hold",
                   hjust = 0, size = 3, colour = "#7f8c8d")
      }
      buys  <- filtered_data[filtered_data$Signal == "Buy", ]
      sells <- filtered_data[filtered_data$Signal == "Sell", ]
      if (nrow(buys) > 0) {
        p <- p +
          geom_point(data = buys, aes(x = Date, y = Low - 0.02 * rng), shape = 24,
                     size = 3.5, fill = "#18bc9c", colour = "#0e6655", inherit.aes = FALSE) +
          geom_label(data = buys, aes(x = Date, y = Low - 0.075 * rng, label = "BUY"),
                     fill = "#18bc9c", colour = "white", size = 3, fontface = "bold", inherit.aes = FALSE)
      }
      if (nrow(sells) > 0) {
        p <- p +
          geom_point(data = sells, aes(x = Date, y = High + 0.02 * rng), shape = 25,
                     size = 3.5, fill = "#e74c3c", colour = "#922b21", inherit.aes = FALSE) +
          geom_label(data = sells, aes(x = Date, y = High + 0.075 * rng, label = "SELL"),
                     fill = "#e74c3c", colour = "white", size = 3, fontface = "bold", inherit.aes = FALSE)
      }
    }

    p <- p +
      scale_x_date(limits = x_lim + c(-bar_w, bar_w), expand = expansion(mult = 0.01)) +
      scale_y_continuous(labels = function(v) paste0("$", format(v, big.mark = ","))) +
      labs(title = paste0(cd$label, " - ", input$time_frame, " ", input$chart_type,
                          " chart  |  ", p_par$strategy),
           x = NULL, y = "Price") +
      dash_theme()

    # Add RSI (own panel, 0-100 scale, with oversold / overbought bands)
    panels <- list(p)
    if ("RSI" %in% input$technical_indicators) {
      p_rsi <- ggplot(filtered_data, aes(Date, RSI)) +
        annotate("rect", xmin = x_lim[1] - bar_w, xmax = x_lim[2] + bar_w,
                 ymin = p_par$rsi_low, ymax = p_par$rsi_high, fill = "#8e44ad", alpha = 0.06) +
        geom_hline(yintercept = c(p_par$rsi_low, p_par$rsi_high),
                   linetype = "dashed", colour = "#8e44ad", alpha = 0.6) +
        geom_line(colour = "#8e44ad", linewidth = 0.7, na.rm = TRUE) +
        scale_x_date(limits = x_lim + c(-bar_w, bar_w), expand = expansion(mult = 0.01)) +
        scale_y_continuous(limits = c(0, 100), breaks = c(0, p_par$rsi_low, p_par$rsi_high, 100)) +
        labs(title = paste0("RSI(", p_par$rsi_n, ")"), x = NULL, y = "RSI") +
        dash_theme()
      panels <- c(panels, list(p_rsi))
    }
    # Add MACD (own panel: MACD line, signal line and histogram)
    if ("MACD" %in% input$technical_indicators) {
      macd_lines <- rbind(
        data.frame(Date = filtered_data$Date, Value = filtered_data$MACD, Series = "MACD"),
        data.frame(Date = filtered_data$Date, Value = filtered_data$MACDSignal, Series = "Signal"))
      p_macd <- ggplot() +
        geom_col(data = filtered_data,
                 aes(Date, MACDHist, fill = MACDHist >= 0), width = bar_w * 1.6, na.rm = TRUE) +
        scale_fill_manual(values = c(`TRUE` = "#18bc9c", `FALSE` = "#e74c3c"), guide = "none") +
        geom_line(data = macd_lines, aes(Date, Value, colour = Series),
                  linewidth = 0.7, na.rm = TRUE) +
        scale_colour_manual(values = c(MACD = "#2980b9", Signal = "#e67e22"), name = NULL) +
        geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
        scale_x_date(limits = x_lim + c(-bar_w, bar_w), expand = expansion(mult = 0.01)) +
        labs(title = paste0("MACD(", p_par$macd_fast, ", ", p_par$macd_slow, ", ",
                            p_par$macd_signal, ")"), x = NULL, y = "MACD") +
        dash_theme()
      panels <- c(panels, list(p_macd))
    }

    # Return the plot: price chart (3 parts height) + indicator panels (1 each)
    draw_stacked(panels, heights = c(3, rep(1.1, length(panels) - 1)))
  })

  # ---- Trading signals tab ---------------------------------------------------
  output$backtest_ui <- renderUI({
    df <- chart_data()$df
    bt <- backtest(df)
    req(bt)
    kpi <- function(lbl, val, col = "#2c3e50")
      column(2, div(class = "kpi", div(class = "lbl", lbl),
                    div(class = "val", style = paste0("color:", col), val)))
    tagList(
      p(class = "text-muted", paste0(
        "Strategy: ", input$strategy, ". Long-only test inside the selected window: ",
        "buy on the bar after a BUY signal, exit on the bar after a SELL signal ",
        "(no costs; for illustration only).")),
      fluidRow(
        kpi("Buy signals", bt$n_buy, "#18bc9c"),
        kpi("Sell signals", bt$n_sell, "#e74c3c"),
        kpi("Strategy return", sprintf("%+.2f%%", bt$strategy),
            ifelse(bt$strategy >= 0, "#18bc9c", "#e74c3c")),
        kpi("Buy & hold", sprintf("%+.2f%%", bt$buyhold),
            ifelse(bt$buyhold >= 0, "#18bc9c", "#e74c3c")),
        kpi("Time in market", sprintf("%.0f%%", bt$in_market))
      )
    )
  })

  output$signal_table <- renderTable({
    df <- chart_data()$df
    sig <- df[df$Signal != "Hold", c("Date", "Close", "ShortMA", "LongMA", "RSI",
                                     "MACD", "MACDSignal", "Signal")]
    validate(need(nrow(sig) > 0,
                  "No Buy/Sell signals in this range - try a longer date range or other parameters."))
    sig$Date <- format(sig$Date, "%Y-%m-%d")
    names(sig) <- c("Date", "Close", "Short MA", "Long MA", "RSI", "MACD", "MACD signal", "Signal")
    sig
  }, digits = 2, striped = TRUE, hover = TRUE)

  # ---- Portfolio overview tab ------------------------------------------------
  portfolio_data <- reactive({
    req(input$tabs == "Portfolio Overview")
    p <- params()
    start <- as.Date(input$date_range[1]); end <- as.Date(input$date_range[2])
    buffer_start <- start - ceiling(p$long_n * 1.5) - 10
    res <- list()
    withProgress(message = "Downloading portfolio...", value = 0, {
      for (s in PORTFOLIO) {
        incProgress(1 / length(PORTFOLIO), detail = s)
        x <- tryCatch(fetch_stock_data(s, buffer_start, end + 1), error = function(e) NULL)
        if (is.null(x)) next       # skip symbols that fail, keep the rest
        cl <- as.numeric(Cl(x))
        d <- data.frame(Date = as.Date(index(x)), Close = cl,
                        ShortMA = moving_average(cl, p$short_n, p$ma_type),
                        LongMA  = moving_average(cl, p$long_n,  p$ma_type))
        d <- d[d$Date >= start & d$Date <= end, ]
        if (nrow(d) < 2) next
        d$Symbol <- s
        d$Rebased <- d$Close / d$Close[1] * 100
        res[[s]] <- d
      }
    })
    validate(need(length(res) > 0, "Could not download any portfolio symbols from Yahoo Finance."))
    res
  })

  output$portfolio_chart <- renderPlot({
    pd <- do.call(rbind, portfolio_data())
    ggplot(pd, aes(Date, Rebased, colour = Symbol)) +
      geom_hline(yintercept = 100, colour = "grey60", linetype = "dashed") +
      geom_line(linewidth = 0.8) +
      labs(title = "Portfolio performance (rebased to 100)", x = NULL, y = "Index") +
      dash_theme() + theme(legend.position = "right")
  })

  output$portfolio_table <- renderTable({
    rows <- lapply(portfolio_data(), function(d) {
      last <- tail(d, 1)
      state <- if (is.na(last$ShortMA) || is.na(last$LongMA)) "Hold"
               else if (last$ShortMA > last$LongMA) "Buy"
               else if (last$ShortMA < last$LongMA) "Sell" else "Hold"
      data.frame(Symbol = last$Symbol, `First close` = d$Close[1], `Last close` = last$Close,
                 `Return %` = (last$Close / d$Close[1] - 1) * 100,
                 `High` = max(d$Close), `Low` = min(d$Close),
                 `MA state` = state, check.names = FALSE)
    })
    do.call(rbind, rows)
  }, digits = 2, striped = TRUE, hover = TRUE)

  # ---- Data tab --------------------------------------------------------------
  output$data_table <- renderTable({
    df <- chart_data()$df
    df$Date <- format(df$Date, "%Y-%m-%d")
    tail(df[, c("Date", "Open", "High", "Low", "Close", "ShortMA", "LongMA",
                "RSI", "MACD", "MACDSignal", "State", "Signal")], 250)
  }, digits = 2, striped = TRUE)

  output$download_data <- downloadHandler(
    filename = function() paste0(chart_data()$label, "_", input$time_frame, "_signals.csv"),
    content = function(file) write.csv(chart_data()$df, file, row.names = FALSE)
  )
}


#==============================================================================
# PLOTTING HELPERS
#==============================================================================

# Shared ggplot theme for every panel.
dash_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 12, colour = "#2c3e50"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#ecf0f1"),
          legend.position = "top", legend.justification = "left",
          plot.margin = margin(4, 10, 4, 6))
}

# Moving-average layer with its own legend (orange = short, blue = long).
ggnewscale_line <- function(ma_long) {
  lv <- unique(ma_long$Series)
  ma_long$Series <- factor(ma_long$Series, levels = lv)
  list(
    geom_line(data = ma_long, aes(x = Date, y = Value, linetype = Series),
              colour = NA, na.rm = TRUE, inherit.aes = FALSE),   # legend carrier
    geom_line(data = ma_long[ma_long$Series == lv[1], ],
              aes(x = Date, y = Value), colour = "#e67e22", linewidth = 0.9,
              na.rm = TRUE, inherit.aes = FALSE),
    geom_line(data = ma_long[ma_long$Series == lv[2], ],
              aes(x = Date, y = Value), colour = "#2980b9", linewidth = 0.9,
              na.rm = TRUE, inherit.aes = FALSE),
    scale_linetype_manual(values = c("solid", "solid"), name = NULL,
                          guide = guide_legend(override.aes = list(
                            colour = c("#e67e22", "#2980b9"), linewidth = 1.2)))
  )
}

# Stacks several ggplots vertically with aligned x axes (base grid only).
draw_stacked <- function(plots, heights) {
  grobs <- lapply(plots, ggplotGrob)
  max_w <- do.call(grid::unit.pmax, lapply(grobs, function(g) g$widths))
  grobs <- lapply(grobs, function(g) { g$widths <- max_w; g })
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(length(grobs), 1,
                                             heights = unit(heights, "null"))))
  for (i in seq_along(grobs)) {
    pushViewport(viewport(layout.pos.row = i, layout.pos.col = 1))
    grid.draw(grobs[[i]])
    popViewport()
  }
  popViewport()
}

# Null-default operator (base R >= 4.4 has one; defined here for older R).
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a


#==============================================================================
# RUN THE APP
#==============================================================================
shinyApp(ui, server)
