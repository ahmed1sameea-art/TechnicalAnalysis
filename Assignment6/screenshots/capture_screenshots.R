#==============================================================================
# BDA400 Assignment 6 - helper script (Sameea Ahmed)
# Captures screenshots of the running dashboard for the documentation.
# Starts app.R in a background R process, drives it with headless Chrome
# (chromote) and saves PNG files into Assignment6/screenshots/.
# Run from the repository root:  source("Assignment6/screenshots/capture_screenshots.R")
#
# AI Assistance Declaration: drafted with AI assistance (Claude); it only
# takes screenshots - all values shown are computed by the app at run time.
#==============================================================================
for (p in c("callr", "chromote")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)

app_dir  <- normalizePath("Assignment6")
shot_dir <- file.path(app_dir, "screenshots")
port <- 8765
url  <- paste0("http://127.0.0.1:", port)

# 1. start the Shiny app in the background
app_proc <- callr::r_bg(function(dir, port) shiny::runApp(dir, port = port, launch.browser = FALSE),
                        args = list(dir = app_dir, port = port))
Sys.sleep(8)

# 2. open it in headless Chrome
b <- chromote::ChromoteSession$new()
b$Emulation$setDeviceMetricsOverride(width = 1500, height = 1300, deviceScaleFactor = 1, mobile = FALSE)
b$Page$navigate(url); Sys.sleep(10)

js   <- function(code, wait = 6) { b$Runtime$evaluate(code); Sys.sleep(wait) }
shot <- function(name) {
  b$screenshot(file.path(shot_dir, name), selector = "html", show = FALSE)
  message("saved ", name)
}
sel  <- function(id, value) sprintf("$('#%s')[0].selectize.setValue('%s');", id, value)
ind  <- function(name, on) sprintf(
  "var c=$('input[name=technical_indicators][value=\"%s\"]'); if (c.prop('checked') != %s) c.click();",
  name, tolower(on))

# 01 - default view: AAPL daily candlestick, MA + RSI + MACD, MA crossover
shot("01_candlestick_all_indicators.png")

# 02 - line chart, EMA, MACD signal-line strategy
js(paste(sel("chart_type", "Line"), sel("strategy", "MACD Signal-Line Crossover"),
         "$('input[name=ma_type][value=EMA]').click();"))
shot("02_line_ema_macd_strategy.png")

# 03 - area chart with RSI and MACD switched OFF (indicator toggles)
js(paste(sel("chart_type", "Area"), "$('input[name=ma_type][value=SMA]').click();",
         sel("strategy", "Moving Average Crossover"), ind("RSI", FALSE), ind("MACD", FALSE)))
shot("03_area_ma_only_toggles.png")

# 04 - OHLC bars, every-bar Buy/Sell/Hold annotation, MA + RSI filter
js(paste(sel("chart_type", "OHLC Bars"), ind("RSI", TRUE), ind("MACD", TRUE),
         sel("strategy", "MA Crossover + RSI Filter"),
         "$('input[name=annotation_mode][value=all]').click();"))
shot("04_ohlc_every_bar_annotations.png")

# 05 - weekly candlestick, MSFT
js(paste(sel("chart_type", "Candlestick"), sel("strategy", "Moving Average Crossover"),
         "$('input[name=annotation_mode][value=events]').click();",
         sel("stock_symbol", "MSFT"), sel("time_frame", "Weekly")), 10)
shot("05_msft_weekly_candlestick.png")

# 06 - TSLA over a longer range (2022-2024): several MA crossover signals
js(paste(sel("stock_symbol", "TSLA"), sel("time_frame", "Daily"),
         "var d=$('#date_range input'); d.eq(0).bsDatepicker('update','2022-01-01');",
         "d.eq(1).bsDatepicker('update','2024-12-31'); d.trigger('change');"), 12)
shot("06_tsla_2022_2024_ma_signals.png")

# 07 - trading signals tab for the same view
js("$('a[data-value=\"Trading Signals\"]').click();")
shot("07_trading_signals_tab.png")

# 08 - portfolio overview tab
js("$('a[data-value=\"Portfolio Overview\"]').click();", 25)
shot("08_portfolio_overview.png")

# 09 - error handling: invalid ticker
js("$('a[data-value=\"Price Chart\"]').click();", 2)
js("var s=$('#stock_symbol')[0].selectize; s.addOption({value:'NOTAREALTICKER',label:'NOTAREALTICKER'}); s.setValue('NOTAREALTICKER');", 10)
shot("09_error_handling_invalid_ticker.png")

b$close(); app_proc$kill()
message("Done - screenshots saved in ", shot_dir)
