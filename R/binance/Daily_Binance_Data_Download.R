library(binancer)
source('../binance/config.R')

Sys.setenv(http_proxy="http://staff-proxy.ul.ie:8080")

# Authenticate with Binance credentials
binance_credentials(API_KEY_BINANCE, secret = SECRET_BINANCE)

# Get all the tickers from Binance
b_tck <- data.frame(binancer::binance_ticker_all_prices(), stringsAsFactors = FALSE)

# Download tickers function
download_tickers <- function(tm) {
  tcks <- list()
  i <- 1
  for(t in b_tck$symbol) {
    msg <- paste(i, ". Getting Daily", tm, "data for ticker ", t, ":")
    print(msg)
    tryCatch(
      {
        tcks[[t]] <- data.frame(binance_klines(t, tm), stringsAsFactors = FALSE)
      }, error = function(cond){
        print(paste("----> Error occurred for ticker ", t," ", cond))
      }, warning = function(cond) {
        print(paste("#### Warning occurred for ticker ", t, " ", cond))
      })
    i <- i + 1
  }
  
  return(tcks)
}

# Get the date of download
today <- Sys.time()
today <- format(Sys.time(), "%Y-%m-%d")
cur_tck_n <- 'all'

# Get the ticker prices for every 1m
tm <- '1m'
tcks <- download_tickers(tm)

# Save the data frame
fn <- paste("../binance/BinanceData/Binance_tickers_",cur_tck_n,"_", tm,"_", today,".Rda", sep="")
save(tcks, file=fn)

# Get the ticker prices for every 3m
tm <- '3m'
tcks <- download_tickers(tm)

# Save the data frame
fn <- paste("../binance/BinanceData/Binance_tickers_",cur_tck_n,"_", tm,"_", today,".Rda", sep="")
save(tcks, file=fn)

# Get the ticker prices for every 5m
tm <- '5m'
tcks <- download_tickers(tm)

# Save the data frame
fn <- paste("../binance/BinanceData/Binance_tickers_",cur_tck_n,"_", tm,"_", today,".Rda", sep="")
tcks <- save(tcks, file=fn)

# Get the ticker prices for every 15m
tm <- '15m'
tcks <- download_tickers(tm)

# Save the data frame
fn <- paste("../binance/BinanceData/Binance_tickers_",cur_tck_n,"_", tm,"_", today,".Rda", sep="")
tcks <- save(tcks, file=fn)

# Get the ticker prices for every 30m
tm <- '30m'
tcks <- download_tickers(tm)

# Save the data frame
fn <- paste("../binance/BinanceData/Binance_tickers_",cur_tck_n,"_", tm,"_", today,".Rda", sep="")
tcks <- save(tcks, file=fn)

# Get the ticker prices for every 30m
tm <- '1h'
tcks <- download_tickers(tm)

# Save the data frame
fn <- paste("../binance/BinanceData/Binance_tickers_",cur_tck_n,"_", tm,"_", today,".Rda", sep="")
tcks <- save(tcks, file=fn)

# Get the ticker prices for every 30m
tm <- '1d'
tcks <- download_tickers(tm)

# Save the data frame
fn <- paste("../binance/BinanceData/Binance_tickers_",cur_tck_n,"_", tm,"_", today,".Rda", sep="")
tcks <- save(tcks, file=fn)
