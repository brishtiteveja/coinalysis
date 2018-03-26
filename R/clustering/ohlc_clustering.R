library(stringr)
library(XML)
library(RCurl)
library(mlbench)
library(dplyr)

#```{r global info}

# List all coins
get_list_of_all_coinmarketcap_coins <- function() {
  url = 'https://coinmarketcap.com/all/views/all/'
  url_parsed <- htmlParse(getURL(url), asText = TRUE)
  tableNodes <- getNodeSet(url_parsed, c('//*[@id="currencies-all"]'))
  all_currency <- readHTMLTable(tableNodes[[1]])
  
  currencies <- gsub("\n"," ", all_currency$Name)
  
  #currencies <- str_replace(gsub("\\s+", " ", str_trim(currencies)), "B", "b")
  
  c_symbols <- c()
  c_names <- c()
  
  for(c in currencies) {
    cr <- strsplit(c, " ")
    c_symbols <- c(c_symbols, cr[[1]][1])
    c_names <- c(c_names, cr[[1]][2])
  }
  
  cr_df <- cbind(c_names, c_symbols)
  colnames(cr_df) <- c('Name', 'Symbol')
  
  return(as.data.frame(cr_df))
}

# Function to create url from start to end
get_currency_data_download_url <- function(currency, history_start, history_end) {
  # Processing URL to download data
  url_cmarket <- 'https://coinmarketcap.com/'
  currency_part <- paste('currencies/', currency, sep="")
  url_pre <- paste(url_cmarket, currency_part, sep="")
  
  # Append start date
  url_s <- paste(url_pre, '/historical-data/?start=', sep="")
  url_s <- paste(url_s, history_start, sep="")
  
  # Append end date
  url_e <- paste(url_s, '&end=', sep="")
  url_e <- paste(url_e, history_end, sep="")
  
  url <- url_e
}


get_currency_historical_data <- function(url) {
  url_parsed <- htmlParse(getURL(url), asText = TRUE)
  #print(url_parsed)
  tableNodes <- getNodeSet(url_parsed, c('//*[@class="table"]'))
  #print(tableNodes)
  
  currency_historical_data <- readHTMLTable(tableNodes[[1]])
  
  return(currency_historical_data)
}
#```

history_start <- '20090101'
history_end <- format(Sys.time(), "%Y%m%d")

if (exists("coin_info") == FALSE)
  coin_info <- get_list_of_all_coinmarketcap_coins()
coin_info <- coin_info[complete.cases(coin_info),]

numCoins <- 5
coins <- coin_info[1:numCoins,2] # First 15 coins
coinList <- as.character(coins)
coinNames <- as.character(coin_info[1:numCoins,1])
bitcoin <- coinNames[1]
url <- get_currency_data_download_url(bitcoin, history_start, history_end)

btc_df <- get_currency_historical_data(url)

coinMetrics <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Market Cap')
btc_ohlc <- btc_df[, coinMetrics[1:4]]
dm <- dim(btc_ohlc_list)

time <- as.Date(btc_df$Date, format='%b %d, %Y')
btc_ohlc$Open <- as.numeric(as.character(btc_ohlc$Open))
btc_ohlc$High <- as.numeric(as.character(btc_ohlc$High))
btc_ohlc$Low <- as.numeric(as.character(btc_ohlc$Low))
btc_ohlc$Close <- as.numeric(as.character(btc_ohlc$Close))

library(xts)
btc_ts <- xts(x=btc_ohlc, order.by = time)
plot.xts(btc_ts)
#candlecolors <- ifelse(btc_ts[,'Close'] > btc_ts[,'Open'], 'RED', 'GREEN')
#plot.xts(btc_ts, type='candles',  width=25000, candle.col=candlecolors, bar.col='BLACK')

dygraph(btc_ts) %>% dyCandlestick()

dir <- getwd()
f <- paste(dir, '/binance/BinanceData/Binance_tickers_all_15m_2018-03-20.Rda', sep="")
load(f)

btc_df <- tcks['BTCUSDT'][[1]]
BinanceCoinMetrics <- c('open_time', 'open', 'high', 'low', 'close', 
                        'volume', 'close_time', 'quote_asset_volume',
                        'trades', 'taker_buy_base_asset_volume',
                        'taker_buy_quote_asset_volume', 'symbol')
btc_ohlc <- btc_df[, BinanceCoinMetrics[2:5]]
colnames(btc_ohlc) <- coinMetrics[1:4]
dm <- dim(btc_ohlc_list)

time <- as.POSIXct(btc_df$close_time)
btc_ohlc$Open <- as.numeric(as.character(btc_ohlc$Open))
btc_ohlc$High <- as.numeric(as.character(btc_ohlc$High))
btc_ohlc$Low <- as.numeric(as.character(btc_ohlc$Low))
btc_ohlc$Close <- as.numeric(as.character(btc_ohlc$Close))

library(xts)
btc_ts <- xts(x=btc_ohlc, order.by = time)

dygraph(btc_ts) %>% dyCandlestick()

library(quantmod)
x <- btc_ts
ClOp <-Cl(x) /  Op(x)
HiOp <-Hi(x) /  Op(x)
LoOp <- Lo(x) / Op(x)
dat<- as.matrix(data.frame(ClOp, HiOp , LoOp))

# Identify 7 clusters
km <- kmeans(dat,7)
scatterplot3d(dat,color=km$cluster)
