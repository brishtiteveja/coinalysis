library(stringr)
library(XML)
library(RCurl)
library(mlbench)
library(plotly)
library(shiny)
library(shinythemes)
library(dplyr)
library(quantstrat)
library(quantmod)
library(xts)


#```{r global info}

# List all coins
get_list_of_all_coinmarketcap_coins <- function() {
  url = 'https://coinmarketcap.com/all/views/all/'
  url_parsed <- htmlParse(getURL(url), asText = TRUE)
  tableNodes <- getNodeSet(url_parsed, c('//*[@id="currencies-all"]'))
  all_currency <- readHTMLTable(tableNodes[[1]])

  currencies <- gsub("\n","", all_currency$Name)

  currencies <- str_replace(gsub("\\s+", " ", str_trim(currencies)), "B", "b")

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

currency1 <- 'bitcoin'
url1 <- get_currency_data_download_url(currency1, history_start, history_end)
data <- get_currency_historical_data(url1)
date <- as.Date(rev(data$Date), "%B %d, %Y")
data$Date <- rev(date)

btc_data <- data
btc_close_data <- as.double(as.character(btc_data$Close))
dat <- xts(x = btc_close_data, order.by = as.POSIXct(btc_data[,1]))
btc_sma_7_data <- SMA(dat, n = 7)


currency2 <- 'ethereum'
url2 <- get_currency_data_download_url(currency2, history_start, history_end)
data <- get_currency_historical_data(url2)
date <- as.Date(rev(data$Date), "%B %d, %Y")
data$Date <- rev(date)

eth_data <- data
eth_close_data <- as.double(as.character(eth_data$Close))
dat <- xts(x = eth_close_data, order.by = as.POSIXct(eth_data[,1]))
eth_sma_7_data <- SMA(dat, n = 7)

currency3 <- 'ripple'
url3 <- get_currency_data_download_url(currency3, history_start, history_end)
data <- get_currency_historical_data(url3)
date <- as.Date(rev(data$Date), "%B %d, %Y")
data$Date <- rev(date)

xrp_data <- data
xrp_close_data <- as.double(as.character(xrp_data$Close))
dat <- xts(x = xrp_close_data, order.by = as.POSIXct(xrp_data[,1]))
xrp_sma_7_data <- SMA(dat, n = 7)

currency4 <- 'litecoin'
url4 <- get_currency_data_download_url(currency4, history_start, history_end)
data <- get_currency_historical_data(url4)
date <- as.Date(rev(data$Date), "%B %d, %Y")
data$Date <- rev(date)

ltc_data <- data
ltc_close_data <- as.double(as.character(ltc_data$Close))
dat <- xts(x = ltc_close_data, order.by = as.POSIXct(ltc_data[,1]))
ltc_sma_7_data <- SMA(dat, n = 7)

currency5 <- 'cardano'
url5 <- get_currency_data_download_url(currency5, history_start, history_end)
data <- get_currency_historical_data(url5)
date <- as.Date(rev(data$Date), "%B %d, %Y")
data$Date <- rev(date)

ada_data <- data
ada_close_data <- as.double(as.character(ada_data$Close))
dat <- xts(x = ada_close_data, order.by = as.POSIXct(ada_data[,1]))
ada_sma_7_data <- SMA(dat, n = 7)

currency6 <- 'stellar'
url6 <- get_currency_data_download_url(currency6, history_start, history_end)
data <- get_currency_historical_data(url6)
date <- as.Date(rev(data$Date), "%B %d, %Y")
data$Date <- rev(date)

xlm_data <- data
xlm_close_data <- as.double(as.character(xlm_data$Close))
dat <- xts(x = xlm_close_data, order.by = as.POSIXct(xlm_data[,1]))
xlm_sma_7_data <- SMA(dat, n = 7)

currency7 <- 'monero'
url7 <- get_currency_data_download_url(currency7, history_start, history_end)
data <- get_currency_historical_data(url7)
date <- as.Date(rev(data$Date), "%B %d, %Y")
data$Date <- rev(date)

xmr_data <- data
xmr_close_data <- as.double(as.character(xmr_data$Close))
dat <- xts(x = xmr_close_data, order.by = as.POSIXct(xmr_data[,1]))
xmr_sma_7_data <- SMA(dat, n = 7)

coins <- c('BTC', 'ETH', 'XRP', 'LTC', 'ADA', 'XLM', 'XMR')

selectorOptions <- list(
  buttons = list(list(
    step= 'month',
    stepmode= 'backward',
    count= 1,
    label= '1m'
  ), list(
    step= 'month',
    stepmode= 'backward',
    count= 6,
    label= '6m'
  ), list(
    step= 'year',
    stepmode= 'todate',
    count= 1,
    label= 'YTD'
  ), list(
    step= 'year',
    stepmode= 'backward',
    count= 1,
    label= '1y'
  ), list(
    step= 'all',
    label= 'ALL'
  ))
)
chart_type = 'candlestick' # 'ohlc'
