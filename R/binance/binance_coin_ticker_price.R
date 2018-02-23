library(binancer)
source('config.R')

binance_credentials(API_KEY_BINANCE, secret = SECRET_BINANCE)

binance_coins <- binance_coins()

b_tck <- data.frame(binancer::binance_ticker_all_prices(), stringsAsFactors = FALSE)

# Group by BTC, ETH, BNB tickers
btc_sym <- grep(".*BTC$", b_tck$symbol )
b_btc_tck <- b_tck[btc_sym,]

eth_sym <- grep(".*ETH$", b_tck$symbol )
b_tck <- b_tck[eth_sym,]

bnb_sym <- grep(".*BNB$", b_tck$symbol)
b_bnb_tck <- b_tck[bnb_sym,]

mnts <- function(u) {
  x <- u * 60
  return(x)
}

hrs <- function(u) {
  x <- u * 60 * 60
  return(x)
}

days <- function(u) {
  x <- u * 60*60*24
  return(x)
}

wks <- function(u) {
  x <- u * 60 * 60 * 24 * 7
  return(x)
}

# Last seven day ticker price
today <- Sys.time()
wk <- 1

# date format
dtfm <- "%Y-%m-%d"
start_time <- format(today, dtfm)
end_time   <- format(today - wks(wk), dtfm)

#eth_tcks <- tcks

b_tck <- b_btc_tck
# Get the ticker price
tcks <- list()
i <- 1
for(t in b_tck$symbol) {
  msg <- paste(i, ". Getting Daily data for ticker ", t, ":")
  print(msg)
  tryCatch(
  {
    tcks[[t]] <- data.frame(binance_klines(t, "1d"), stringsAsFactors = FALSE)
  }, error = function(cond){
        print(paste("----> Error occurred for ticker ", t," ", cond))
  }, warning = function(cond) {
        print(paste("#### Warning occurred for ticker ", t, " ", cond))
  })
  i <- i + 1
}



# Daily price change of all tickers
b_tck_daily_change <- list()
b_tck_dyday_change <- list()
b_tck_daily_change_vec <- c()
b_tck_dyday_change_vec <- c()

for (t in b_tck$symbol) {
  df <- tcks[[t]]
  df <- df[order(df['open_time'], decreasing=TRUE),]
  nr <- nrow(df)
  pr_today <- df[1,]$close
  pr_yday <- df[2,]$close
  pr_dyday <- df[3,]$close
  diff <- pr_today - pr_yday
  diffB <- pr_yday - pr_dyday
  dCh <- diff/pr_yday * 100
  dChB <- diffB/pr_dyday * 100
  b_tck_daily_change[[t]] <- dCh
  b_tck_dyday_change[[t]] <- dChB
  b_tck_daily_change_vec <- c(b_tck_daily_change_vec, dCh)
  b_tck_dyday_change_vec <- c(b_tck_dyday_change_vec, dChB)
}

syms <- gsub("BTC", "", b_tck$symbol)
b_tck_daily_change_df <- data.frame(cbind(
                                          syms),
                                          as.numeric(b_tck_daily_change_vec
                                             ),
                                          stringsAsFactors = FALSE
                                        )
b_tck_dyday_change_df <- data.frame(cbind(
                                          syms),
                                          as.numeric(b_tck_dyday_change_vec
                                             ),
                                          stringsAsFactors = FALSE
                                        )
colnames(b_tck_daily_change_df) <- c('ticker', 'dChPer')
colnames(b_tck_dyday_change_df) <- c('ticker', 'dyChPer')

b_tck_daily_change_df <- b_tck_daily_change_df[order(b_tck_daily_change_df['dChPer'], decreasing = TRUE),]
b_tck_dyday_change_df <- b_tck_dyday_change_df[order(b_tck_dyday_change_df['dyChPer'], decreasing = TRUE),]

library(dplyr)
library(ggplot2)
p_d <- b_tck_daily_change_df %>%
   filter(dChPer > 0) %>%
   ggplot(aes(ticker, dChPer)) +
      geom_bar(stat='identity') +
      ylab("%24hr Change ") +
      theme(axis.text.x = element_text(angle=90, hjust = 1))

p_dy<- b_tck_dyday_change_df %>%
   filter(dyChPer > 0) %>%
   ggplot(aes(ticker, dyChPer)) +
      geom_bar(stat='identity') +
      ylab("%24hr Change yesterday ") +
      theme(axis.text.x = element_text(angle=90, hjust = 1))

library(cowplot)
plot_grid(p_d, p_dy, nrow=2, ncol=1)

# Consecutive percentage change for a coin
pos_ch_coins <- b_tck_daily_change_df %>%
                        filter(dChPer > 0)

col <- c(1,2,3,4,5,6,7,8,9,10)
i <- 1
for (t in b_tck$symbol) {
  #ticker <- paste(t, "BTC", sep="")
  #df <- tcks[[ticker]]
  t <- 'ADXBTC'
  print(paste("ticker ", t))
  df <- tcks[[t]]
  df <- df[order(df['open_time'], decreasing=TRUE),]
  nr <- nrow(df)
  dat <- data.frame(close=cbind(df$close))
  ch <- -diff(dat$close)/dat[-1,]*100
  plot(df$close_time[2:nr], ch, ylab=t, type='l', col=(i%%10))
  abline(h=0,lty=2)
  i <- i+1
}

library(DT)
datatable(b_tck_daily_change_df)

df <- binance_klines("ADXBTC",'15m')
df <- binance_klines("EVXBTC",'15m')
df <- binance_klines("VENBTC",'15m')

library(plotly)
p <- df %>%
  plot_ly(x = ~open_time, type="candlestick",
          open = ~open, close = ~close,
          high = ~high, low = ~low) %>%
  layout(title = "ADXBTC Candlestick Chart")

p

