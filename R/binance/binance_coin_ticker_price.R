library(binancer)
source('config.R')

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

cur_tck_n <- 'ETH'
b_tck <- b_eth_tck
cur_tck <- b_tck

# Get the ticker price
tcks <- list()
i <- 1
tm <- '1d'
for(t in b_tck$symbol) {
  msg <- paste(i, ". Getting Daily data for ticker ", t, ":")
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

# Save the data frame
today <- format(Sys.time(), "%Y-%m-%d")
fn <- paste("BinanceData/Binance_tickers_",cur_tck_n,"_", tm,"_", today,".Rda", sep="")
save(tcks, file=fn)

# Loading the data frame
daily_tck <- load(fn)

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

syms <- gsub(cur_tck_n, "", b_tck$symbol)
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
      ylab("%24hr Change (ETH)") +
      theme(axis.text.x = element_text(angle=90, hjust = 1))

p_dy<- b_tck_dyday_change_df %>%
   filter(dyChPer > 0) %>%
   ggplot(aes(ticker, dyChPer)) +
      geom_bar(stat='identity') +
      ylab("%24hr Change yesterday (ETH)") +
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
datatable(b_tck_dyday_change_df)

b_tck_daily_change <- list()
b_tck_dyday_change <- list()

n = 108 # num of coins
d = 15 # 2 weeks
leg = c()
colr = c()
ltyp = c()

i = 1
coins_up_trend <- c()
coins_up_trend_slope <- c()
for (tk in b_tck_daily_change_df$ticker[1:n]) {
  t <- paste(tk,"ETH", sep="")
  print(paste("Ticker", t))
  df <- tcks[[t]]
  df <- df[order(df['close_time'], decreasing=TRUE),]
  nr <- nrow(df)
  diff <- -diff(df$close)
  b_tck_daily_change[[t]] <- diff
  x_c = df$close_time
  y_c = df$close
  m = lm(y_c ~ x_c)
  cf <- coef(m)
  sl <- cf[[2]]
  if(sl > 0) {
    x <- as.numeric(df$close_time[2:d])
    y <- as.numeric(b_tck_daily_change[[t]][1:(d-1)])
    plot(x,y,t='l', xlab='n',ylab='n',xaxt='n', yaxt='n', col=i, lty=i,
       main=tk)
    par(new=T)
    plot(x_c, y_c, t='l', col=i, lty=i, lwd=2)

    coins_up_trend <- c(coins_up_trend, tk)
    coins_up_trend_slope <- c(coins_up_trend_slope, sl)
  }
  abline(cf, lty=2, col='green', lwd=2)
  abline(h=0, lty=i, col=i)
  #par(new=T)
  leg = c(leg, tk)
  colr = c(colr, i)
  ltyp = c(ltyp, i)
  i = i+1
}
legend('topleft', col=colr, legend= leg, lty=ltyp, cex=0.5)
xlabels=format(df$close_time[2:d], "%Y-%m-%d")
axis(1, at=x,labels=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.5, labels = xlabels, srt = 90, pos = 1, xpd = TRUE)

coins_up_trend
coins_up_trend_slope
