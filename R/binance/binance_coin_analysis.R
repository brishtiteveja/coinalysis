library(binancer)
source('config.R')

# Set binance credentials
binance_credentials(API_KEY_BINANCE, secret = SECRET_BINANCE)

binance_coins <- binance_coins()

#---------------------------------------------------------------------------------#
# Check binance balance
#--------------------------------------------------------------------------------#

my_binance_balance <- data.frame(binance_balances(),stringsAsFactors = FALSE)
my_non_zero_binance_balance <- subset(my_binance_balance, total != 0)
d <- my_non_zero_binance_balance
plot(d$total)

# Ordered by decreasing total asset
dn <- cbind(d['asset'],round(d$total))
colnames(dn) <- c('asset', 'total')
dn[order(d['total'], decreasing = TRUE),]

# Scatter plot of assets
library(ggplot2)
ggplot(d, aes(asset, total)) +
        geom_point(aes(color=asset)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Bar plot of assets
ggplot(d, aes(x=asset, y=total)) +
        geom_bar(aes(fill=asset), width=1, stat='identity') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Pie chart of assets
ggplot(d, aes(x="", total)) +
        geom_bar(aes(fill=asset), width=1, stat='identity') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_polar("y", start = 0)

# Percentage/Proportion of assets in Pie chart
d_cum <- cumsum(dn['total'])
d_perc <- dn['total'] / d_cum[nrow(t_cum), ncol(t_cum)] * 100
rows <- order(d_perc['total'], decreasing = TRUE)
d_perc <- data.frame(d_perc[rows,])

df_perc <- cbind(d[rows, 'asset'], d_perc)
colnames(df_perc) <- c('asset', 'total')

blank_theme <- theme_minimal()+
        theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.border = element_blank(),
                panel.grid=element_blank(),
                axis.ticks = element_blank(),
                plot.title=element_text(size=14, face="bold")
        )
library(scales)
ggplot(df_perc[1:5,], aes(x="", total)) +
        geom_bar(aes(fill=asset), width=1, stat='identity') +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        coord_polar("y", start = 0) +
        blank_theme +
        theme(axis.text.x = element_blank())


# ----------------------------------------------------------------------------------#

# Check_binance
b_act <- binance_account()

# Binance coin prices
b_pr <- binance_coins_prices()
b_pr <- binance_coins_prices()

# Binance tickers
b_tck <- data.frame(binancer::binance_ticker_all_prices(), stringsAsFactors = FALSE)

# Group by BTC, ETH, BNB tickers
btc_sym <- grep(".*BTC$", b_tck$symbol )
b_btc_tck <- b_tck[btc_sym,]

eth_sym <- grep(".*ETH$", b_tck$symbol )
b_eth_tck <- b_tck[eth_sym,]

bnb_sym <- grep(".*BNB$", b_tck$symbol)
b_bnb_tck <- b_tck[bnb_sym,]

#plotly bar plot
library(plotly)
p <- plot_ly(
  x = b_eth_tck$from,
  y = b_eth_tck$from_usd,
  name = "",
  type = "bar"
)
p

# Google Sheet Database
library(googlesheets)
sheets <- gs_ls()
b_s <- sheets[sheets$sheet_title == "Binance_Cryptocurrency_Price",]
key <- b_s$sheet_key
g <- gs_key(key)

g <- g %>% gs_ws_new(ws_title = "btc_tickers", input = b_btc_tck, trim = TRUE, verbose = FALSE)
g <- g %>% gs_ws_new(ws_title = "eth_tickers", input = b_eth_tck, trim = TRUE, verbose = FALSE)
g <- g %>% gs_ws_new(ws_title = "bnb_tickers", input = b_bnb_tck, trim = TRUE, verbose = FALSE)


#klines
daily_tck <- list()
i <- 1
tm <- '3m'
for(t in b_tck$symbol) {
   msg <- paste(i, ". Getting Daily data for ticker ", t, ":")
   print(msg)
   daily_tck[[t]] <- data.frame(binance_klines(t, tm), stringsAsFactors = FALSE)
   i <- i + 1
}

# Save the data frame
today <- format(Sys.time(), "%Y-%m-%d")
fn <- paste("BinanceData/Binance_tickers_", tm,"_", today,".Rda", sep="")
save(daily_tck, file=fn)

# Loading the data frame
daily_tck <- load(fn)

i <- 1
for (t in b_tck$symbol) {
   sheet_name <- paste(t, "_daily_tck", sep="")
   msg <- paste(i, ". Writing daily price data into google sheet for ticker", t)
   print(msg)
   i <- i+1
   g <- g %>% gs_ws_new(ws_title = sheet_name, input = daily_tck[[t]], trim = TRUE, verbose = FALSE)
}


# ETH market closing price
eth_tck_w_p <- list()
eth_tck <- data.frame(nrow=14, ncol=length(b_eth_tck$symbol) + 1)

i <- 1
for(t in b_eth_tck$symbol) {
   msg <- paste(i, ". For ticker ", t, ":")
   print(msg)
   tryCatch(
   {
        d <- daily_tck[[t]]$close
        eth_tck_w_p[[t]] <- as.double(rev(rev(d)[1:14]))
        eth_tck <- cbind(eth_tck, eth_tck_w_p[[t]])
   }, error = function(cond){
        print(paste("error", cond))
   }, warning = function(cond) {
        print(paste("warning", cond))
   })

   i <- i + 1
}
t <- 'ETHBTC'
d <- daily_tck[[t]]$close
eth_tck_w_p[[t]] <- as.double(rev(rev(d)[1:14]))
eth_tck <- cbind(eth_tck, eth_tck_w_p[[t]])

colnames(eth_tck) <- c(b_eth_tck$symbol, 'ETHBTC')
eth_tck_m <- eth_tck[, apply(eth_tck, 2, function(x) {!any(is.na(x))})]

# variance
varmat <- apply(eth_tck_m, 2, function(x){var(x)})

sdmat <- apply(eth_tck_m, 2, function(x){sd(x)})
names(sdmat) <- sub("ETH", "", names(sdmat))
barplot(sdmat, cex.names=0.5, las=2)

# correlation matrix
cormat <- cor(eth_tck_m)

cormat_m <- cormat[3:nrow(cormat),3:ncol(cormat)]

library(plotly)
p <- plot_ly(z=cormat_m, type='heatmap')
p

library(DT)
datatable(cormat_m)

# find coins with high and low correlations
corInf <- matrix(,nrow=nrow(cormat_m), 4)
for (i in 1:nrow(cormat_m)) {
    r_name <- rownames(cormat_m)[i]
    r <- cormat_m[i,]
    p <- r[r > 0 & r <= 1]
    p_h <- r[r > 0.5 & r <= 1]
    n <- r[r < 0 & r >= -1]
    n_h <- r[r < -0.5 & r >= -1]

    cl <- c(length(p), length(n), length(p_h), length(n_h))
    corInf <- rbind(corInf, cl)
}
row.has.na <- apply(corInf, 1, function(x){!any(is.na(x))})
corInf <- corInf[row.has.na,]
rownames(corInf) <- gsub("ETH", "", rownames(cormat_m))
colnames(corInf) <- c('positive', 'negative', 'high_positive',
                      'high_negative')
for (i in 1:ncol(corInf)) {
    c_name <- colnames(corInf)[i]
    if (is.na(c_name))
       colnames(corInf)[i] = 'Unknown'
}
for (i in 1:nrow(corInf)) {
        c_name <- rownames(corInf)[i]
        if (is.na(c_name))
                rownames(corInf)[i] = 'Unknown'
}

m <- corInf
m_s_idx <- order(m[,1], decreasing = TRUE)
m <- corInf[m_s_idx,]
barplot(t(m), las=2, space=0.5, cex.names = 0.5)


# Let's train models and predict next price
setwd("~/Documents/projects/crypto/coinalysis/R/binance/BinanceData")
load('Binance_tickers_all_5m_2018-05-07.Rda')
dfp <- tcks[['TRXETH']]

df <- daily_tck[['ETHBTC']]
head(df)
n <- ncol(df)

# Do regression modelling, GAM, GLM, xgboost, random forest, arima modeling , forecasting,
# rnn 
dfp <- tcks[['TRXETH']]
plot(dfp$close_time, dfp$close, t='l')

dfp$close_open <- dfp$close/dfp$open
dfp$low_open <- dfp$low/dfp$open
dfp$high_open <- dfp$high/dfp$open

plot(dfp$close_time, dfp$close_open, t='l')
lines(dfp$close_time, dfp$low_open, col=2)
lines(dfp$close_time, dfp$high_open, col=3)

library(plotly)
library(dplyr)
p <-  plot_ly(x=dfp$close_open, y=dfp$low_open, z=dfp$high_open) %>%
      add_markers() %>%
      layout(scene = list(xaxis = list(title = 'Close/Open'),
                      yaxis = list(title = 'Low/Open'),
                      zaxis = list(title = 'High/Open'),
                      marker = list(size=0.2)))
p


i <- 1
for (c in names(tcks)) {
  tryCatch({
    dfp <- tcks[[c]]
    ts <- ts(dfp$close, start=dfp$close_time[1], frequency=20)
    mf <- sarima(ts, p = 1, q=1, d= 0)
    mf$ttable
    par(mfrow=c(2,1))
    plot(ts, main=paste(i, c, sep='. '))
    sarima.for(ts, n.ahead=20, p=1,q=1, d= 0)
  },
  error = function(cond) {
    print(paste('error', cond))
  }, 
  warning = function(cond) {
    print(paste('warning',cond))
  }, 
  finally = function(cond) {
    print(paste('finally',cond))
  })
  
  i <- i + 1
}

library(quantstrat)
library(ggplot2)
i <- 1
for (c in names(tcks)) {
  tryCatch({
    print(c)
    dfp <- tcks[[c]]
    ts <- ts(dfp$close, start=dfp$close_time[1], frequency=20)

    gf <- ggplot()
    
    y <- SMA(ts, n = 10)
    n <- length(y)
    x <- as.numeric(1:n)
    df <- data.frame(x=x, y=y)
    gf <- ggplot(df, aes(x=x,y=y)) + geom_line()
    
    i = i+1
    y <- SMA(ts, n = 6)
    n <- length(y)
    x <- as.numeric(1:n)
    df <- data.frame(x=x, y=y)
    gf <- gf + geom_line(data=df, aes(x=x,y=y, colour='green')) + ggtitle(c)
    plot(gf)
    sleep(5)
  },
  error = function(cond) {
    print(paste('error', cond))
  }, 
  warning = function(cond) {
    print(paste('warning',cond))
  }, 
  finally = function(cond) {
    print(paste('finally',cond))
  })
  
  i <- i + 1
}

