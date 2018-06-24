library(binancer)
library(xts)
library(zoo)
library(quantstrat)
# tutorial : http://www.r-programming.org/papers
library(quantmod)
library(TTR)
library(tidyquant)
library(tidyverse)
library(timetk)
library(highcharter)

setwd("~/Documents/projects/crypto/coinalysis/R/binance")
source('../binance/Config.R')

plot_ohlc <- function(SYMB, wd=2) {
  par(mfrow=c(1,1))
  plot.new()
  plot(index(SYMB), SYMB$open, t='l', ylab='Price', xlab='Time', lwd=wd)
  lines(index(SYMB), SYMB$close, col='green', lwd=wd)
  lines(index(SYMB), SYMB$high, col='red', lwd=wd)
  lines(index(SYMB), SYMB$low, col='pink', lwd=wd)
  legend("topright", legend=c('open', 'close', 'high', 'low'), lty=1, 
         col=c('black', 'green', 'red', 'pink'), cex=0.75)
}

# S&P data
getSymbols('SPY',src='yahoo',return.class='ts')
# Not getting any dates in SPY

stock_prices <- "SPY" %>%
  tq_get(get  = "stock.prices",
         from = "2007-01-01",
         to   = "2017-01-01")


SYMB <- xts(stock_prices[,c('open', 'high', 'low', 'close')], order.by = stock_prices$date)
plot(SYMB)
plot(index(SYMB),SYMB$close, t='l')

# bitcoin from quandl
bitcoin_quandl <- tribble(
  ~code,          ~symbol,
  "BCHARTS/LOCALBTCEUR", "LOCALBTCEUR"
)

bitcoin_quandl %>% tq_get(get = "quandl") %>% 
      select(date, open, high, low, close) -> BTC_SYMB

df <- data.frame(BTC_SYMB)
head(df)
SYMB <- xts(df[,2:5], order.by = df$date)
SYMB <- SYMB["2017-05-31/2017-12-31"]
plot(index(SYMB), rowMeans(HLC(SYMB)), t='l')
plot(Cl(SYMB))

# Daily bitcoin returns, just using lagging
bitcoin_quandl %>% tq_get(get='quandl') %>%
  select(date, close) %>%
  mutate(lag = lag(close), returns = (log(close) - log(lag(close))))%>%
  replace_na(list(returns = 0)) %>%
  mutate_at(vars(3), funs(ifelse(!is.finite(.), 0, .))) %>% 
  tk_xts() -> bitcoin_return

highchart(type = "stock") %>% 
  hc_title(text = "BitCoin Daily Returns/Prices") %>%
  hc_yAxis_multiples(
    list(lineWidth = 3),
    list(showLastLabel = FALSE, opposite = TRUE)) %>% 
  hc_add_series(bitcoin_return$close, 
                name = names(bitcoin_return$close)) %>%
  hc_add_series(bitcoin_return$returns,
                name = names(bitcoin_return$returns), yAxis = 1) %>% 
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE)

# get 1m downloaded data for 3 months
binance_data_dir <- "~/Documents/projects/crypto/coinalysis/R/binance/BinancePickleData"
csv_data_file <- "BTCUSDT_5m_from_2018-3-1_to_2018-6-14.csv"

df <- read.table(paste(binance_data_dir, csv_data_file, sep="/"), 
                   header = TRUE, sep=",", row.names = NULL, stringsAsFactors = FALSE)
colnames(df) <- c('time', 'close', 'high', 'low', 'open')
BTC_SYMB <- xts(df[,2:5], order.by = as.POSIXct(df$time))

SYMB <- BTC_SYMB
SYMB <- BTC_SYMB["2018-03-20 09:00:00/2018-03-20 20:00:00"]
#plot_ohlc(SYMB["2018-05-22 21:22:00/2018-05-29 21:22:00"], wd=2)
chartSeries(SYMB, type='bars')

# with plotly
df <- data.frame(SYMB)
rownames(df) <- 1:dim(df)[1]
df$time <- as.POSIXct(index(SYMB))
head(df)
p <- df %>%
  plot_ly(x = ~time, type="ohlc",
          open = ~open, close = ~close,
          high = ~high, low = ~low) %>%
  layout(title = "BTC-USD OHLC Chart",
       xaxis = list(rangeslider = list(visible = F)))
p

# with high chart
highchart(type='stock') %>% 
  hc_title(text = "BTC-USD Price") %>%
  hc_yAxis_multiples(
    list(lineWidth = 3),
    list(showLastLabel = FALSE, opposite = TRUE)) %>% 
  hc_add_series(df, 
                type='candlestick') %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE)

# Gdax bitcoin price data
gdax_btc_price_fn <- '/Users/andy/Documents/projects/crypto/coinalysis/python/btc_usd_gdax_price.csv'
df <- read.table(gdax_btc_price_fn, sep=',', header = TRUE)
tz <- Sys.timezone()
df$time <- as.POSIXct(df$time, tz='US/Central')
head(df)
BTC_SYMB <- xts(df[,2:5], order.by = df$time)
plot(BTC_SYMB)
SYMB <- BTC_SYMB

# Binance ticker data collection
t <- 'BTCUSDT'
tm <- '1d'
tkr <- binance_klines(t, interval=tm)
df <- data.frame(tkr[,c('close_time', 'open', 'high', 'low', 'close', 'volume', 
                        'quote_asset_volume', 'trades', 'taker_buy_base_asset_volume',
                        'taker_buy_quote_asset_volume')], stringsAsFactors = FALSE)
BTC_SYMB <- xts(df[,2:10], order.by = as.POSIXct(df$close_time))
plot_ohlc(BTC_SYMB)
SYMB <- BTC_SYMB

# Visualize technical indicators
SYMB <- BTC_SYMB["2018-06-18/2018-06-19"]
chartSeries(SYMB, type="bars")

# RSI: Relative Strength Index
rsi <- RSI(rowMeans(HLC(SYMB)), n = 7)
addRSI(n=14)

# MACD: Moving Average Convergence Divergence
macd <- MACD(rowMeans(HLC(SYMB)))
addMACD()

# CCI : Commodity Channel Index
addCCI()

# Bolinger bands
addBBands()
bb <- BBands(HLC(SYMB))

# DVO ta
dvo <- DVO(HLC(SYMB))
dvots <- xts(dvo, order.by = index(SYMB))
addTA(dvots, col=6, lty=1, lwd=2)

# chaikin volatility
ad <- chaikinAD(df[,c("high","low","close")], df[,"volume"])
adts <- xts(ad, order.by = df$close_time)
addTA(ta = ad, col='red')
plot(ad, t='l')
addChAD()

# chaikin money flow
cmf <- CMF(df[,c("high","low","close")], df[,"volume"])
cmfts <- xts(cmf, order.by = df$close_time)
addTA(cmfts)

# Chaikin Accumulation / Distribution (AD) line
chaikAD <- chaikinAD(df[,c("high","low","close")], df[,"volume"])
chaikADts <- xts(chaikAD, order.by = index(SYMB))
# chaikin volatility
addTA(chaikADts, col = 'red')

# Chaikin Volatility
chaikVol <- chaikinVolatility(df[,c("high","low","close")], df[,"volume"])
chaikVolts <- xts(chaikVol, order.by = index(SYMB))
addTA(chaikVolts)

# Create initdate, from, and to strings
initdate <- "2018-06-04"
from <- "2018-06-04"
to <- "2019-06-30"

# Set the timezone to UTC
Sys.setenv(TZ = "UTC")

# Set the currency to USD
curr <- 'USD'
currency(curr)

# Load the quantmod package
library(quantmod)

# Use stock() to initialize SPY and set currency to USD
stock("SYMB", currency=curr)

# Define your trade size and initial equity
tradesize <- 100
initeq <- 100

# Define the names of your strategy, portfolio and account
strategy.st <- "cryptostrat"
portfolio.st <- "cryptostrat"
account.st <- "cryptostrat"

# Remove the existing strategy if it exists
#rm.strat(strategy.st)

# Initialize the portfolio
initPortf(portfolio.st, symbols = "SYMB", initDate = initdate,
          currency = curr)

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = curr, initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
strategy(strategy.st, store = TRUE)

## Technical indicators
# Create a 60-min SMA
symb_sma <- SMA(x = Cl(SYMB), n = 60)

# Create an RSI with a 3-day lookback period
symb_rsi <- RSI(price = Cl(SYMB), n = 3)

# Plot the closing prices of SPY
plot(Cl(SYMB))

# Overlay a 15-min SMA
lines(SMA(Cl(SYMB), n = 15), col = 'red')
# Overlay a 5-min SMA
lines(SMA(Cl(SYMB), n = 2), col = 'orange')
# Overlay a 5-min SMA
lines(SMA(Cl(SYMB), n = 5), col = 'blue')
# Overlay a 30-min SMA
lines(SMA(Cl(SYMB), n = 30), col = 'green')
# Overlay a 60-min SMA
lines(SMA(Cl(SYMB), n = 60), col = 'pink')

# Hull Moving Average
lines(HMA(Cl(SYMB), n = 2), col='red')

#Hull Moving Average
HMA_m <- function(src, length) {
        #HMA = WMA(2*WMA(n/2) − WMA(n)),sqrt(n))
        half_wma <- 2 * WMA(src, length / 2)
        full_wma <- WMA(src, length)
        n <- round(sqrt(length))
        res <- WMA(half_wma - full_wma, n)
        return(res)
}
p <- Cl(SYMB)
plot(time(p),p$close, type='l')
lines(time(p), HMA_m(Cl(SYMB), 2), col='brown')
lines(time(p), HMA_m(Cl(SYMB), 4), col='brown')
lines(time(p), HMA_m(Cl(SYMB), 8), col='red')

# The function for computing the Ichimoku cloud
ichimoku <- function(data,pars) {

        # REMEMBER THAT THE DATA SHOULD BE IN ORDER
        #
        # HIGH, LOW and CLOSE
        #
        # ==========================================

        # Number of observations
        Nobs <- NROW(data)

        # Get the three parameters
        p1 <- pars[1]
        p2 <- pars[2]
        p3 <- pars[3]

        # The maximum of these should be p3, check
        if ((p1 > p2) | (p1 > p3) | (p2 > p3))
        {
                stop("parameters should enter in ascending order")
        }
        # Set the max
        maxp <- p3

        # You will leave out maxp observations
        cloud.lines <- matrix(0,nrow=Nobs-maxp,ncol=5)
        colnames(cloud.lines) <- c("Tenkan","Kijun", "SenkouA", "SenkouB", "Chikou")

        # Run a loop to make the computations
        for (i in seq(maxp+1,Nobs,1))
        {
                # Compute the cloud lines
                tenkan <- (max(data[seq(i-p1,i,1),1])+min(data[seq(i-p1,i,1),2]))/2
                kijun <- (max(data[seq(i-p2,i,1),1])+min(data[seq(i-p2,i,1),2]))/2
                senkouA<- (tenkan+kijun)/2
                senkouB<- (max(data[seq(i-p3,i,1),1])+min(data[seq(i-p3,i,1),2]))/2
                chikou <- data[i,3]

                # Save in appropriate places
                cloud.lines[(i-maxp),] <- c(tenkan,kijun,senkouA,senkouB,chikou)
        }

        # OK, now align them correctly: SenkouA and SenkouB are moved p2 periods forward
        # while Chikou is moved p2 periods backward…
        A1 <- rbind(cloud.lines[,1:2],matrix(NA,p2,2))
        A2 <- rbind(matrix(NA,p2,2),cloud.lines[,3:4])
        A3 <- c(cloud.lines[(p2+1):(Nobs-maxp),5],matrix(NA,2*p2,1))
        new.cloud.lines <- cbind(A1,A2,A3)
        colnames(new.cloud.lines) <- colnames(cloud.lines)

        # Align the data as well
        new.data <- rbind(data[(maxp+1):Nobs,],matrix(NA,p2,3))
        colnames(new.data) <- colnames(data)

        # OK, return everything
        return(list(data=new.data,cloud.lines=new.cloud.lines))
}

ichimoku <- function(HLC, nFast=9, nMed=26, nSlow=52) {
  turningLine <- (runMax(Hi(HLC), nFast)+runMin(Lo(HLC), nFast))/2
  baseLine <- (runMax(Hi(HLC), nMed)+runMin(Lo(HLC), nMed))/2
  spanA <- lag((turningLine+baseLine)/2, nMed)
  spanB <- lag((runMax(Hi(HLC), nSlow)+runMin(Lo(HLC), nSlow))/2, nMed)
  plotSpan <- lag(Cl(HLC), -nMed) #for plotting the original Ichimoku only
  laggingSpan <- lag(Cl(HLC), nMed)
  lagSpanA <- lag(spanA, nMed)
  lagSpanB <- lag(spanB, nMed)
  out <- cbind(turnLine=turningLine, baseLine=baseLine, spanA=spanA, spanB=spanB, plotSpan=plotSpan, laggingSpan=laggingSpan, lagSpanA, lagSpanB)
  colnames(out) <- c("turnLine", "baseLine", "spanA", "spanB", "plotLagSpan", "laggingSpan", "lagSpanA","lagSpanB")
  return (out)
}

getLastHighestSupport <- function(x) {
  support <- getSupport(x)
  ns <- dim(support)[1]
  
  resi <- rep(0, ns)
  resv <- rep(0, ns)
  
  maxv <- -Inf
  maxi <- -Inf
  for(i in 1:ns) {
    if (support$val[i] > maxv) {
      maxi <- support$id[i]
      maxv <- support$val[i]
    }
    resi[i] <- maxi
    resv[i] <- maxv
  }
  
  lhs <- data.frame(id=resi, val=resv)
  
  return(lhs)
}

# Set the ichimoku parameters
plot(HLC(SYMB), legend.loc = 'bottomleft', cex=0.4)
require(quantmod)
#install_github('IlyaKipnis/IKTrading')
#require(IKTrading)
plot_ichimoku <- function(df) {
  require(plotly)
  require(dplyr)
  
  plot_ly(data = df,
          x = ~date,
          open = ~open, high = ~high, low = ~low, close = ~close,
          type = 'ohlc') %>%
    add_trace(y = ~turnLine,
              type = 'scatter', mode = 'lines',
              name = 'turn line',
              line = list(color='blue')
    ) %>%
    add_trace(y = ~baseLine,
              type = 'scatter', mode = 'lines',
              name = 'base line', 
              line = list(color='red')
    ) %>%
    add_trace(y = ~spanA,
              type = 'scatter', mode = 'lines',
              name = 'spanA',
              line = list(color='red')
    ) %>%
    add_trace(y = ~spanB,
              type = 'scatter', mode = 'lines',
              name = 'spanB',
              line = list(color='green'),
              fill = 'tonexty'
    ) %>%
    add_trace(y = ~laggingSpan,
              type = 'scatter', mode = 'lines',
              name = 'lagging span',
              line = list(color=rgb(0.5,1,0,0.5))
    ) %>%
    add_trace(y = ~lagSpanA,
              type = 'scatter', mode = 'lines',
              name = 'lag span A',
              line = list(color=rgb(0.25,0,0.5,0.5))
    ) %>%
    add_trace(y = ~lagSpanB,
              type = 'scatter', mode = 'lines',
              name = 'lag span B',
              line = list(color=rgb(0.5,0,0.25,0.5))
    )
}

ibm_ic = ichimoku(HLC(SYMB), nFast = 2, nSlow = 14, nMed = 7)
plot(ibm_ic, legend.loc = 'bottomleft', cex=0.4)
df <- data.frame(ibm_ic)
df$open <- as.numeric(unlist(OHLC(SYMB)$open))
df$high <- as.numeric(unlist(OHLC(SYMB)$high))
df$low <- as.numeric(unlist(OHLC(SYMB)$low))
df$close <- as.numeric(unlist(OHLC(SYMB)$close))
df$date <- as.POSIXct(rownames(df))

# plot ichimoku cloud
plot_ichimoku(df)

t <- time(ibm_ic)
polygon(c(t,rev(t)), c(ibm_ic$spanA, rev(ibm_ic$spanB), col=rgb(1, 0, 0,0.5), border=NA))
# What kind of indicator?
"trend"

SYMB <- SYMB["2018-05-20 21:22:00/2018-06-04 21:22:00"]
# when the short-MA crosses long-MA, then is the time to buy

# Plot the closing price of SPY
plot(Cl(SYMB))

s <- SYMB["2015-08-01/2015-11-01"]
plot_ly(data.frame(s), type='ohlc')
plot(DVO(s, navg = 2), col='red')

# Plot the RSI 2
plot(RSI(Cl(SYMB), n = 2), col='red')
plot(DVO(SYMB, navg = 2), col='red')

# What kind of indicator?
"reversion"

# Add a 5-min SMA indicator to strategy.st
add.indicator(strategy = strategy.st,

              # Add the SMA function
              name = "SMA",

              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 5),

              # Label your indicator SMA200
              label = "SMA5")

# Add a 15-min SMA indicator to strategy.st
add.indicator(strategy = strategy.st,

              # Add the SMA function
              name = "SMA",

              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 15),

              # Label your indicator SMA200
              label = "SMA15")


# Add a 60-min SMA indicator to strategy.st
add.indicator(strategy = strategy.st,

              # Add the SMA function
              name = "SMA",

              # Create a lookback period
              arguments = list(x=quote(Cl(mktdata)), n=60),

              # Label your indicator SMA60
              label = "SMA60")

# Add an RSI 3 indicator to strategy.st
add.indicator(strategy = strategy.st,

              # Add the RSI 3 function
              name = "RSI",

              # Create a lookback period
              arguments = list(price = quote(Cl(mktdata)), n = 3),

              # Label your indicator RSI_3
              label = "RSI_3")

# Write the RSI_avg function
RSI_avg <- function(price, n1, n2) {

        # RSI 1 takes an input of the price and n1
        rsi_1 <- RSI(price = price, n = n1)

        # RSI 2 takes an input of the price and n2
        rsi_2 <- RSI(price = price, n = n2)

        # RSI_avg is the average of rsi_1 and rsi_2
        RSI_avg <- (rsi_1 + rsi_2)/2

        # Your output of RSI_avg needs a column name of RSI_avg
        colnames(RSI_avg) <- "RSI_avg"
        return(RSI_avg)
}

# Add this function as RSI_3_4 to your strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, name="RSI_avg", arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")

# Declare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 12) {

        # Compute the ratio between closing prices to the average of high and low
        ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)

        # Smooth out the ratio outputs using a moving average
        avgratio <- SMA(ratio, n = navg)

        # Convert ratio into a 0-100 value using runPercentRank()
        out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
        colnames(out) <- "DVO"
        return(out)
}

add.indicator(strategy = strategy.st, name = "DVO",
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 12),
              label = "DVO_2_12")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SYMB))
test_subset <- test["2018-02-22 18:58:59/2018-02-22 19:11:59"]


# Add a sigComparison which specifies that SMA15 must be greater than SMA60, call it longfilter
add.signal(strategy.st, name = "sigComparison",

           # We are interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA5", "SMA15"),

                            # Particularly, we are interested when the SMA15 is greater than the SMA60
                            relationship = "gt"),

           # Label this signal longfilter
           label = "longfilter")

# Add a sigCrossover which specifies that the SMA15 is less than the SMA60 and label it filterexit
add.signal(strategy.st, name = "sigCrossover",

           # We're interested in the relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA5", "SMA15"),

                            # The relationship is that the SMA50 crosses under the SMA200
                            relationship = "lt"),

           # Label it filterexit
           label = "filterexit")

# Implement a sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
add.signal(strategy.st, name = "sigThreshold",

           # Use the DVO_2_126 column
           arguments = list(column = "DVO_2_12",

                            # The threshold is 20
                            threshold = 20,

                            # We want the oscillator to be under this value
                            relationship = "lt",

                            # We're interested in every instance that the oscillator is less than 20
                            cross = FALSE),

           # Label it longthreshold
           label = "longthreshold")

# Add a sigThreshold signal to your strategy that specifies that DVO_2_12 must cross above 80 and label it thresholdexit
add.signal(strategy.st, name = "sigThreshold",

           # Reference the column of DVO_2_12
           arguments = list(column = "DVO_2_6",

                            # Set a threshold of 80
                            threshold = 80,

                            # The oscillator must be greater than 80
                            relationship = "gt",

                            # We are interested only in the cross
                            cross = TRUE),

           # Label it thresholdexit
           label = "thresholdexit")

test_init <- applyIndicators(strategy.st, mktdata = OHLC(SYMB))
test <- applySignals(strategy = strategy.st, mktdata = test_init)

# Add a sigFormula signal to your code specifying that both longfilter and longthreshold must be TRUE, label it longentry
add.signal(strategy.st, name = "sigFormula",

           # Specify that longfilter and longthreshold must be TRUE
           arguments = list(formula = "longfilter & longthreshold",

                            # Specify that cross must be TRUE
                            cross = TRUE),

           # Label it longentry
           label = "longentry")

# Fill in the rule's type as exit
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all",
                          ordertype = "market", orderside = "long",
                          replace = FALSE, prefer = "Open"),
         type = "exit")


# Create an entry rule of 1 share when all conditions line up to enter into a position
add.rule(strategy.st, name = "ruleSignal",

         # Use the longentry column as the sigcol
         arguments=list(sigcol = "longentry",
                        # Set sigval to TRUE
                        sigval = TRUE,
                        # Set orderqty to 1
                        orderqty = 1,
                        # Use a market type of order
                        ordertype = "market",
                        # Take the long orderside
                        orderside = "long",
                        # Do not replace other signals
                        replace = FALSE,
                        # Buy at the next day's opening price
                        prefer = "Open"),
         # This is an enter type rule, not an exit
         type = "enter")


out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update your portfolio (portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update your account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

# What is the date of the last trade?
"YYYY-MM-DD"

# Get the tradeStats for your portfolio
tstats <- tradeStats(Portfolios = portfolio.st)

# Print the profit factor
tstats$Profit.Factor

# Use chart.Posn to view your system's performance on SPY
chart.Posn(Portfolio = portfolio.st, Symbol = "SYMB")

sma5 = SMA(Cl(SYMB), n=5)
# Overlay the SMA50 on your plot as a blue line
add_TA(sma5, on = 1, col = "green")

sma15 = SMA(Cl(SYMB), n=15)
# Overlay the SMA60 on your plot as a red line
add_TA(sma15, on = 1, col = "blue")

dvo <- DVO(HLC = HLC(SYMB), navg = 2, percentlookback = 12)

# Add the DVO_2_126 to the plot in a new window
add_TA(dvo)

# Cash Sharpe Ratio
portpl <- .blotter$portfolio.cryptostrat$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric=FALSE)

# Get instrument returns
instrets <- PortfReturns(portfolio.st)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)

