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

# Binance ticker data collection
t <- 'BTCUSDT'

tm_list <- c("5m", "15m", "1h","6h", "12h", "1d") # "3d", "1w", "1M")
tm <- '1d'
tm_list <- '1d'

SMA_12 <- list()
SMA_26 <- list()
RSI <- list()
MACD <- list()
CCI <- list()
DPO <- list()

SYMBL <- list()
for(tm in tm_list) {
  print(paste("Processing for interval ", tm))
  tkr <- binance_klines(t, interval=tm)
  df <- data.frame(tkr[,c('close_time', 'open', 'high', 'low', 'close', 'volume', 
                          'quote_asset_volume', 'trades', 'taker_buy_base_asset_volume',
                          'taker_buy_quote_asset_volume')], stringsAsFactors = FALSE)
  SYMB <- xts(df[,2:10], order.by = as.POSIXct(df$close_time))
  
  chartSeries(SYMB, type='bars', name=tm)
  MACD[[tm]] <- MACD(rowMeans(HLC(SYMB)))
  addMACD()
 
  SMA_12[[tm]] <- SMA(rowMeans(HLC(SYMB)), n=12)
  addSMA(n=12, col='red')
  
  SMA_26[[tm]] <- SMA(rowMeans(HLC(SYMB)), n = 26)
  addSMA(n=26, col='grey')
  
  CCI[[tm]] <- CCI(HLC(SYMB))
  addCCI()
  
  RSI <- RSI(rowMeans(HLC(SYMB)))
  addRSI()
  
  DPO[[tm]] <- DPO(rowMeans(HLC(SYMB)))
  addDPO()
  
  SYMBL[[tm]] <- SYMB
    
  Sys.sleep(5)
}


SYMB <- SYMBL[[tm]]

# Define the names of your strategy, portfolio and account
account.st <- "ananda"
portfolio.st <- "BTCUSDT"
strategy.st <- "test_strat"
rm.strat(strategy.st)


# Set the timezone to UTC
Sys.setenv(TZ = "UTC")

curr <- 'USD'
currency(curr)

stock("SYMB", currency=curr)

# Create initdate, from, and to strings
initdate <- format(min(index(SYMB)), "%Y-%m-%d")
from <- format(Sys.time(), "%Y-%m-%d")
to <- format(Sys.time() + 60 * 60 * 24 * 60, "%Y-%m-%d")

# Define your trade size and initial equity
tradesize <- 1000
initeq <- 10000

# initialize the account
initAcct(account.st, portfolios = c(portfolio.st), initDate = initdate, 
         initEq = initeq, currency = currency)

# Initialize the portfolio
initPortf(portfolio.st, symbols = "SYMB", initDate = initdate,
          currency = curr)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
rm.strat(strategy.st)
strategy(strategy.st, store = TRUE)

# Add a SMA (n=12) indicator to strategy.st
add.indicator(strategy = strategy.st,
              # Add the SMA function
              name = "SMA",
              # Create a lookback period
              arguments = list(x = quote(
                xts(rowMeans(HLC(mktdata)), order.by = index(mktdata)))
                               , n = 12),
              # Label your indicator SMA200
              label = "SMA12")

# Add a 26-min SMA indicator to strategy.st
add.indicator(strategy = strategy.st,
              name = "SMA",
              # Create a lookback period
              arguments = list(x = quote(
                xts(rowMeans(HLC(mktdata)), order.by = index(mktdata)))
                               , n = 26),
              # Label your indicator SMA200
              label = "SMA26")


add.indicator(strategy = strategy.st,
              name = "CCI",
              arguments = list(HLC = quote(HLC(mktdata))),
              label = "CCI")

add.indicator(strategy = strategy.st,
              name = "MACD",
              arguments = list(x = quote(
                xts(rowMeans(HLC(mktdata)), order.by = index(mktdata)))),
              label = "MACD")

add.indicator(strategy = strategy.st,
              name = "RSI",
              arguments = list(price = quote(
                xts(rowMeans(HLC(mktdata)), order.by = index(mktdata)))),
              label = "RSI")
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
add.indicator(strategy = strategy.st,
              name = "DVO",
              arguments = list(HLC = quote(HLC(mktdata))),
              label = "DVO")


strat <- getStrategy(strategy.st)
summary(strat)

ind_state <- applyIndicators(strategy = strategy.st, mktdata = SYMB)

add.signal(strategy.st, name = "sigComparison",
           # We are interested in the relationship between the SMA12 and the SMA26
           arguments = list(columns = c("SMA12", "SMA26"),
                            # When SMA12 is greater than the SMA26
                            relationship = "gt"),
           label = "smacomp")

add.signal(strategy.st, name = "sigCrossover",
           # We are interested in the relationship between the SMA12 and the SMA26
           arguments = list(columns = c("SMA12", "SMA26"),
                            # When the SMA12 is going above the SMA26, time to enter
                            relationship = "gt"),
           # Label this signal longfilter
           label = "smaenter")

# Add a sigCrossover which specifies that the SMA12 is less than the SMA26 and label it filterexit
add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns = c("SMA12", "SMA26"),
                            relationship = "lt"),
           label = "smaexit")

add.signal(strategy.st, name = "sigThreshold",
           arguments = list(column = "CCI",
                            # The threshold is 20
                            threshold = -100,
                            # We want the oscillator to be above this value
                            relationship = "gt",
                            # We're interested in every instance that the oscillator is greater than 0
                            cross = TRUE),
           label = "cci_above_neg_100_enter")

add.signal(strategy.st, name = "sigThreshold",
           arguments = list(column = "CCI",
                            # The threshold is 20
                            threshold = 0,
                            relationship = "lt",
                            # We're interested in every instance that the oscillator is less than 0
                            cross = TRUE),
           label = "ccibaseexit")
           

sig_state <- applySignals(strategy = strategy.st, mktdata = ind_state)

# exit rule
# initially only with sma
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "smaexit", 
                          sigval = TRUE, 
                          orderqty = "all",
                          ordertype = "market", 
                          orderside = "long",
                          replace = FALSE, prefer = "Open"),
         type = "exit")


# enter rule
add.rule(strategy.st, name = "ruleSignal",
         arguments=list(sigcol = "smaenter",
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
#daterange

# Update your account (account.st)
updateAcct(account.st, daterange)
updateEndEq(account.st)

# What is the date of the last trade?
"YYYY-MM-DD"

# Get the tradeStats for your portfolio
tstats <- tradeStats(Portfolios = portfolio.st)
tstats

# Print the profit factor
tstats$Profit.Factor

# create custom theme
myTheme<-chart_theme()
myTheme$col$dn.col<-'lightblue'
myTheme$col$dn.border <- 'lightgray'
myTheme$col$up.border <- 'lightgray'
# Use chart.Posn to view your system's performance on SPY
chart.Posn(Portfolio = portfolio.st, Symbol = "SYMB", theme=myTheme
, TA='add_SMA(n=12,col=4, on=1, lwd=2)')

