library(quantstrat)

#Tutorials
# http://r.789695.n4.nabble.com/quantstrat-newbie-indicators-example-td4662247.html
# Momentum in R
# https://www.r-bloggers.com/author/rbresearch/
# https://github.com/rossb34/Ranking

# Create initdate, from, and to strings
initdate <- "2018-02-20"
from <- "2018-02-20"
to <- "2019-02-20"

# Set the timezone to UTC
Sys.setenv(TZ = "UTC")

# Set the currency to USD 
curr <- 'ETH'
currency(curr)

symbols <- 'ADX'

# Creating xts time series
library(binancer)
dat <- binance_klines('ADXETH')
ADX <- xts(dat[,c('open', 'high', 'low', 'close', 'volume')], order.by = dat$open_time)
colnames(ADX) <- c('ADX.Open', 'ADX.high', 'ADX.Low', 'ADX.Close', 'ADX.Volume')
SYMB <- ADX

stock(symbols, currency="ETH")

# Setting up strategy
initeq <- 0.1
tradesize <- 0.1

# Define the names of your strategy, portfolio and account
strategy.st <- "cryptostrat"
portfolio.st <- "cryptostrat"
account.st <- "cryptostrat"

# Initialize the portfolio
initPortf(portfolio.st, symbols = symbols, 
          initDate = initdate, currency = curr)

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, 
         initDate = initdate, currency = curr, initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
strategy(strategy.st, store = TRUE)

# Create a 200-minute SMA
symb_sma <- SMA(x = Cl(SYMB), n = 200)

# Create an RSI with a 3-minute lookback period
symb_rsi <- RSI(price = Cl(SYMB), n = 3)

# Plot the closing prices of SPY
plot(Cl(SYMB))

# Overlay a 60-minute SMA
lines(SMA(Cl(SYMB), n = 60), col = 'red')

# Plot the closing price of SPY
plot(Cl(SYMB))

# Plot the RSI 2
plot(RSI(Cl(SYMB), n = 2), col='red')

# Add a 60-min(1hr) SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "SMA", 
              
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 60), 
              
              # Label your indicator SMA60
              label = "SMA60")

# Add a 30-min SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "SMA", 
              
              # Create a lookback period
              arguments = list(x=quote(Cl(mktdata)), n=30), 
              
              # Label your indicator SMA30
              label = "SMA30")

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
DVO <- function(HLC, navg = 2, percentlookback = 15) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 15),
              label = "DVO_2_15")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SYMB))
