library(binancer)
source('../binance/Config.R')

t <- 'DGDETH'

tm <- '1m'
tkr <- binance_klines(t, interval=tm)
df <- data.frame(tkr[,c('close_time', 'open', 'high', 'low', 'close', 'volume')], stringsAsFactors = FALSE)

library(xts)
SYMB <- xts(df[,c('open', 'high', 'low', 'close', 'volume')], order.by = df$close_time)
plot(index(SYMB),SYMB$close, t='l')

# Load the quantstrat package
library(quantstrat)

# Create initdate, from, and to strings
initdate <- "2018-02-22"
from <- "2018-02-22"
to <- "2019-02-22"

# Set the timezone to UTC
Sys.setenv(TZ = "UTC")

# Set the currency to USD
curr <- 'ETH'
currency(curr)

# Load the quantmod package
library(quantmod)

# Use stock() to initialize SPY and set currency to USD
stock("SYMB", currency="ETH")

# Define your trade size and initial equity
tradesize <- 0.1
initeq <- 0.1

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
lines(SMA(Cl(SYMB), n = 5), col = 'blue')
# Overlay a 30-min SMA
lines(SMA(Cl(SYMB), n = 30), col = 'green')
# Overlay a 60-min SMA
lines(SMA(Cl(SYMB), n = 60), col = 'pink')

# What kind of indicator?
"trend"

# when the short-MA crosses long-MA, then is the time to buy

# Plot the closing price of SPY
plot(Cl(SYMB))

# Plot the RSI 2
plot(RSI(Cl(SYMB), n = 2), col='red')

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


