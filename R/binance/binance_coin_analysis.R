library(binancer)
source('config.R')

# Set binance credentials
binance_credentials(API_KEY_BINANCE, secret = SECRET_BINANCE)

binance_coins <- binance_coins()

# Check binance balance
my_binance_balance <- data.frame(binance_balances(),stringsAsFactors = FALSE)
my_non_zero_binance_balance <- subset(my_binance_balance, total != 0)
d <- my_non_zero_binance_balance
plot(d$total)

# Check_binance
b_act <- binance_account()

# Binance coin prices
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
for(t in b_tck$symbol) {
   msg <- paste(i, ". Getting Daily data for ticker ", t, ":")
   print(msg)
   daily_tck[[t]] <- data.frame(get_klines(t, "1d"), stringsAsFactors = FALSE)
   i <- i + 1
}

i <- 1
for (t in b_tck$symbol) {
   sheet_name <- paste(t, "_daily_tck", sep="")
   msg <- paste(i, ". Writing daily price data into google sheet for ticker", t)
   print(msg)
   i <- i+1
   g <- g %>% gs_ws_new(ws_title = sheet_name, input = daily_tck[[t]], trim = TRUE, verbose = FALSE)
}

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
cormat <- cor(eth_tck_m)

cormat_m <- cormat[3:nrow(cormat),3:ncol(cormat)]

library(plotly)
p <- plot_ly(z=cormat_m, type='heatmap')
p

library(DT)
datatable(cormat_m)
