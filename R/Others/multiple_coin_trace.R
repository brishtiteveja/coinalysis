library(plotly)
library(quantstrat)
library(xts)

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

# plotting
library(plotly)
library(quantmod)

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

p <- btc_data %>%
  plot_ly(x = ~Date, type=chart_type,
            open = ~Open, 
            close = ~Close,
            high = ~High, 
            low = ~Low,
            name = coins[1]
          ) %>%
  add_trace(x=as.Date(time(btc_sma_7_data$SMA)),
            y=as.double(btc_sma_7_data$SMA), 
            type = 'scatter',
            color = 'green',
            name="BTC_SMA_7") %>%
  add_trace(data=eth_data,x = ~Date, type=chart_type,
            open = ~Open, 
            close = ~Close,
            high = ~High, 
            low = ~Low,  
            increasing=list(line=list(color= 'blue')),
            decreasing=list(line=list(color= 'orange')),
            yaxis='y2', 
            name = coins[2]
          ) %>%
  add_trace(x=as.Date(time(eth_sma_7_data$SMA)),
            y=as.double(eth_sma_7_data$SMA), 
            type = 'scatter',
            color = 'blue',
            yaxis = 'y2',
            name="ETH_SMA_7") %>%
  add_trace(data=xrp_data,x = ~Date, type=chart_type,
            open = ~Open, 
            close = ~Close,
            high = ~High, 
            low = ~Low, 
            increasing=list(line=list(color= 'black')),
            decreasing=list(line=list(color= 'pink')),
            yaxis='y3',
            name = coins[3]
          ) %>%
  add_trace(x=as.Date(time(xrp_sma_7_data$SMA)),
            y=as.double(xrp_sma_7_data$SMA), 
            type = 'scatter',
            color = 'black',
            yaxis = 'y3',
            name="XRP_SMA_7") %>%
  add_trace(data=ltc_data,x = ~Date, type=chart_type,
            open = ~Open,
            close = ~Close,
            high = ~High,
            low = ~Low,
            increasing=list(line=list(color= 'aliceblue')),
            decreasing=list(line=list(color= 'yellow')),
            yaxis='y4', 
            name = coins[4]
          ) %>%
  add_trace(x=as.Date(time(ltc_sma_7_data$SMA)),
            y=as.double(ltc_sma_7_data$SMA), 
            type = 'scatter',
            color = 'aliceblue',
            yaxis = 'y4',
            name="LTC_SMA_7") %>%
  add_trace(data=ada_data,x = ~Date, type=chart_type,
            open = ~Open,
            close = ~Close,
            high = ~High,
            low = ~Low,
            increasing=list(line=list(color= 'violet')),
            decreasing=list(line=list(color= 'skyblue')),
            yaxis='y5',
            name = coins[5]
        ) %>%
  add_trace(x=as.Date(time(ada_sma_7_data$SMA)),
            y=as.double(ada_sma_7_data$SMA), 
            type = 'scatter',
            color = 'violet',
            yaxis = 'y5',
            name="ADA_SMA_7") %>%
  add_trace(data=xlm_data,x = ~Date, type=chart_type,
            open = ~Open,
            close = ~Close,
            high = ~High,
            low = ~Low,
            increasing=list(line=list(color= 'darkred')),
            decreasing=list(line=list(color= 'lime')),
            yaxis='y6',
            name = coins[6]
        ) %>%
  add_trace(x=as.Date(time(xlm_sma_7_data$SMA)),
            y=as.double(xlm_sma_7_data$SMA), 
            type = 'scatter',
            color = 'darkred',
            yaxis = 'y6',
            name="XLM_SMA_7") %>%
  add_trace(data=xmr_data,x = ~Date, type=chart_type,
            open = ~Open,
            close = ~Close,
            high = ~High,
            low = ~Low,
            increasing=list(line=list(color= 'darkorchid')),
            decreasing=list(line=list(color= 'magenta')),
            yaxis='y7',
            name = coins[7]
      ) %>%
  add_trace(x=as.Date(time(xmr_sma_7_data$SMA)),
            y=as.double(xmr_sma_7_data$SMA), 
            type = 'scatter',
            color = 'darkorchild',
            yaxis = 'y7',
            name="XMR_SMA_7") %>%
  layout(title = "Historical Price",
         showlegend = TRUE,
         autosize = TRUE,
         margin = list(
                     l = 50,
                     r = 200,
                     b = 50,
                     t = 50,
                     pad = 2
         ),
         showlegend = FALSE,
         xaxis = list(
            domain = c(0, 0.5),
            autorange = FALSE, 
            range= c(
                    '2017-08-01',
                    '2018-01-31'
            ),
            rangeselector = selectorOptions,
            rangeslider= list(
                  autorange = TRUE,
                  range = c("2009-01-01", "2018-01-31")
            ),
            title = "Date",
            type = "date"
         ),
         yaxis=list(
           title='BTC',
           side='left'
         ),
         yaxis2=list(
           title='ETH',
           anchor = 'x',
           overlaying='y',
           side='right'
         ),
         yaxis3=list(
           title='XRP',
           anchor = 'free',
           overlaying='y',
           side='right',
           position=0.55
         ),
         yaxis4=list(
           title='LTC',
           anchor = 'free',
           overlaying='y',
           side='right',
           position=0.60
         ),
         yaxis5=list(
           title='ADA',
           anchor = 'free',
           overlaying='y',
           side='right',
           position=0.65
         ),
         yaxis6=list(
           title='XLM',
           anchor = 'free',
           overlaying='y',
           side='right',
           position=0.70
         ),
         yaxis7=list(
           title='XMR',
           anchor = 'free',
           overlaying='y',
           side='right',
           position=0.75
         )
  )

p
