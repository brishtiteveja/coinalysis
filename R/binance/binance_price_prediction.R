library(binancer)
source('../binance/Config.R')

t <- 'BTCUSDT'

tm <- '1m'
tkr <- binance_klines(t, interval=tm)
df <- data.frame(tkr[,c('close_time', 'open', 'high', 'low', 'close', 'volume')], stringsAsFactors = FALSE)

library(plotly)
library(ranger)



library(plotly)

timeseries_orig <- c()
time <- c()
old_df <- data.frame()

library(data.table)
clock <- 0
while(1) {
  tmp_df <- rbind(old_df, df)
  dim(tmp_df)
  
  df <- unique(tmp_df, by=close_time)
  dim(df)
  
  time = c(as.POSIXct(df$close_time), time)
  timeseries_orig = c(as.numeric(df$close), timeseries_orig)
  
  timeseries = timeseries_orig #[1:as.integer(tL* 0.80)]
  
  #time = dfp$close_time
  par(mfrow=c(1,1))
  plot(time, timeseries, t='l')
  
  TRAIN_SIZE = (30/5)*2
  TARGET_TIME = 1
  LAG_SIZE = 1
  
  res <- process_time_series_data(TRAIN_SIZE, TARGET_TIME, LAG_SIZE, scale = TRUE, percentage = 0.97)
  X_train <- res$X_train
  X_train_orig <- res$X_train_orig
  X_test <- res$X_test
  Y_train <- res$Y_train
  Y_train_orig <- res$Y_train_orig
  Y_test <- res$Y_test
  
  X_train <- matrix(as.numeric(unlist(X_train)), ncol=TRAIN_SIZE, byrow=TRUE)
  X_test <- matrix(as.numeric(unlist(X_test)), ncol=TRAIN_SIZE, byrow=TRUE)
  Y_train <- matrix(as.numeric(unlist(Y_train)), ncol=TARGET_TIME, byrow=TRUE)
  Y_test <- matrix(as.numeric(unlist(Y_test)), ncol=TARGET_TIME, byrow=TRUE)
  
  ny <- dim(X_train)[2]
  
  # Model using random forest
  #seed
  nc <- ncol(X_train)
  vars <- c()
  for(i in 1:nc) {
    cn <- paste("Lag", as.character(nc - i + 1), sep="")
    vars <- c(vars, cn)
  }
  
  X_train_rf <- cbind(X_train, Y_train)
  outcome <- 'Price'
  colnames(X_train_rf) <- c(vars, outcome)
  head(X_train_rf)
  
  (fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))
  
  set.seed(12345)
  (model_rf <- ranger(fmla, # formula 
                      data= data.frame(X_train_rf), # data
                      num.trees = 500, 
                      respect.unordered.factors = "order"))
  model_rf
  
  X_test_rf <- cbind(X_test)
  colnames(X_test_rf) <- c(vars)
  head(X_test_rf)
  X_pred_rf <- predict(model_rf, X_test_rf)$predictions
  np <- length(X_pred_rf)
  
  nt <- length(time)
  s <- (nt-np + 1)
  e <- s + np -1
  t <- time[s:e]
  length(t)
  
  model <- model_rf
  predict_continuous_rf <- function(model, X_train_orig, nx, ny, vars ) {
    s <- length(X_train_orig)
    X_test_new_list <- X_train_orig[[s]]
    X_test_new <- matrix(unlist(X_test_new_list), nrow=1, ncol=ny)
    colnames(X_test_new) <- c(vars)
    
    X_pred_cont <- c()
    for (i in 1:nx) {
      #print(X_test_new_list)
      pred = predict(model, X_test_new)$predictions
      #print(pred)
      X_pred_cont <- c(X_pred_cont, pred)
      X_test_new_list <- X_test_new_list[2:ny]
      X_test_new_list <- c(X_test_new_list, pred)
      X_test_new <- matrix(unlist(X_test_new_list), nrow=1, ncol=ny)
      colnames(X_test_new) <- c(vars)
    }
    return(X_pred_cont) 
  }
  nx <- dim(X_test)[1]
  ny <- dim(X_test)[2]
  X_pred_cont_rf <- predict_continuous_rf(model_rf, X_train_orig, nx, ny, vars)
  
  #plot_ly(data = data.frame(time=as.POSIXct(time), timeseries=as.numeric(timeseries)),
  #        x = ~time,
  #        y = ~timeseries)
  
  lines(t, X_pred_rf, col='red')
  lines(t, X_pred_cont_rf, col='green')
  
  
  old_df <- df
  
  print("Waiting for 5 minutes")
  # Waiting for 5 minutes
  Sys.sleep(time=300)
  clock <- clock + 300
  
  # Save new data frame every 1 hour
  if (clock == 3600) {
     
  }
  
  tm <- '1m'
  tkr <- binance_klines(t, interval=tm)
  df <- data.frame(tkr[,c('close_time', 'open', 'high', 'low', 'close', 'volume')], stringsAsFactors = FALSE)
}

# directories
binance_dir <- "~/Documents/projects/crypto/coinalysis/R/binance"
binance_data_dir <- "~/Documents/projects/crypto/coinalysis/R/binance/BinancePickleData"
binance_csv_dir <-  "~/Documents/projects/crypto/coinalysis/R/binance/BinanceCSVData"

rda_file <- 'Binance_tickers_all_1m_2018-05-25.Rda'
csv_data_file <- "BTCUSDT_1m_from_2018-2-22_to_2018-5-31.csv"

setwd(binance_dir)

load(paste('BinanceData/', rda_file, sep=""))
dfp <- tcks[['BTCUSDT']]
head(dfp)
