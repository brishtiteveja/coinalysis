library(binancer)
library(plotly)
library(ranger)
library(data.table)
library(dplyr)

source('../binance/Config.R')

# directories
binance_dir <- "~/Documents/projects/crypto/coinalysis/R/binance/"
binance_csv_data_dir <- "~/Documents/projects/crypto/coinalysis/R/binance/BinanceCSVData/"

t_symb <- 'BTCUSDT'

tm <- '1m'
tkr <- binance_klines(t_symb, interval=tm)
df <- data.frame(tkr[,c('close_time', 'open', 'high', 'low', 'close', 'volume')], stringsAsFactors = FALSE)

timeseries_orig <- c()
time <- c()
old_df <- data.frame()

clock <- 0
while(1) {
  df <- df[,c('close_time', 'close')]
  tmp_df <- rbind(old_df, df)
  tmp_df <- tmp_df[order(tmp_df$close_time),]
  print(dim(tmp_df))
 
  # merge the new data frame with the old one 
  df <- unique(tmp_df, by=close_time)
  df <- df[!duplicated(df$close_time),]
  rownames(df) <- seq(1, dim(df)[1])
  print(dim(df))
  
  time = as.POSIXct(df$close_time)
  timeseries_orig = as.numeric(df$close)
  
  timeseries = timeseries_orig #[1:as.integer(tL* 0.80)]
  
  # processing time series
  TRAIN_SIZE = (30/5)*2
  TARGET_TIME = 1
  LAG_SIZE = 1
  
  res <- process_time_series_data(timeseries, TRAIN_SIZE, TARGET_TIME, LAG_SIZE, scale = TRUE, percentage = 0.97)
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
  
  # training model and predicting using random forest
  # how to handle previous model?
  resm <- train_and_predict(X_train, Y_train, X_test, Y_test, model = 'random_forest', time)
  X_pred_rf <- resm$X_pred_rf
  X_pred_cont_rf <- resm$X_pred_cont_rf
  
  # plotting
  #plot_ly(data = data.frame(time=as.POSIXct(time), timeseries=as.numeric(timeseries)),
  #        x = ~time,
  #        y = ~timeseries)
  par(mfrow=c(1,1))
  
  np <- length(X_pred_rf)
  nt <- length(time)
  s <- (nt-np + 1)
  e <- s + np-1
  t <- time[s:e]
  
  #plot(time, timeseries, t='l')
  #lines(t, X_pred_rf, col='red')
  #lines(t, X_pred_cont_rf, col = 'green')
  pt1_df <- data.frame(t_full = time, ts = timeseries)
  pt2_df <- data.frame(t_test = t, X_pred_rf = X_pred_rf, X_pred_cont_rf = X_pred_cont_rf)
  p = plot_ly(x = time, y = timeseries, type='scatter', mode='lines+markers',
              marker=list(color='black', size=3), name=t_symb) %>%
    add_trace(data=pt2_df, x=~t_test, y=~X_pred_rf, 
              mode='lines+nomarkers', line= list(color='red'), name="pred") %>%
    add_trace(data=pt2_df, x=~t_test, y=~X_pred_cont_rf,
              mode='lines+nomarkers', line=list(color='green'), name="cont pred")
  print(p)
  
  old_df <- df
  
  print("Waiting for 5 minutes for next batch data")
  # Waiting for 5 minutes
  Sys.sleep(time=120)
  clock <- clock + 300
  
  # Save new data frame every 1 hour
  if (clock == 1800) {
    t_frm1 <- min(df$close_time)
    t_frm2 <- unlist(strsplit(as.character(t_frm1),split = c(' ')))
    t_frm3 <- paste(t_frm2[1], t_frm2[2], sep = "_")
    t_to1 <- max(df$close_time)
    t_to2 <- unlist(strsplit(as.character(t_to), split = c(' ')))
    t_to3 <- paste(t_to2[1], t_to2[2], sep = "_")
    frm_to <- paste(t_frm3, t_to3, sep="_")

    fn <- paste(t_symb, "_", tm, "_", sep = "")
    output_csv_file <- paste(binance_csv_data_dir, fn, frm_to, ".csv", sep = "")
    
    write.table(df, file=output_csv_file, sep = ",", col.names = TRUE, row.names = FALSE) 
    
    #dev.off()
  }
  
  error <- TRUE
  while(error) {
    tryCatch({
      tkr <- binance_klines(t_symb, interval=tm)
      error <- FALSE
    },
      error = function(e) {
        print(e)
    })
  }
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
