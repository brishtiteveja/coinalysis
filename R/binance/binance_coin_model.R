new_dataset <- function(dataset, step_size) {
  data_X = c()
  data_Y = c()
  for(i in 1:(length(dataset)-step_size)) {
    a <- dataset[i:(i+step_size)]
    data_X <- c(data_X, a)
    data_Y <- c(data_Y, dataset[i + step_size])
  }
  return(list(X = data_X, Y = data_Y))
}

dataset <- runif(100)
new_dataset(dataset, 2)

split_into_chunks <- function(data, train, predict, step, binary=TRUE, scale=TRUE) {
  X <- list()
  Y <- list()
  
  n <- 1
  for(i in 1:length(data)) {
    tryCatch({
      x_i = data[i:(i + train-1)]
      y_i = data[(i + train - 1 + predict)]
    }, error = function(cond) {
      print(cond)
    }, warning = function(cond){
      print(cond)
    } )
      
    # Use it only for daily return time series
    if (binary){
      if(y_i > 0.0) {
        y_i <- c(1.0, 0.0)
      }
      else {
        y_i <- c(0.0, 1.0)
      }
      if (scale)
        x_i <- scale(x_i)
    }
    else {
      if(i+train > length(data) || (i+train+predict) > length(data)) {
        break
      }
      timeseries <- c(data[i:(i + train - 1 + predict)])
      if(scale)
        timeseries[i:(i + train - 1)] = scale(timeseries[i : (i + train - 1)])
      
      nt <- length(timeseries)
      x_i = timeseries[-nt]
      y_i = timeseries[nt]
    }
  
    X[[n]]<-x_i
    Y[[n]]<-y_i
    n <- n + 1
    
    i <- i + step
  }
  
  res = list(X=X, Y=Y)
  return(res)
}


shuffle_in_unison<- function(a, b) {
  # courtsey http://stackoverflow.com/users/190280/josh-bleecher-snyder
  if (length(a) != length(b)) {
    print("Error: Length doesn't match for X and Y.")
    return()
  }
  
  shuffled_a = list()
  shuffled_b = list()
  permutation = sample(length(a))
  
  old_index <- 1
  for(new_index in permutation) {
    shuffled_a[[new_index]] = a[[old_index]]
    shuffled_b[[new_index]] = b[[old_index]]
    old_index <- old_index + 1
  } 
   
  res <- list(X = shuffled_a, Y = shuffled_b) 
  return(res)
}

crete_Xt_Yt <- function(X, y, percentage = 0.8) {
  idx = as.integer(length(X) * percentage)
  X_train = X[1:idx]
  Y_train = y[1:idx]
  
  sres = shuffle_in_unison(X_train, Y_train)
  X_train = sres$X
  Y_train = sres$Y
  
  N <- length(X)
  X_test = X[(idx+1):N]
  Y_test = y[(idx+1):N]
  
  res <- list(X_train = X_train, X_test = X_test, Y_train = Y_train, Y_test = Y_test)
  return(res)
}

process_time_series_data <- function(train_size, target_time, lag_size, binary=FALSE, scale=FALSE) {
  X_Y <- split_into_chunks(timeseries, TRAIN_SIZE, TARGET_TIME, LAG_SIZE, binary=FALSE, scale=FALSE)
  
  X <- X_Y$X
  Y <- X_Y$Y
  
  res <- crete_Xt_Yt(X, Y, percentage = 0.9)

  return(res)  
}



setwd("~/Documents/projects/crypto/coinalysis/R/binance")
load('BinanceData/Binance_tickers_all_5m_2018-05-07.Rda')
dfp <- tcks[['BTCUSDT']]

timeseries = dfp$close #* 100000000
time = dfp$close_time
plot(time, timeseries, t='l')


TRAIN_SIZE = (30/5)*2
TARGET_TIME = 1
LAG_SIZE = 1

res <- process_time_series_data(TRAIN_SIZE, TARGET_TIME, LAG_SIZE, scale = TRUE)
X_train <- res$X_train
X_test <- res$X_test
Y_train <- res$Y_train
Y_test <- res$Y_test

X_train <- matrix(as.numeric(unlist(X_train)), ncol=TRAIN_SIZE, byrow=TRUE)
X_test <- matrix(as.numeric(unlist(X_test)), ncol=TRAIN_SIZE, byrow=TRUE)
Y_train <- matrix(as.numeric(unlist(Y_train)), ncol=TARGET_TIME, byrow=TRUE)
Y_test <- matrix(as.numeric(unlist(Y_test)), ncol=TARGET_TIME, byrow=TRUE)

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

library(ranger)
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

lines(t, X_pred_rf, col='red')

# Using GLM: quassipoisson
head(X_train_rf)
nc <- ncol(X_train)
vars <- c()
for(i in 1:nc) {
  cn <- paste("Lag", as.character(nc - i + 1), sep="")
  vars <- c(vars, cn)
}
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))
fmla.glm <- as.formula(fmla)
library(mgcv)
model.glm <- glm(fmla.glm, data=data.frame(X_train_rf), family=gaussian)
model.glm
model.glm2 <- glm(fmla.glm, data=data.frame(X_train_rf), family=quasipoisson)
model.glm2

X_pred_glm <- predict(model.glm, data.frame(X_test_rf))
X_pred_glm2 <- predict(model.glm2, data.frame(X_test_rf))
np <- length(X_pred_gam)

nt <- length(time)
s <- (nt-np + 1)
e <- s + np -1
t <- time[s:e]
length(t)

lines(t, X_pred_glm, col='blue')
lines(t, X_pred_glm2, col='skyblue')

# Using GAM
head(X_train_rf)
nc <- ncol(X_train)
vars <- c()
for(i in 1:nc) {
  cn <- paste("s(Lag", as.character(nc - i + 1), ")", sep="")
  vars <- c(vars, cn)
}
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))
fmla.gam <- as.formula(fmla)
library(mgcv)
model.gam <- gam(fmla.gam, data=data.frame(X_train_rf), family=gaussian)
model.gam

X_pred_gam <- predict(model.gam, data.frame(X_test_rf))
np <- length(X_pred_gam)

nt <- length(time)
s <- (nt-np + 1)
e <- s + np -1
t <- time[s:e]
length(t)

lines(t, X_pred_gam, col='pink')

# Using xgboost
library(xgboost)
library(dplyr)
cv <- xgb.cv(data=X_train,
             label=Y_train,
             nrounds = 1000,
             nfold=10,
             objective='reg:linear',
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10,
             verbose = 0
             )

# Get the evaluation log 
elog <- cv$evaluation_log

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)

ntrees <- which.min(elog$train_rmse_mean)

model_xgb <- xgboost(data = X_train, # training data as matrix
                     label = Y_train,  # column of outcomes
                     nrounds = ntrees,       # number of trees to build
                     objective = "reg:linear", # objective
                     eta = 0.3,
                     depth = 6,
                     verbose = 0  # silent
)

X_pred_xgb <- predict(model_xgb, X_test)
np <- length(X_pred_xgb)

nt <- length(time)
s <- (nt-np + 1)
e <- s + np -1
t <- time[s:e]
length(t)

lines(t, X_pred_xgb, col='green')


#lstm
library(keras)
library(readr)
library(stringr)
library(purrr)

model <- keras_model_sequential()

HIDDEN_RNN <- 2
EMB_SIZE <- 1
model %>%
  layer_lstm(32, input_shape = c(1,TRAIN_SIZE), return_sequences=TRUE) %>%
  layer_lstm(16) %>%
  #layer_activation('relu') %>%
  layer_dense(1) %>%
  layer_activation("linear")

optimizer <- optimizer_rmsprop(lr = 0.01)

model %>% compile(
  loss = 'mean_squared_error',#"categorical_crossentropy", #
  optimizer = optimizer
)

# Training & Results ----------------------------------------------------

sample_mod <- function(preds, temperature = 1){
  preds <- log(preds)/temperature
  exp_preds <- exp(preds)
  preds <- exp_preds/sum(exp(preds))
  
  rmultinom(1, 1, preds) %>% 
    as.integer() %>%
    which.max()
}

on_epoch_end <- function(epoch, logs) {
  
  cat(sprintf("epoch: %02d ---------------\n\n", epoch))
  
}

print_callback <- callback_lambda(on_epoch_end = on_epoch_end)

x <- array(0, dim = c(nrow(X_train), 1, ncol(X_train)))
y <- array(0, dim = c(nrow(Y_train), ncol(Y_train)))
for(i in 1:nrow(X_train)){
  x[i,,] <- X_train[i,]
  y[i,] <- Y_train[i]
}

model %>% 
  fit(
    x, y,
    batch_size = 10,
    epochs = 20,
    verbose = 2,
    callbacks = print_callback
  )

  
x_test <- array(0, dim = c(nrow(X_test), 1, ncol(X_test)))
y_test <- array(0, dim = c(nrow(Y_test), ncol(Y_test)))
for(i in 1:nrow(X_test)){
  x_test[i,,] <- X_test[i,]
  y_test[i,] <- Y_test[i]
}
model %>%
  evaluate(x_test, y_test)

X_pred_lstm <- predict(model, x_test)
np <- length(X_pred_lstm)

nt <- length(time)
s <- (nt-np + 1)
e <- s + np -1
t <- time[s:e]
length(t)

plot(time, timeseries, t='l')
lines(t, X_pred_lstm, col='red')

