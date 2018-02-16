# To Do
# Other historical stats can be received from https://blockchain.info/api/charts_api
# Idea: shinyapp  database constant update
#       regression price predictions
#       correlation between different variables


library(stringr)
library(XML)
library(RCurl)

get_currency_historical_data <- function(url) {
  url_parsed <- htmlParse(getURL(url), asText = TRUE)
  #print(url_parsed)
  tableNodes <- getNodeSet(url_parsed, c('//*[@class="table"]'))
  #print(tableNodes)

  currency_historical_data <- readHTMLTable(tableNodes[[1]])

  return(currency_historical_data)
}
# Function to create url from start to end
get_currency_data_download_url <- function(currency, history_start, history_end) {
  # Processing URL to download data
  url_cmarket <- 'https://coinmarketcap.com/'
  currency_part <- paste('currencies/', currency, sep="")
  url_pre <- paste(url_cmarket, currency_part, sep="")

  # Append start date
  url_s <- paste(url_pre, '/historical-data/?start=', sep="")
  url_s <- paste(url_s, history_start, sep="")

  # Append end date
  url_e <- paste(url_s, '&end=', sep="")
  url_e <- paste(url_e, history_end, sep="")

  url <- url_e
}

history_start <- '20090101'
history_end <- format(Sys.time(), "%Y%m%d")

currency1 <- 'bitcoin'
url1 <- get_currency_data_download_url(currency1, history_start, history_end)
data <- get_currency_historical_data(url1)
date <- as.Date(rev(data$Date), "%B %d, %Y")
data$Date <- rev(date)

btc_data <- data

btc_df <- btc_data[,c('Date', 'Close', 'Volume', 'Market Cap')]
colnames(btc_df) <- c('Date', 'Price', 'Volume', 'MarketCap')

btc_df$Price <- as.numeric(as.character(btc_df$Price))

volume_dat <- gsub(",", "", as.character(btc_df$Volume))
btc_df$Volume <- as.double(volume_dat)

market_cap_dat <- gsub(",", "", as.character(btc_df$MarketCap))
btc_df$MarketCap <- as.double(market_cap_dat)

# drop NAs
btc_df <- btc_df[complete.cases(btc_df),]
btc_df_backup <- btc_df
summary(btc_df_backup)

#subsetting btc_df after 2017
btc_df<-subset(btc_df, Date >= '2017-01-01')
summary(btc_df)

library(xts)
btc_ts <- xts(btc_df[,-1], order.by = btc_df$Date)
summary(btc_ts)

library(ggplot2)
time_range <- index(btc_ts) >= '2016-01-01'
df <-btc_ts[time_range,]


p1<-ggplot(df, aes(x=Index, y=Price)) +
    geom_line() +
    labs(x='Time') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2<-ggplot(df,aes(x=Index, y=Volume)) +
    geom_line(color='blue') +
    labs(x='Time') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
p3<-ggplot(df,aes(x=Index, y=MarketCap)) +
    geom_line(color='red') +
    labs(x='Time') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(cowplot)
plot_grid(p1, p2, p3, ncol = 3)

# The outcome column
(outcome <- "Price")

# The input variables
(vars <- c("Volume", "MarketCap"))

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))
fmla

#QuassiPoisson Model
(btc_pr_model.qp <- glm(fmla, # formula
                         data= btc_df, # data
                         family=quasipoisson))

summary(btc_pr_model.qp)
btc_df$Pred.qp <- predict(btc_pr_model.qp, btc_df)
library(dplyr)
btc_df %>%
        mutate(residuals.qp=Price-Pred.qp) %>%
        summarize(rmse.qp=sqrt(mean(residuals.qp^2)))

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(btc_df, aes(x = Pred.qp, y = Price)) +
        geom_point() +
        geom_abline()

# Plot predictions and cnt by date/time
library(tidyr)
p.qp <- btc_df %>%
        gather(key = valuetype, value = value, Price, Pred.qp) %>%
        ggplot(aes(x = Date, y = value, color = valuetype, linetype = valuetype)) +
        geom_point() +
        geom_line() +
        labs(y='BTC Price (USD)') +
        ggtitle("Predicted Bitcoin Price with QuassiPoisson Model.")
p.qp

#GAM Model
library(mgcv)
fmla.gam <- Price ~ s(Volume) + s(MarketCap)
(btc_pr_model.gam <- gam(fmla.gam, # formula
                         data= btc_df, # data
                         family=gaussian))

plot(btc_pr_model.gam)

summary(btc_pr_model.gam)
btc_df$Pred.gam <- predict(btc_pr_model.gam, btc_df)
library(dplyr)
btc_df %>%
        mutate(residuals.gam=Price-Pred.gam) %>%
        summarize(rmse.gam=sqrt(mean(residuals.gam^2)))

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(btc_df, aes(x = Pred.gam, y = Price)) +
        geom_point() +
        geom_abline()

# Plot predictions and cnt by date/time
library(tidyr)
p.gam <- btc_df %>%
        gather(key = valuetype, value = value, Price, Pred.gam) %>%
        ggplot(aes(x = Date, y = value, color = valuetype, linetype = valuetype)) +
        geom_point() +
        geom_line() +
        labs(y='BTC Price (USD)') +
        ggtitle("Predicted Bitcoin Price with Gam Model.")
p.gam

#Random Forest Model
library(ranger)
seed<-set.seed(34245)
(btc_pr_model.rf <- ranger(fmla, # formula
                         data= btc_df, # data
                         num.trees = 500,
                         respect.unordered.factors = "order",
                         seed = seed))

summary(btc_pr_model.rf)
btc_df$Pred.rf <- predict(btc_pr_model.rf, btc_df)$predictions
library(dplyr)
btc_df %>%
        mutate(residuals.rf=Price-Pred.rf) %>%
        summarize(rmse.rf=sqrt(mean(residuals.rf)))

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(btc_df, aes(x = Pred.rf, y = Price)) +
        geom_point() +
        geom_abline()

# Plot predictions and cnt by date/time
library(tidyr)
p.rf <- btc_df %>%
        gather(key = valuetype, value = value, Price, Pred.rf) %>%
        ggplot(aes(x = Date, y = value, color = valuetype, linetype = valuetype)) +
        geom_point() +
        geom_line() +
        labs(y='BTC Price (USD)') +
        ggtitle("Predicted Bitcoin Price with Random Forest Model.")
p.rf
#xgboost
library(vtreat)
treatplan <- designTreatmentsZ(btc_df, vars)

# Get the "clean" and "lev" variables from the scoreFrame
(newvars <- treatplan %>%
                magrittr::use_series(scoreFrame) %>%
                filter(code %in% c('clean', 'lev')) %>%  # get the rows you care about
                use_series(varName))

btc_df.treat <- prepare(treatplan, btc_df, varRestriction = newvars)
str(btc_df.treat)

library(xgboost)
# calculate number of tree to be used
cv <- xgb.cv(data=as.matrix(btc_df.treat),
             label=btc_df$Price,
             nrounds=100,
             nfold= 5,
             objective='reg:linear',
             eta=0.3,
             max_depth=6,
             early_stopping_rounds = 10,
             verbos=0)
elog <- cv$evaluation_log
elog
elog %>%
        summarize(ntrees.train= which.min(train_rmse_mean),
                  ntrees.test = which.min(test_rmse_mean))

ntrees = which.min(elog$test_rmse_mean)
btc_pr_model.xgb <- xgboost(data=as.matrix(btc_df.treat),
                            label=btc_df$Price,
                            nrounds=ntrees,
                            objective='reg:linear',
                            eta=0.3,
                            depth=6,
                            verbose=0
                            )

btc_df$Pred.xgb <- predict(btc_pr_model.xgb, as.matrix(btc_df.treat))
btc_df %>%
        mutate(residuals.xgb=Price-Pred.xgb) %>%
        summarize(rmse.xgb=sqrt(mean(residuals.xgb)))

ggplot(btc_df, aes(x=Pred.xgb, y=Price)) +
        geom_point() +
        geom_abline()

p.xgb <- btc_df %>%
        gather(key = valuetype, value = value, Price, Pred.xgb) %>%
        ggplot(aes(x = Date, y = value, color = valuetype, linetype = valuetype)) +
        geom_point() +
        geom_line() +
        labs(y='BTC Price (USD)') +
        ggtitle("Predicted Bitcoin Price with xgboost Model.")
p.xgb

library(cowplot)
plot_grid(p.qp, p.gam, p.rf, p.xgb, nrow=2, ncol=2)

library(DT)
datatable(btc_df[,c("Price", "Pred.qp","Pred.gam", "Pred.rf", "Pred.xgb")])

btc_ts <- xts(btc_df[,-1], order.by = btc_df$Date)
summary(btc_ts)

# Plotting the predictions
models <- c('BTC Price', 'QuasiPoisson', 'GAM', 'Random Forest', 'Xgboost')
p_col <- c('black', 'green', 'blue', 'pink', 'red')

windows(height = 7, width = 3.5)

plot(btc_df$Date, btc_df$Price, type='l', col=p_col[1], xlab='Time', ylab='Price (USD)',
        main = 'Regression Models to estimate past Bitcoin Price')
lines(btc_df$Date, btc_df$Pred.qp, type='l', col=alpha(p_col[2],0.5))
lines(btc_df$Date, btc_df$Pred.gam, type='l', col=alpha(p_col[3],0.5))
lines(btc_df$Date, btc_df$Pred.rf, type='l', col=alpha(p_col[4],0.5))
lines(btc_df$Date, btc_df$Pred.xgb, type='l', col=alpha(p_col[5],0.5))
legend('top', models, lty=1, col=p_col, cex=0.4)

