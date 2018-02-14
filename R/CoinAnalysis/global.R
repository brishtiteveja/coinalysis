library(stringr)
library(XML)
library(RCurl)
library(mlbench)
library(plotly)
library(shiny)
library(shinythemes)
library(dplyr)

#```{r global info}

# List all coins
get_list_of_all_coinmarketcap_coins <- function() {
  url = 'https://coinmarketcap.com/all/views/all/'
  url_parsed <- htmlParse(getURL(url), asText = TRUE)
  tableNodes <- getNodeSet(url_parsed, c('//*[@id="currencies-all"]'))
  all_currency <- readHTMLTable(tableNodes[[1]])

  currencies <- gsub("\n"," ", all_currency$Name)

  #currencies <- str_replace(gsub("\\s+", " ", str_trim(currencies)), "B", "b")

  c_symbols <- c()
  c_names <- c()

  for(c in currencies) {
    cr <- strsplit(c, " ")
    c_symbols <- c(c_symbols, cr[[1]][1])
    c_names <- c(c_names, cr[[1]][2])
  }

  cr_df <- cbind(c_names, c_symbols)
  colnames(cr_df) <- c('Name', 'Symbol')

  return(as.data.frame(cr_df))
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


get_currency_historical_data <- function(url) {
  url_parsed <- htmlParse(getURL(url), asText = TRUE)
  #print(url_parsed)
  tableNodes <- getNodeSet(url_parsed, c('//*[@class="table"]'))
  #print(tableNodes)

  currency_historical_data <- readHTMLTable(tableNodes[[1]])

  return(currency_historical_data)
}
#```

numCoins = 15
history_start <- '20090101'
history_end <- format(Sys.time(), "%Y%m%d")

currentCoinMetric = 'Close'
# Store features and actual class in seprate variables
if (exists("coin_info") == FALSE)
  coin_info <- get_list_of_all_coinmarketcap_coins()
coin_info <- coin_info[complete.cases(coin_info),]
if (exists("coin_data_lists") == FALSE)
  coin_data_lists <- list()
coins <- coin_info[1:numCoins,2] # First 15 coins
coinList <- as.character(coins)
coinNames <- as.character(coin_info[1:numCoins,1])
i <- 1
for (c in coinNames) {
  if (is.null(coin_data_lists[[coinList[i]]]) == TRUE) {
    url <- get_currency_data_download_url(c, history_start, history_end)
    coin_data_lists[[coinList[i]]] <- get_currency_historical_data(url)
  }
  i = i+1
}

coinMetrics <- c('Low', 'High', 'Open', 'Close', 'Volume', 'Market Cap')
