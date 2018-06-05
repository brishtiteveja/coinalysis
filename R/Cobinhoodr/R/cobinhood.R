COBINHOOD <- list(
    INTERVALS = c(
        '1m', '3m', '5m', '15m', '30m',
        '1h', '2h', '4h', '6h', '8h', '12h',
        '1d', '3d', '1w', '1M'))

# API-URLS
# https://cobinhood.github.io/api-public/#overview
# https://www.npmjs.com/package/node-cobinhood-api
# https://github.com/KenoLeon/CobinhoodAPI

## #############################################################################
## Utils

#' Look up Cobinhood API secret stored in the environment
#' @return string
#' @keywords internal
cobinhood_secret <- function() {
    cobinhood_check_credentials()
    credentials$secret
}


#' Look up Cobinhood API key stored in the environment
#' @return string
#' @keywords internal
cobinhood_key <- function() {
    cobinhood_check_credentials()
    credentials$key
}


#' Sets the API key and secret to interact with the Cobinhood API
#' @param key string
#' @param secret string
#' @export
#' @examples \dontrun{
#' cobinhood_credentials('foo', 'bar')
#' }
cobinhood_credentials <- function(key, secret) {
    credentials$key <- key
    credentials$secret <- secret
    
    return(credentials)
}


#' Check if Cobinhood credentials were set previously
#' @return fail on missing credentials
#' @keywords internal
cobinhood_check_credentials <- function() {
    if (is.null(credentials$secret)) {
        stop('Cobinhood API secret not set. May be not required.')
    }
    if (is.null(credentials$key)) {
        stop('Cobinhood API key not set? Call cobinhood_credentials()')
    }
}

#' Return current UNIX timestamp in millisecond
#' @return milliseconds since Jan 1, 1970
#' @keywords internal
timestamp <- function() {
  as.character(round(as.numeric(Sys.time()) * 1e3))
}


#' Request the Cobinhood API
#' @param base URL
#' @param path string
#' @param method HTTP request method
#' @param params list
#' @param config httr::config
#' @param retry allow retrying the query on failure
#' @param retries internal counter of previous retries
#' @return R object
#' @keywords internal
#' @importFrom httr GET content config add_headers
#' @importFrom futile.logger flog.error
query <- function(base, path, method = 'GET',
                  params = list(), config = config(),
                  retry = method == 'GET', retries = 0) {
  
  res <- tryCatch(
    content(GET(base, config = config, path = path, query = params)),
    error = function(e) e)
  
  if (inherits(res, 'error')) {
    if (isTRUE(retry) & retries < 4) {
      mc <- match.call()
      mc$retries <- mc$retries + 1
      flog.error('Query to %s/%s failed for the %sst/nd/rd/th time, retrying',
                 base, path, mc$retries)
      eval(mc, envir = parent.frame())
    }
  }
  
  res
  
}

#' Request the Cobinhood API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @param sign if signature required
#' @param retry allow retrying the query on failure
#' @return R object
#' @keywords internal
cobinhood_query <- function(endpoint, method = 'GET',
                          params = list(), sign = FALSE,
                          retry = method == 'GET') {

    method <- match.arg(method)

    if (isTRUE(sign)) {
        #params <- cobinhood_sign(params)
        config <- add_headers(
          'authorization' = cobinhood_key()
          )
    } else {
        config <- config()
    }

    res <- query(
        base = 'https://api.cobinhood.com',
        path = endpoint,
        method = method,
        params = params,
        config = config)

    return(res)
}


## #############################################################################
## Ticker data

#' Get latest Cobinhood conversion rates and USD prices on all symbol pairs
#' @return data.table
#' @export
cobinhood_all_tickers <- function() {
    qry <- cobinhood_query(endpoint = '/v1/market/tickers')
    if (qry$success) {
      tcks <- qry$result[[1]]
    } else {
      print("Cobinhood ticker query was unsuccessful. Please try again.")
      return()
    }
    
    tck <- tcks[[1]]
    n_col = length(tck)
    tcks_df <- data.frame(matrix(unlist(tcks), ncol=n_col, byrow=TRUE))
    colnames(tcks_df) <- names(tck)
    
    return(tcks_df)
}

## #############################################################################
## Ticker Price data

#' Get latest Cobinhood conversion rates and USD prices on all symbol pairs
#' @return data.table
#' @export
cobinhood_ticker_all_prices <- function() {
  tcks <- cobinhood_all_tickers()
  symbols <- c()
  for(tck in as.character(tcks$trading_pair_id)) {
    symb <- unlist(strsplit(tck, "-"))
    symb <- paste(symb[1], symb[2], sep="")
    symbols <- c(symbols, symb)
  }
  
  prices <- data.frame(symbol = symbols, 
                       price=as.numeric(as.character(tcks$last_trade_price)), 
                       stringsAsFactors = FALSE)
  
  return(prices)
}

## #############################################################################
## Check order

cobinhood_check_order <- function() {
  order_params <- list(trading_pair_id = "BTC-USDT",
                       side = "bid",
                       type = "limit",
                       price = "5000.01",
                       size = "1.0100"
                       )
  qry <- cobinhood_query(endpoint = '/v1/trading/orders', params = order_params)
  if (qry$success) {
    tcks <- qry$result[[1]]
  } else {
    print("Cobinhood ticker query was unsuccessful. Please try again.")
    return()
  }
}


