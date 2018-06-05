library(binancer)
setwd("~/Documents/projects/crypto/coinalysis/R/binance")
source('config.R')

# Set binance credentials
binance_credentials(key=API_KEY_BINANCE, secret = SECRET_BINANCE)
binance_coins <- binance_coins()
binance_ticker_prices <- binance_ticker_all_prices()
coinN <- length(binance_coins)

# Set cobinhood credentials
cobinhood_credentials(key=API_KEY_COBINHOOD, secret = '')
cobinhood_ticker_prices <- cobinhood_ticker_all_prices()

#ticker_prices <- binance_ticker_prices
ticker_prices <- cobinhood_ticker_prices

# BTC Market
btc_tck_idx <- endsWith(ticker_prices$symbol, "BTC")
btc_tck <- ticker_prices$symbol[btc_tck_idx]
ticker_prices_btc_market <- ticker_prices[btc_tck_idx, ]
coinNBTC <- dim(ticker_prices_btc_market)[1]
arbitrage_matrix_BTC <- matrix(data = rep(0, (coinNBTC + 1) * (coinNBTC + 1)), nrow=(coinNBTC + 1), ncol=(coinNBTC + 1))
colnames(arbitrage_matrix_BTC) <- c("BTC", strsplit(btc_tck, "BTC"))
rownames(arbitrage_matrix_BTC) <- c("BTC", strsplit(btc_tck, "BTC"))

# ETH Market
eth_tck_idx <- endsWith(ticker_prices$symbol, "ETH")
eth_tck <- ticker_prices$symbol[eth_tck_idx]
ticker_prices_eth_market <- ticker_prices[eth_tck_idx, ]
coinNETH <- dim(ticker_prices_eth_market)[1]
arbitrage_matrix_ETH <- matrix(data = rep(0, (coinNETH + 1) * (coinNETH + 1)), nrow=(coinNETH + 1), ncol=(coinNETH + 1))
colnames(arbitrage_matrix_ETH) <- c("ETH", strsplit(eth_tck, "ETH"))
rownames(arbitrage_matrix_ETH) <- c("ETH", strsplit(eth_tck, "ETH"))

# Handle ETHOS (Only for cobinhood)
ethos_id <- which(colnames(arbitrage_matrix_ETH) == "c(\"\", \"OS\")")
colnames(arbitrage_matrix_ETH)[ethos_id] <- 'ETHOS'
rownames(arbitrage_matrix_ETH)[ethos_id] <- 'ETHOS'
eth_tck[ethos_id] = 'ETHOSETH'


# BNB Market
bnb_tck_idx <- endsWith(ticker_prices$symbol, "BNB")
bnb_tck <- ticker_prices$symbol[bnb_tck_idx]
ticker_prices_bnb_market <- ticker_prices[bnb_tck_idx, ]
coinNBNB <- dim(ticker_prices_bnb_market)[1]
arbitrage_matrix_BNB <- matrix(data = rep(0, (coinNBNB + 1) * (coinNBNB + 1)), nrow=(coinNBNB + 1), ncol=(coinNBNB + 1))
colnames(arbitrage_matrix_BNB) <- c("BNB", strsplit(bnb_tck, "BNB"))
rownames(arbitrage_matrix_BNB) <- c("BNB", strsplit(bnb_tck, "BNB"))

# COB Market
cob_tck_idx <- endsWith(ticker_prices$symbol, "COB")
cob_tck <- ticker_prices$symbol[cob_tck_idx]
ticker_prices_cob_market <- ticker_prices[cob_tck_idx, ]
coinNCOB <- dim(ticker_prices_cob_market)[1]
arbitrage_matrix_COB <- matrix(data = rep(0, (coinNCOB + 1) * (coinNCOB + 1)), nrow=(coinNCOB + 1), ncol=(coinNCOB + 1))
colnames(arbitrage_matrix_COB) <- c("COB", strsplit(cob_tck, "COB"))
rownames(arbitrage_matrix_COB) <- c("COB", strsplit(cob_tck, "COB"))


# USDT Market
usdt_tck_idx <- endsWith(ticker_prices$symbol, "USDT")
usdt_tck <- ticker_prices$symbol[usdt_tck_idx]
ticker_prices_usdt_market <- ticker_prices[usdt_tck_idx, ]
coinNUSDT <- dim(ticker_prices_usdt_market)[1]
arbitrage_matrix_USDT <- matrix(data = rep(0, (coinNUSDT + 1) * (coinNUSDT + 1)), nrow=(coinNUSDT + 1), ncol=(coinNUSDT + 1))
colnames(arbitrage_matrix_USDT) <- c("USDT", strsplit(usdt_tck, "USDT"))
rownames(arbitrage_matrix_USDT) <- c("USDT", strsplit(usdt_tck, "USDT"))

i <- j <- k <- l <- 1
tckN <- length(ticker_prices$symbol)
for (c in 1:tckN) {
  tck <- ticker_prices[c,]
  symb <- tck$symbol
  #print(symb)

  if (endsWith(symb, "BTC") == TRUE) {
    #print(sprintf("i=%d",i))
    s <- unlist(strsplit(symb, "BTC"))
    #print(s)
    arbitrage_matrix_BTC[s, "BTC"] <- as.numeric(tck$price)
    arbitrage_matrix_BTC["BTC", s] <- as.numeric(1/tck$price)
    if(i==1) {
      arbitrage_matrix_BTC["BTC", "BTC"] <- 1
    }
    i <- i+1
  } 
  else if (endsWith(symb, "ETH") == TRUE) {
    #print(sprintf("j=%d",j))
    s <- unlist(strsplit(symb, "ETH"))
    if (startsWith(symb, "ETHOS") == TRUE)
        s <- "ETHOS"
    #print(s)
    arbitrage_matrix_ETH[s, "ETH"] <- as.numeric(tck$price)
    arbitrage_matrix_ETH["ETH", s] <- as.numeric(1/tck$price)
    if(j==1) {
      arbitrage_matrix_ETH["ETH", "ETH"] <- 1
    }
    j <- j+1
  } else if (endsWith(symb, "BNB") == TRUE) {
    #print(sprintf("k=%d",k))
    s <- unlist(strsplit(symb, "BNB"))
    #print(s)
    arbitrage_matrix_BNB[s, "BNB"] <- as.numeric(tck$price)
    arbitrage_matrix_BNB["BNB", s] <- as.numeric(1/tck$price)
    if(k==1) {
      arbitrage_matrix_BNB["BNB", "BNB"] <- 1
    }
    k <- k+1
  } else if (endsWith(symb, "COB") == TRUE) {
    #print(sprintf("k=%d",k))
    s <- unlist(strsplit(symb, "COB"))
    #print(s)
    arbitrage_matrix_COB[s, "COB"] <- as.numeric(tck$price)
    arbitrage_matrix_COB["COB", s] <- as.numeric(1/tck$price)
    if(k==1) {
      arbitrage_matrix_COB["COB", "COB"] <- 1
    }
    k <- k+1
  } else if (endsWith(symb, "USDT") == TRUE) {
    #print(sprintf("l=%d",l))
    s <- unlist(strsplit(symb, "USDT"))
    #print(s)
    arbitrage_matrix_USDT[s, "USDT"] <- as.numeric(tck$price)
    arbitrage_matrix_USDT["USDT", s] <- as.numeric(1/tck$price)
    if(l==1) {
      arbitrage_matrix_USDT["USDT", "USDT"] <- 1
    }
    l <- l+1
  }
}

for (ir in 1:coinNBTC) {
  rC <- unlist(strsplit(btc_tck[ir], "BTC"))
  for (jc in 1:coinNBTC) {
    cC <- unlist(strsplit(btc_tck[jc], "BTC"))
    if (ir == jc) {
      arbitrage_matrix_BTC[rC, cC] <- 1
      next
    }
    arbitrage_matrix_BTC[rC, cC] <- as.numeric(arbitrage_matrix_BTC[rC, "BTC"] * arbitrage_matrix_BTC["BTC", cC])
    arbitrage_matrix_BTC[cC, rC] <- as.numeric(1/arbitrage_matrix_BTC[rC, cC])
  }
}

for (ir in 1:coinNETH) {
  rC <- strsplit(eth_tck[ir], "ETH")
  if(rC == "c(\"\", \"OS\")") { # special care for ETHOS in cobinhood
    rC <- "ETHOS"
  }
  rC <- unlist(rC)
  for (jc in 1:coinNETH) {
    cC <- strsplit(eth_tck[jc], "ETH")
    if(cC == "c(\"\", \"OS\")") {
      cC <- "ETHOS"
    }
    cC <- unlist(cC)
    if (ir == jc) {
      arbitrage_matrix_ETH[rC, cC] <- 1
      next
    }
    arbitrage_matrix_ETH[rC, cC] <- as.numeric(arbitrage_matrix_ETH[rC, "ETH"] * arbitrage_matrix_ETH["ETH", cC])
    arbitrage_matrix_ETH[cC, rC] <- as.numeric(1/arbitrage_matrix_ETH[rC, cC])
  }
}

for (ir in 1:coinNBNB) {
  rC <- unlist(strsplit(bnb_tck[ir], "BNB"))
  for (jc in 1:coinNBNB) {
    cC <- unlist(strsplit(bnb_tck[jc], "BNB"))
    if (ir == jc) {
      arbitrage_matrix_BNB[rC, cC] <- 1
      next
    }
    arbitrage_matrix_BNB[rC, cC] <- as.numeric(arbitrage_matrix_BNB[rC, "BNB"] * arbitrage_matrix_BNB["BNB", cC])
    arbitrage_matrix_BNB[cC, rC] <- as.numeric(1/arbitrage_matrix_BNB[rC, cC])
  }
}

for (ir in 1:coinNCOB) {
  rC <- unlist(strsplit(cob_tck[ir], "COB"))
  for (jc in 1:coinNBNB) {
    cC <- unlist(strsplit(cob_tck[jc], "COB"))
    if (ir == jc) {
      arbitrage_matrix_COB[rC, cC] <- 1
      next
    }
    arbitrage_matrix_COB[rC, cC] <- as.numeric(arbitrage_matrix_COB[rC, "COB"] * arbitrage_matrix_BNB["BNB", cC])
    arbitrage_matrix_COB[cC, rC] <- as.numeric(1/arbitrage_matrix_COB[rC, cC])
  }
}

for (ir in 1:coinNUSDT) {
  rC <- unlist(strsplit(usdt_tck[ir], "USDT"))
  for (jc in 1:coinNUSDT) {
    cC <- unlist(strsplit(usdt_tck[jc], "USDT"))
    if (ir == jc) {
      arbitrage_matrix_USDT[rC, cC] <- 1
      next
    }
    arbitrage_matrix_USDT[rC, cC] <- as.numeric(arbitrage_matrix_USDT[rC, "USDT"] * arbitrage_matrix_USDT["USDT", cC])
    arbitrage_matrix_USDT[cC, rC] <- as.numeric(1/arbitrage_matrix_USDT[rC, cC])
  }
}

# Let's check arbitrage opportunity from usdt -> coin -> m2 -> usdt
#M_list <- c('BTC', 'ETH', 'BNB', 'USDT')
M_list <- c('BTC', 'ETH', 'COB', 'USDT')

arb_mat <- list()
arb_mat[['BTC']] <- arbitrage_matrix_BTC
arb_mat[['ETH']] <- arbitrage_matrix_ETH
arb_mat[['BNB']] <- arbitrage_matrix_BNB
arb_mat[['COB']] <- arbitrage_matrix_COB
arb_mat[['USDT']] <- arbitrage_matrix_USDT


get_trading_fee <- function(coin, amt) {
  return(0)
  if (coin != "BNB")
    fee <- amt * 0.1 / 100
  else 
    fee <- amt * 0.1 /(2 * 100)
  
  return(fee)
}

get_market_coin <- function(c1, c2) {
  c <- NULL
  tryCatch({
    v = arb_mat[[c1]][c1, c2] 
    return(c1)
  }, error = function(e) {
    v = arb_mat[[c2]][c1, c2] 
    return(c2)
  })
}


init_amt <- 100
M1 <- M_list[4]
m_tck <- usdt_tck # change
profit_unit <- arb_mat[['USDT']][M1, 'USDT']

# zero trading on cobinhood
on_cobinhood = 0

# sell coin in Market 1 (M1)
# I can sell and get any other coin in M1 market
arbit_path <- list()
selling_path <- list()
for (c in m_tck) {
  coin <- unlist(strsplit(c, M1))
  
  # Step 1
  # selling init_amt of M1 and getting n_coin 
  err = FALSE
  tryCatch({
    mk <- get_market_coin(M1, coin)
    selling_price1 <- paste(arb_mat[[mk]][M1, coin], "",M1,"-", coin)
    sold_amt <- arb_mat[[mk]][M1, coin] * init_amt
    fee <- get_trading_fee(M1, sold_amt)
    if(on_cobinhood)
      print(paste("fee =", fee, "", coin))
    n_coin <- sold_amt - fee * on_cobinhood
    #print(paste("Sold", init_amt, "of", M1, "and bought", n_coin, "of", coin, "in", mk, "market"))
    #print("1.-----------------------------------------------------------------------------")
    
  }, error = function(e){
    #print(e)
    err = TRUE
  })
  
  if(err)
    next
  
  # now convert in to other market coin
  M2_idx <- which(M_list != M1)
  M2_list <- M_list[M2_idx]
  
  # now sell in each market
  for(m2 in M2_idx) {
    # number of M2 coin
    M2 <- M_list[m2]
    
    # if (coin == M2)
    #   next
    err2 = FALSE
    tryCatch({
       # Step 2
       mk <- get_market_coin(M2, coin)
       selling_price2 <- paste(arb_mat[[mk]][coin, M2], "",coin,"-", M2)
       sold_amt2 <- arb_mat[[mk]][coin, M2] * n_coin
       fee2 <- get_trading_fee(coin, sold_amt2)
       if(on_cobinhood)
          print(paste("fee2 =", fee2, "", M2))
       n_M2 <- sold_amt2 - fee2 * on_cobinhood
                
       #print(paste("Sold", n_coin, "of", coin, "and bought", n_M2, "of", M2, "in", mk, "market"))
       #print("2.-----------------------------------------------------------------------------")
       #print("")
    },
    error = function(e) {
       #print(paste("Could not sell", coin, "in", M2, "market")) 
       #print(e)
       err2 = TRUE
    })
    
    if (err2)
      next
    
    tryCatch({

      # We can do arbitrary coin trading cycle here
    
      # Step 3
      mk <- get_market_coin(M1, M2)
      selling_price3 <- paste(arb_mat[[mk]][M1, M2], "",M1,"-", M2)
      sold_amt3 <- arb_mat[[mk]][M2, M1] * n_M2
      fee3 <- get_trading_fee(M2, sold_amt3)
      if(on_cobinhood)
        print(paste("fee3 =", fee3, "", M1))
      n_M1_back <- sold_amt3 - fee3 * on_cobinhood
    
      #print(paste("Sold", n_M2, "of", M2, "and bought", n_M1_back, "of", M1, "in", mk, "market"))
      #print("3.-----------------------------------------------------------------------------")
      #print("")
      a_p <- paste("Arbitrage path:", M1, "-->", coin, "-->", M2, "-->", M1)
      s_p <- paste("Price path:", selling_price1, "-->", selling_price2, "-->", selling_price3)
      print(a_p)
      print(s_p)
      final_amt <- n_M1_back 
      profit <- final_amt - init_amt
      print(paste("initial amount = ", init_amt))
      print(paste("final amount = ", final_amt))
    
      if (profit > 0) {
        print(paste("Profit of", profit, "", M1, "=", profit*profit_unit, "USDT"))
        arbit_path[[a_p]] <- profit * profit_unit
        selling_path[[s_p]] <- profit * profit_unit 
      } else
        print(paste("Loss of", profit, "", M1, "=", profit*profit_unit, "USDT"))
    
      print("")
    
    },
    error = function(e) {
      #print(paste("Could not sell", M2, "in", M1, "market")) 
      #print(e)
    })
  }
  
  print("------------------------------")
  print("     ------------")
  
  #break
}



