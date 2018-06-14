
tweet_polarity_fn <- '/Users/andy/Documents/projects/crypto/coinalysis/python/bitcoin_prediction/sentiment6.txt'
df <- read.table(tweet_polarity_fn, sep=",")
time <- as.Date(as.character(df[,1]), format="%Y%m%d")
sent_df <- data.frame(time=time, sent=as.numeric(df[,2]))
plot(sent_df, t='l')

btc_sent <- xts(as.numeric(sent_df[,2]), order.by = sent_df$time)
plot(btc_sent)

# Hodl news
#https://news.hodlhodl.com/charts?scale=fromthebeginning&start_point=2016-07-07

# Crypto Fear and Greed Index
#https://alternative.me/crypto/fear-and-greed-index/

# Sentiment
# https://www.crypto-sentiment.com/sentiment

# Strategic Bias
# https://www.crypto-sentiment.com/strategic-bias

# Overconfidence Index
#https://www.crypto-sentiment.com/overconfidence-index