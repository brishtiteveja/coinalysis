library(xts)
library(dplyr)
library(dygraphs)

data(sample_matrix)
m <- tail(sample_matrix, n = 32)
dygraph(m) %>%
  dyCandlestick()

install.packages(c('quantmod','graphics','scatterplot3d','fpc','gplots','RColorBrewer'))

library(quantmod)
library(binancer)

t <- 'BTCUSDT'

tm <- '1d'
tkr <- binance_klines(t, interval=tm)
d <- data.frame(tkr[,c('close_time', 'open', 'high', 'low', 'close', 'volume')], stringsAsFactors = FALSE)
df <- d[, c('open', 'high', 'low', 'close')]
colnames(df) <- c('Open', 'High', 'Low', 'Close')

library(xts)
SYMB <- xts(df, order.by = d$close_time)
x<- SYMB
plot(x)

# candlestick chart
c <- candleChart(x, 
            subset = 'last 6 months',
            theme=chartTheme('white'),
            TA=c(addBBands())
            )
addRSI()
addEMA(with.col = 'Close')

library(dygraphs)
dygraph(x) %>% 
  dyCandlestick()

# kmeans clustering

ClOp <-Cl(x) /  Op(x)
HiOp <-Hi(x) /  Op(x)
LoOp <- Lo(x) / Op(x)
datf <- data.frame(ClOp, HiOp , LoOp)
colnames(datf) <- c('ClOp', 'HiOp', 'LoOp')
dat<- as.matrix(datf)
colnames(dat) <- c('ClOp', 'HiOp', 'LoOp')

# Identify 7 clusters
km <- kmeans(dat,7)

# 3d scatter plot to see the clusters
library(scatterplot3d)
scatterplot3d(dat,color=km$cluster)

library(plotly)
p <- plot_ly(datf, 
             x = ~ClOp, y = ~HiOp, z = ~LoOp,
             color = km$cluster
            ) %>%
  add_markers(marker=list(size = 3,
                          colors = 'rgba(152, 0, 0, .8)'
                         )
             ) %>%
  layout(scene = list(xaxis = list(title = 'Close'),
                      yaxis = list(title = 'High'),
                      zaxis = list(title = 'Low')))

p


# subset the ohlc data with clusters
x$Cluster <- as.numeric(km$cluster)

plot_ohlc_cluster <- function(x, k, col=1) {
  x_k_d <- x_k <- subset(x, Cluster==k)[,1:4]
  plm <- paste('OHLC Cluster ', k, sep='')
  dp <- dygraph(x_k, main=plm) %>%
    dyCandlestick() %>%
      dyOptions(colorValue = col)
  print(dp)
  return(x_k_d)
}

# Examine Cluster 1
x_k_d <- plot_ohlc_cluster(x, 1)
head(x_k)
idx <- index(x_k)
x_k_s <- subset(dat, rownames(dat) %in% as.character(idx))
head(x_k_s)

# Examine Cluster 2
x_k <- plot_ohlc_cluster(x, 2)
head(x_k)
idx <- index(x_k)
x_k_s <- subset(dat, rownames(dat) %in% as.character(idx))
head(x_k_s)

# Examine Cluster 3
x_k <- plot_ohlc_cluster(x, 3)
head(x_k)
idx <- index(x_k)
x_k_s <- subset(dat, rownames(dat) %in% as.character(idx))
head(x_k_s)

# Examine Cluster 4
x_k <- plot_ohlc_cluster(x, 4)
head(x_k)
idx <- index(x_k)
x_k_s <- subset(dat, rownames(dat) %in% as.character(idx))
head(x_k_s)

# Examine Cluster 5
x_k <- plot_ohlc_cluster(x, 5)
head(x_k)
idx <- index(x_k)
x_k_s <- subset(dat, rownames(dat) %in% as.character(idx))
head(x_k_s)

# Examine Cluster 6
x_k <- plot_ohlc_cluster(x, 6)
head(x_k)
idx <- index(x_k)
x_k_s <- subset(dat, rownames(dat) %in% as.character(idx))
head(x_k_s)

# Examine Cluster 7
plot_ohlc_cluster(x, 7)
head(x_k)
idx <- index(x_k)
x_k_s <- subset(dat, rownames(dat) %in% as.character(idx))
head(x_k_s)

