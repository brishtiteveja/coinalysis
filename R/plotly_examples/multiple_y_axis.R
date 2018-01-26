# Get this figure: fig <- get_figure("brishtiteveja", 14)
# Get this figure's data: data <- get_figure("brishtiteveja", 14)$data
# Add data to this figure: p <- add_trace(p, x=c(4, 5), y=c(4, 5), kwargs=list(filename="Plot 14", fileopt="extend"))
# Get y data of first trace: y1 <- get_figure("brishtiteveja", 14)$data[[1]]$y

# Get figure documentation: https://plot.ly/r/get-requests/
# Add data documentation: https://plot.ly/r/file-options/

# You can reproduce this figure in R with the following code!

# Learn about API authentication here: https://plot.ly/r/getting-started
# Find your api_key here: https://plot.ly/settings/api

library(plotly)
trace1 <- list(
  x = c(1, 2, 3), 
  y = c(4, 5, 6), 
  name = "yaxis1 data", 
  type = "scatter", 
  uid = "71b47d", 
  xsrc = "brishtiteveja:13:242ee5", 
  ysrc = "brishtiteveja:13:4323fe"
)
trace2 <- list(
  x = c(2, 3, 4), 
  y = c(40, 50, 60), 
  name = "yaxis2 data", 
  type = "scatter", 
  uid = "e1a84b", 
  xsrc = "brishtiteveja:13:fed864", 
  yaxis = "y2", 
  ysrc = "brishtiteveja:13:bf3a44"
)
trace3 <- list(
  x = c(4, 5, 6), 
  y = c(40000, 50000, 60000), 
  name = "yaxis3 data", 
  type = "scatter", 
  uid = "1a9af7", 
  xsrc = "brishtiteveja:13:f3372d", 
  yaxis = "y3", 
  ysrc = "brishtiteveja:13:7336ed"
)
trace4 <- list(
  x = c(5, 6, 7), 
  y = c(400000, 500000, 600000), 
  name = "yaxis4 data", 
  type = "scatter", 
  uid = "466da4", 
  xsrc = "brishtiteveja:13:9af270", 
  yaxis = "y4", 
  ysrc = "brishtiteveja:13:e64880"
)
data <- list(trace1, trace2, trace3, trace4)
layout <- list(
  title = "multiple y-axes example", 
  width = 800, 
  xaxis = list(
    autorange = TRUE, 
    domain = c(0.3, 0.7)#, 
    #range = c(0.508659079664, 7.49134092034), 
    #type = "linear"
  ), 
  yaxis = list(
    autorange = TRUE, 
    range = c(3.86524822695, 6.13475177305), 
    tickfont = list(color = "#1f77b4"), 
    title = "yaxis title", 
    titlefont = list(color = "#1f77b4"), 
    type = "linear"
  ), 
  yaxis2 = list(
    anchor = "free", 
    autorange = TRUE, 
    overlaying = "y", 
    position = 0.15, 
    range = c(38.6524822695, 61.3475177305), 
    side = "left", 
    tickfont = list(color = "#ff7f0e"), 
    title = "yaxis2 title", 
    titlefont = list(color = "#ff7f0e"), 
    type = "linear"
  ), 
  yaxis3 = list(
    anchor = "x", 
    autorange = TRUE, 
    overlaying = "y", 
    range = c(38652.4822695, 61347.5177305), 
    side = "right", 
    tickfont = list(color = "#d62728"), 
    title = "yaxis4 title", 
    titlefont = list(color = "#d62728"), 
    type = "linear"
  ), 
  yaxis4 = list(
    anchor = "free", 
    autorange = TRUE, 
    overlaying = "y", 
    position = 0.85, 
    range = c(386524.822695, 613475.177305), 
    side = "right", 
    tickfont = list(color = "#9467bd"), 
    title = "yaxis5 title", 
    titlefont = list(color = "#9467bd"), 
    type = "linear"
  )
)
p <- plot_ly()
p <- add_trace(p, x=trace1$x, y=trace1$y, name=trace1$name, type=trace1$type, uid=trace1$uid, xsrc=trace1$xsrc, ysrc=trace1$ysrc)
p <- add_trace(p, x=trace2$x, y=trace2$y, name=trace2$name, type=trace2$type, uid=trace2$uid, xsrc=trace2$xsrc, yaxis=trace2$yaxis, ysrc=trace2$ysrc)
p <- add_trace(p, x=trace3$x, y=trace3$y, name=trace3$name, type=trace3$type, uid=trace3$uid, xsrc=trace3$xsrc, yaxis=trace3$yaxis, ysrc=trace3$ysrc)
p <- add_trace(p, x=trace4$x, y=trace4$y, name=trace4$name, type=trace4$type, uid=trace4$uid, xsrc=trace4$xsrc, yaxis=trace4$yaxis, ysrc=trace4$ysrc)
p <- layout(p, title=layout$title, width=layout$width, xaxis=layout$xaxis, yaxis=layout$yaxis, yaxis2=layout$yaxis2, yaxis3=layout$yaxis3, yaxis4=layout$yaxis4)
p