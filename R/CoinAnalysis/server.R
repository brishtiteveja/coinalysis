#```{r}
createDualAxisPlot <- function(date, coin1, y1_dat, coin2, y2_dat) {
        trace1 <- list(
                x = c(date),
                y = c(y1_dat),
                line = list(color = "rgb(221, 42, 145)"),
                marker = list(
                        color = "rgb(0, 0, 155)",
                        size = 2
                ),
                mode = "lines+markers",
                name = coin1,
                opacity = 1,
                type = "scatter",
                uid = "e27bd3",
                xaxis = "x",
                yaxis = "y"
        )
        trace2 <- list(
                x = c(date),
                y = c(y2_dat),
                line = list(color = "rgb(36, 32, 29)"),
                marker = list(
                        color = "rgb(228, 103, 9)",
                        size = 5
                ),
                mode = "lines+markers",
                name = coin2,
                type = "scatter",
                uid = "efd000",
                yaxis = "y2"
        )

        layout <- list(
                autosize = TRUE,
                hovermode = "closest",
                showlegend = TRUE,

                xaxis = list(
                        autorange = TRUE,
                        rangeslider = list(
                                autorange= TRUE
                        ),
                        #range = c(-0.745097129104, 11.4882190132),
                        title = "",
                        type = "date",
                        showgrid = FALSE
                ),
                yaxis = list(
                        autorange = TRUE,
                        #domain = c(0, 1),
                        #range = c(-50, -20),
                        title = "USD",
                        type = "linear",
                        showgrid = FALSE
                ),
                yaxis2 = list(
                        anchor = "x",
                        autorange = TRUE,
                        overlaying = "y",
                        #range = c(-25.3091876767, 55.9930010303),
                        title = "USD",
                        side = "right",
                        type = "linear",
                        showgrid = FALSE,
                        showline = TRUE
                )
        )

        plot_data <- cbind.data.frame(trace1$x, trace1$y, trace2$y)
        colnames(plot_data) <- c('x', 'y1', 'y2')

        p <- plot_ly(plot_data, source='subset') %>%
                add_trace(x=~x, y=~y1, line=trace1$line, marker=trace1$marker, mode=trace1$mode, name=trace1$name,
                          opacity=trace1$opacity, type=trace1$type, uid=trace1$uid, xaxis=trace1$xaxis, yaxis=trace1$yaxis,
                          visible = TRUE) %>%
                add_trace(x=~x, y=~y2, line=trace2$line, marker=trace2$marker, mode=trace2$mode, name=trace2$name,
                          type=trace2$type, uid=trace2$uid, yaxis=trace2$yaxis,visible = TRUE) %>%
                layout(autosize=layout$autosize, hovermode=layout$hovermode, #showlegend=layout$showlegend,
                       xaxis=layout$xaxis, yaxis=layout$yaxis, yaxis2=layout$yaxis2,
                       dragmode = "select")

        return(p)
}


inputChangeEventHandler <- function(input, output) {
  # Create a convenience data.frame which can be used for charting
  coin1 <- input$coinInput1
  coin2 <- input$coinInput2

  history_start <- '20090101'
  history_end <- format(Sys.time(), "%Y%m%d")

  coin_data1 <- coin_data_lists[[coin1]]
  coin_data2 <- coin_data_lists[[coin2]]

  coin1_p_date <- as.Date(coin_data1$Date, "%B %d, %Y")
  coin2_p_date <- as.Date(coin_data2$Date, "%B %d, %Y")

  # Find the lowest timestamp to start
  min_coin1_date <- min(coin1_p_date)
  min_coin2_date <- min(coin2_p_date)
  min_date <- max(min_coin1_date, min_coin2_date)

  # Find the latest timestamp to end
  max_coin1_date <- max(coin1_p_date)
  max_coin2_date <- max(coin2_p_date)
  max_date <- min(max_coin1_date, max_coin2_date)

  coin1_s_id <- which(coin1_p_date == min_date)
  coin1_e_ed <- which(coin1_p_date == max_date)

  coin2_s_id <- which(coin2_p_date == min_date)
  coin2_e_ed <- which(coin2_p_date == max_date)

  # Create date sequence for x axis
  x <- coin1_p_date[coin1_s_id:(coin1_e_ed + 1)]
  n <- length(x) + 1
  date <- as.character.Date(x)
  dat1 <- coin_data1[coin1_s_id:(coin1_e_ed + 1),]
  dat2 <- coin_data2[coin2_s_id:(coin2_e_ed + 1),]

  date <- as.Date(as.character(dat1$Date) , "%b %d, %Y")

  if (input$coinMetric %in% c('Low', 'High', 'Open', 'Close'))
  {
    y1_dat <- as.numeric(as.character(dat1[, input$coinMetric]))
    y2_dat <- as.numeric(as.character(dat2[, input$coinMetric]))
  }
  else if (input$coinMetric %in% c('Market Cap', 'Volume')) {
    y1_dat <- as.numeric(as.character(gsub(",","",dat1[, input$coinMetric])))
    y2_dat <- as.numeric(as.character(gsub(",","",dat2[, input$coinMetric])))
  }

  ay <- list(
    #tickfont = list(color = "red"),
    overlaying = "y",
    side = "right"
  )
  p <- createDualAxisPlot(date, coin1, y1_dat, coin2, y2_dat)

  return(p)
}



# server
server <- function(input, output){
  # Observes the second feature input for a change
  observeEvent(input$coinMetric,{
    p <- inputChangeEventHandler(input, output)
    output$Plot1 <- renderPlotly({p})
  })
  observeEvent(input$coinInput1,{
    p <- inputChangeEventHandler(input, output)
    output$Plot1 <- renderPlotly({p})
  })
  # Observes the second feature input for a change
  observeEvent(input$coinInput2,{
    p <- inputChangeEventHandler(input, output)
    output$Plot1 <- renderPlotly({p})
  })

  # Coupled Correlation Plot on selection
  # Coupled hover event
  output$Plot2 <- renderPlotly({

    # Read in hover data
    event.data <- event_data("plotly_selected", source = 'subset')
    validate(need(!is.null(event.data), "Select data from time series chart to populate."))
    # If NULL dont do anything
    if(is.null(event.data) == T) return(NULL)

    y1_df <- subset(event.data, curveNumber == 0)
    y2_df <- subset(event.data, curveNumber == 1)

    library(dplyr)
    df <- inner_join(x = y1_df, y = y2_df, by = "pointNumber")

    plot_ly(df, x = ~y.x, y = ~y.y, type = "scatter") %>% #,
            #colors = colorRamp(c('#e3dfc8', '#808c6c')))%>%
           layout(title = "Cross-Correlation of two coins in selected timeseries",
           xaxis = list(title = input$coinInput1),
           yaxis = list(title = input$coinInput2))
  })

  output$Plot3 <- renderPlotly({
      library(dplyr)
      coins <- names(coin_data_lists)

      coin_df <- data.frame()
      coin_col_date <- coin_data_lists[[coins[1]]]['Date']
      coin_col <- coin_data_lists[[coins[1]]][input$coinMetric]
      coin_df <- cbind.data.frame(coin_col_date, coin_col)

      for(c in coins[2:length(coins)]) {
          cdf_date <- coin_data_lists[[c]]['Date']
          cdf <- coin_data_lists[[c]][input$coinMetric]
          c_df <- cbind.data.frame(cdf_date, cdf)
          coin_df <- suppressWarnings(inner_join(coin_df, c_df, by='Date'))
      }

      coin_pr_df <- coin_df[,2:length(coin_df)]
      names(coin_pr_df) <- coins
      coin_mat <- as.data.frame(lapply(coin_pr_df, as.numeric))

      cormat <- round(cor(coin_mat),2)

      plot_ly(x = rownames(cormat), y = colnames(cormat), z = cormat, type = "heatmap",
              colors = colorRamp(c('#e3dfc8', '#808c6c')))%>%
              layout(title = "Correlation heatmap for Cryptocurrencies",
                     xaxis = list(title = ""),
                     yaxis = list(title = ""))

  })
}
