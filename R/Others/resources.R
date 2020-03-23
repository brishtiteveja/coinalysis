#```{Tutorial app for coupled events}

# App 1
library(shiny)
library(mlbench)
library(plotly)
library(shinythemes)
library(dplyr)


ui <- fluidPage(
        # Set theme
        theme = shinytheme("spacelab"),

        # Some help text
        h2("Coupled hover-events in plotly charts using Shiny"),
        h4("This Shiny app showcases coupled hover-events using Plotly's ", tags$code("event_data()"), " function."),

        # Vertical space
        tags$hr(),

        # Window length selector
        selectInput("window", label = "Select Window Length", choices = c(10, 20, 30, 60, 90), selected = 10),

        # Plotly Chart Area
        fluidRow(
                column(6, plotlyOutput(outputId = "timeseries", height = "600px")),
                column(6, plotlyOutput(outputId = "correlation", height = "600px"))),

        tags$hr(),
        tags$blockquote("Hover over time series chart to fix a specific date. Correlation chart will update with historical
                        correlations (time span will be hover date +/- selected window length)")
        )

server <- function(input, output){

        # Read data
        stockdata <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/stockdata.csv")

        # Create dates
        stockdata$Date <- as.Date(stockdata$Date)

        # Reshape
        ds <- reshape2::melt(stockdata, id = "Date")
        ds <- filter(ds, variable != "GSPC")

        # Set some colors
        plotcolor <- "#F5F1DA"
        papercolor <- "#E3DFC8"

        # Plot time series chart
        output$timeseries <- renderPlotly({
                p <- plot_ly(source = "source") %>%
                        add_lines(data = ds, x = ~Date, y = ~value, color = ~variable, mode = "lines", line = list(width = 3))

                # Add SP500
                p <- p %>%
                        add_lines(data = stockdata, x = ~Date, y = ~GSPC, mode = "lines", yaxis = "y2", name = "SP500", opacity = 0.3,
                                  line = list(width = 5)) %>%
                        layout(title = "Stock prices for different stocks overlaid with SP500",
                               xaxis = list(title = "Dates", gridcolor = "#bfbfbf", domain = c(0, 0.98)),
                               yaxis = list(title = "Stock Price", gridcolor = "#bfbfbf"),
                               plot_bgcolor = plotcolor,
                               paper_bgcolor = papercolor,
                               yaxis2 = list(title = "SP500", side = "right", overlaying = "y"))
                p
        })

        # Coupled hover event
        output$correlation <- renderPlotly({

                # Read in hover data
                eventdata <- event_data("plotly_hover", source = "source")
                validate(need(!is.null(eventdata), "Hover over the time series chart to populate this heatmap"))

                # Get point number
                datapoint <- as.numeric(eventdata$pointNumber)[1]

                # Get window length
                window <- as.numeric(input$window)

                # Show correlation heatmap
                rng <- (datapoint - window):(datapoint + window)
                cormat <- round(cor(stockdata[rng, 1:5]),2)

                plot_ly(x = rownames(cormat), y = colnames(cormat), z = cormat, type = "heatmap",
                        colors = colorRamp(c('#e3dfc8', '#808c6c')))%>%
                        layout(title = "Correlation heatmap",
                               xaxis = list(title = ""),
                               yaxis = list(title = ""))

        })
}


# App 2

library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)

# Load data
data(BreastCancer)

# Remove NAs
BreastCancer <- na.omit(BreastCancer)

# Remvove ID
BreastCancer <- BreastCancer[,-1]

# Store features and actual class in seprate variables
featureList <- colnames(BreastCancer)[-10]
class <- BreastCancer$Class

# Convert to numeric
BreastCancer[,1:9] <- apply(BreastCancer[,-10], 2, as.numeric)

# ui.R definition
ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),

  # Some help text
  h2("Coupled events in plotly charts using Shiny"),
  h4("This Shiny app showcases coupled events using Plotly's ", tags$code("event_data()"), " function."),
  tags$ol(
    tags$li("The first chart showcases", tags$code("plotly_selected")),
    tags$li("The third chart showcases", tags$code("plotly_click"))
  ),

  # Vertical space
  tags$hr(),

  # Feature selection
  fixedRow(
    column(6, selectInput(inputId = "featureInput1", label = "Select first feature", choices = featureList, selected = "Cell.Size")),
    column(6, selectInput(inputId = "featureInput2", label = "Select second feature (observed event)", choices = featureList, selected = "Epith.c.size"))),

  # First row
  fixedRow(
    column(6, plotlyOutput("Plot1", height = "600px")),
    column(6, plotlyOutput("Plot2", height = "600px"))),

  tags$hr(),
  tags$blockquote("First drag a selection box in the scatter plot to populate the barchart. Then select one of the bars in the barchat
                  to populate the boxplot"),


  # Second row
  fixedRow(
    column(3, plotlyOutput("Plot3", height = "600px")),
    column(9, plotlyOutput("Plot4", height = "600px"))))

# server.R definition
server <- function(input, output){

  # Observes the second feature input for a change
  observeEvent(input$featureInput2,{

    # Create a convenience data.frame which can be used for charting
    plot.df <- data.frame(BreastCancer[,input$featureInput1],
                          BreastCancer[,input$featureInput2],
                          Class = BreastCancer$Class)

    # Add column names
    colnames(plot.df) <- c("x", "y", "Class")

    # Do a plotly contour plot to visualize the two featres with
    # the number of malignant cases as size
    # Note the use of 'source' argument
    output$Plot1 <- renderPlotly({
      plot_ly(plot.df, x = ~x, y = ~y, mode = "markers", type = "scatter", color = ~Class, source = "subset",
              marker = list(size = 30)) %>%
        layout(title = paste(input$featureInput1, "vs ", input$featureInput2),
               xaxis = list(title = input$featureInput1),
               yaxis = list(title = input$featureInput2),
               dragmode =  "select",
               plot_bgcolor = "6A446F")
    })

    # Create a contour plot of the number of malignant cases
    output$Plot2 <- renderPlotly({

      plot.df %>%
        group_by(x, y, Class) %>%
        summarize(Count = n()) %>%
        filter(Class == "malignant") %>%
        plot_ly(x = ~x, y = ~y, z = ~Count, type = "contour") %>%
        layout(title = "Contour map of number of malignant cases",
               xaxis = list(title = input$featureInput1),
               yaxis = list(title = input$featureInput2))

    })

    # Assign to parent environment
    plot.df <<- plot.df
  })

  # Coupled event 1
  output$Plot3 <- renderPlotly({

    # Get subset based on selection
    event.data <- event_data("plotly_selected", source = "subset")

    # If NULL dont do anything
    if(is.null(event.data) == T) return(NULL)
    print(event.data)

    # # Get number of malignant and benign cases from selection
    malig.class <- subset(plot.df, Class == "malignant")[subset(event.data, curveNumber == 0)$pointNumber + 1,]
    benign.class <- subset(plot.df, Class == "benign")[subset(event.data, curveNumber == 1)$pointNumber + 1,]

    # Combine
    plot.subset <- rbind(malig.class, benign.class)

    # Summarize
    plot.summ <- plot.subset %>%
      group_by(x, y, Class) %>%
      summarize(Count = n())

    # Assign to parent frame
    plot.summ <<- plot.summ

    # Plot
    plot_ly(plot.summ, x = ~Class, y = ~Count, type = "bar", source = "select", color = ~Class) %>%
      layout(title = "No. of Malignant and Benign cases <br> in Selection",
             plot_bgcolor = "6A446F",
             yaxis = list(domain = c(0, 0.9)))
  })

  # Coupled event 2
  output$Plot4 <- renderPlotly({

    # Get subset based on selection
    event.data <- event_data("plotly_click", source = "select")

    # If NULL dont do anything
    if(is.null(event.data) == T) return(NULL)

    # If Malignant
    if(event.data[3] == "malignant"){

      tab <- subset(plot.summ, Class == "malignant")

      p1 <- plot_ly(tab, x = x, y = Count, type = "box", showlegend = F) %>%
        layout(yaxis = list(title = "Count"),
               xaxis = list(title = input$featureInput1))

      p2 <- plot_ly(tab, x = y, y = Count, type = "box", showlegend = F) %>%
        layout(title = "Box plot for Malignant cases",
               yaxis = list(title = "Count"),
               xaxis = list(title = input$featureInput2))

      subplot(p1, p2)
    }else{
      tab <- subset(plot.summ, Class == "benign")

      p1 <- plot_ly(tab, x = ~x, y = ~Count, type = "box", showlegend = F) %>%
        layout(yaxis = list(title = "Count"),
               xaxis = list(title = input$featureInput1))

      p2 <- plot_ly(tab, x = ~y, y = ~Count, type = "box", showlegend = F) %>%
        layout(title = "Box plot for Benign cases",
               yaxis = list(title = "Count"),
               xaxis = list(title = input$featureInput2))

      subplot(p1, p2)
    }

  })

}



runApp(shinyApp(ui, server))
runApp(shinyApp(ui, server), display.mode = "showcase")
runApp(shinyApp(ui, server), launch.browser = TRUE)
