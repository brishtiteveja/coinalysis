# ui.R definition
ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),

  # Feature selection
  fixedRow(
    column(4, selectInput(inputId = "coinInput1", label = "Select first coin", choices = coinList, selected = coinList[1])),
    column(4, selectInput(inputId = "coinInput2", label = "Select second coin", choices = coinList, selected = coinList[2])),
    column(4, selectInput(inputId = "coinMetric", label = "Select coin metric", choices = coinMetrics, selected = coinMetrics[1]))
  ),
  # row for first plot
  fixedRow(
    column(12, plotlyOutput("Plot1", height="500px"))
  ),
  fixedRow(
    column(6, plotlyOutput("Plot3")),
    column(6, plotlyOutput("Plot2"))
  )
)
