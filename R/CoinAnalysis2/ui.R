# ui.R definition
ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),

  # row for first plot
  fixedRow(
    column(12, plotlyOutput("Plot", height="600px"))
  )
)
