library(shiny)

shinyServer(function(input, output) {
  
  # Initialize list of inputs
  inputTagList <- tagList()
  
  output$allInputs <- renderUI({
    # Get value of button, which represents number of times pressed (i.e. number of inputs added)
    i <- input$appendInput
    # Return if button not pressed yet
    if(is.null(i) || i < 1) return()
    # Define unique input id and label
    newInputId <- paste0("input", i)
    newInputLabel <- paste("Input", i)
    # Define new input
    newInput <- selectInput(newInputId, newInputLabel, c("Option 1", "Option 2", "Option 3"))
    # Append new input to list of existing inputs
    inputTagList <<- tagAppendChild(inputTagList, newInput)
    # Return updated list of inputs
    inputTagList
  })
  
})
