library(shiny)
library(tidyverse)

## Data upload module



ui <- fluidPage(
  textInput("text","Text"),
  uiOutput("tableInputSection"),
  tableOutput("debug")
)

server <- function(input,output){

  text <- reactive(input$text)

  sampleFiles <- list("sample1" = "data_sample/sample1.csv",
                      "sample2" = "data_sample/sample2.csv")
  # sampleFiles <- reactive({
  #   l <- sampleFiles
  #   names(l) <- c("F1",text())
  #   l
  # })

  inputData <- dsmodules::tableInputServer("dataIn",
                                           sampleFiles = sampleFiles)

  output$debug <- renderTable({
    req(inputData())
    inputData()
  })

  output$tableInputSection <- renderUI({
    choices <- list("P1"="pasted",
                   "FU"="fileUpload",
                   "SAMPLE"="sampleData")

    choiceNames <- c(text(),
                     "File Upload",
                     "Sample")

    names(choices) <- choiceNames

    dsmodules::tableInputUI("dataIn",
                            label = "some inut label",
                            choices = choices,
                            selected =  "sampleData")
  })

}
shinyApp(ui,server)





