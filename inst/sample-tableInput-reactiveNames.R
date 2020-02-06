library(shiny)
library(tidyverse)

## Data upload module



ui <- fluidPage(
  textInput("text","Text"),
  uiOutput("tableInputSection"),
  verbatimTextOutput("debug")
)

server <- function(input,output){

  text <- reactive(input$text)

  sampleFiles <- list("sample1.csv","sample2.csv")
  sampleFiles <- reactive({
    l <- list("sample1.csv","sample2.csv")
    names(l) <- c("F1",text())
    l
  })

  inputData <- callModule(tableInput, "dataIn",
                          sampleFiles = sampleFiles
  )
  output$debug <- renderPrint({
    inputData()
  })

  output$tableInputSection <- renderUI({
    choiceNames <- c(text(),
                     "File Upload",
                     "Sample")
    tagList(
      tableInputUI("dataIn",
                   choices = list("P1"="pasted",
                                  "FU"="fileUpload",
                                  "SAMPLE"="sampleData"),
                   selected = "sampleData")
    )
  })

}
shinyApp(ui,server)





