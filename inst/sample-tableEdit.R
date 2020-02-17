library(shiny)
library(dsmodules)
library(tidyverse)
library(rhandsontable)

# Data edit module



ui <- fluidPage(
  tableEditUI("tableEditable"),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){
  inputData <- reactive({
    mtcars
  })

  data <- callModule(tableEdit, "tableEditable", inputData = inputData,
                     addColSelect = TRUE,
                     selectColsLabel = "Seleccione y organice columnas",
                     addRowFilters = TRUE,
                     addCtypes = TRUE)
  output$debug <- renderPrint({
    data()
  })
}
shinyApp(ui,server)


