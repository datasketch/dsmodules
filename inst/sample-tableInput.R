library(shiny)
library(tidyverse)
library(dsmodules)

## Data upload module



ui <- fluidPage(
  tableInputUI("dataIn", choices = list("Copiar & Pegar"="pasted",
                                        "Cargar"="fileUpload",
                                        "Muestra"="sampleData"),
               selected = "pasted"),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){
  inputData <- callModule(tableInput, "dataIn",
                          sampleFile =
                            list("File1"="data_sample/sample1.csv"))
  output$debug <- renderPrint({
    inputData()
  })
}
shinyApp(ui,server)





