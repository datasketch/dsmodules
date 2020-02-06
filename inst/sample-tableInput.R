library(shiny)
library(tidyverse)
library(dsAppModules)

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
                            list("File1"="sample1.csv","Archivo2"="sample2.csv"))
  output$debug <- renderPrint({
    inputData()
  })
}
shinyApp(ui,server)





