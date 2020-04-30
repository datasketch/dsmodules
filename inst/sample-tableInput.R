library(shiny)
library(tidyverse)
library(dsmodules)

## Data upload module



ui <- fluidPage(
  tableInputUI("dataIn", choices = list("Copiar & Pegar" = "pasted",
                                        "Cargar" = "fileUpload",
                                        "Muestra" = "sampleData",
                                        "Google" = "googleSheets"),
               selected = "pasted"),
  verbatimTextOutput("debug"),
  downloadTableUI("download_data_button", "Descarga", formats = c("csv", "xlsx", "json"))
)

server <- function(input,output,session){
  inputData <- callModule(tableInput, "dataIn",
                          sampleFile =
                            list("File1"="data_sample/sample1.csv"))
  output$debug <- renderPrint({
    inputData()
  })

  callModule(downloadTable, "download_data_button", table = reactive(inputData()), formats = c("csv", "xlsx", "json"))

}

shinyApp(ui,server)





