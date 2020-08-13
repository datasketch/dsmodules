library(shiny)
library(tidyverse)
library(dsmodules)
library(shinyinvoer)
## Data upload module



ui <- fluidPage(
  tableInputUI("dataIn", choices = list("Copiar & Pegar" = "pasted",
                                        "Cargar" = "fileUpload",
                                        "Muestra" = "sampleData",
                                        "Google" = "googleSheets"),
               selected = "pasted"),
  verbatimTextOutput("debug")#,
  #downloadTableUI("download_data_button", "Descarga", formats = c("csv", "xlsx", "json"))
)

server <- function(input,output,session){


  inputData <- callModule(tableInput, "dataIn",
                          sampleFiles =
                            list("File1"="data_sample/sample1.csv", "File2" = "ab"  ),
                          aditional_info = list(ab = data.frame(num = c(1,2,4), cd = c("a", "b", "c"))))

  output$debug <- renderPrint({
    print(inputData())
  })

  #callModule(downloadTable, "download_data_button", table = reactive(inputData()), formats = c("csv", "xlsx", "json"))

}

shinyApp(ui,server)





