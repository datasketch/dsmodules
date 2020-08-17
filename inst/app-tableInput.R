library(shiny)
library(tidyverse)
library(dsmodules)
library(shinyinvoer)
## Data upload module



ui <- fluidPage(
  tableInputUI("dataIn", "Input Data 1", choices = list("Muestra" = "sampleData",
                                                        "Copiar & Pegar" = "pasted",
                                                        "Cargar" = "fileUpload",
                                                        "Google" = "googleSheets"),
               selected = "sampleData"),
  tableInputUI("dataIn2", "Input Data 2", choices = list("Muestra" = "sampleData",
                                                         "Copiar & Pegar" = "pasted",
                                                         "Cargar" = "fileUpload",
                                                         "Google" = "googleSheets"),
               selected = "sampleData"),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){

  inputData <- tableInputServer("dataIn",
                                sampleFiles = list( File1 = "data_sample/sample1.csv",
                                                    File2 = "data_sample/sample2.csv" ),
                                sampleSelected = "File1")

  inputData2 <- tableInputServer("dataIn2",
                                 sampleFiles = list(Cars = cars,
                                                     Mtcars = mtcars))

  output$debug <- renderPrint({
    l <- list(inputData(), inputData2())
    l
  })

}

shinyApp(ui,server)





