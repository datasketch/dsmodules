library(shiny)
library(tidyverse)
library(dsmodules)
library(shinyinvoer)
## Data upload module



ui <- fluidPage(
  column(6,
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
         checkboxGroupInput("reactive_dataset", "",
                            choices = c("Iris", "Cars", "Mtcars"
                            ), selected = c("Cars", "Mtcars")),
         tableInputUI("dataIn3", "Input Data 3", choices = list("Muestra" = "sampleData",
                                                                "Copiar & Pegar" = "pasted",
                                                                "Cargar" = "fileUpload",
                                                                "Google" = "googleSheets"),
                      selected = "sampleData")
  ),
  column(6,
         verbatimTextOutput("debug")
  )
)

server <- function(input,output,session){

  dataset <- reactive({
    datasets <- input$reactive_dataset
    l <- lapply(datasets, function(x){
      if(x == "Cars") return(cars)
      if(x == "Mtcars") return(mtcars)
      iris
    })
    names(l) <- datasets
    l
  })

  inputData <- tableInputServer("dataIn",
                                sampleFiles = list( File1 = "data_sample/sample1.csv",
                                                    File2 = "data_sample/sample2.csv" ),
                                sampleSelected = "File1")

  inputData2 <- tableInputServer("dataIn2",
                                 sampleFiles = list(Cars = cars,
                                                    Mtcars = mtcars))

  inputData3 <- tableInputServer("dataIn3",
                                 sampleFiles = dataset)
  output$debug <- renderPrint({
    l <- list(inputData1 = inputData(),
              inputData2 = inputData2(),
              inputData3 = inputData3())
    l
  })

}

shinyApp(ui,server)





