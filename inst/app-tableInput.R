library(shiny)
library(tidyverse)
library(dsmodules)
library(shinyinvoer)
library(shinyjs)
## Data upload module



ui <- fluidPage(
  shinyjs::useShinyjs(),
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
         uiOutput("debug")
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
                                showAdvancedOptionsButton = TRUE,
                                sampleFiles = list( File1 = "data_sample/sample1.csv",
                                                    File2 = "data_sample/sample2.csv" ),
                                sampleSelected = "File1")

  inputData2 <- tableInputServer("dataIn2",
                                 sampleFiles = list(Cars = cars,
                                                    Mtcars = mtcars))

  inputData3 <- tableInputServer("dataIn3",
                                 sampleFiles = dataset)


  output$tableInputData <- renderTable(inputData())
  output$tableInputData1 <- renderTable(inputData2())
  output$tableInputData2 <- renderTable(inputData3())

  output$debug <- renderUI({
    div(tableOutput("tableInputData"),
        tableOutput("tableInputData1"),
        tableOutput("tableInputData2"))
  })

}

shinyApp(ui,server)





