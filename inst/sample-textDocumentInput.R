library(shiny)
library(tidyverse)
library(dsmodules)


ui <- fluidPage(textDocumentInputUI("initial_data",
                                    choices = list("Muestra" = "sampleData",
                                                   "Copiar & pegar" = "pasted",
                                                   "URL (scraping de párrafos)" = "url",
                                                   "Cargar" = "fileUpload"
                                                   #"GoogleSheet" = "googleSheet",
                                                   #"Mi librería" = "dsLibrary"
                                    ),
                                    selected = "sampleData"),
                verbatimTextOutput("debug"),
                uiOutput("data_preview", style = "padding: 20px; width: 450px;"))


server <- function(input, output, session) {

  datasetInput <- callModule(textDocumentInput,
                             "initial_data",
                             sampleFile = list("Montés" = "data_sample/mdr.txt"),
                             infoList = list("pasted" = "",
                                             "fileUpload" = "Importar archivos de texto (.doc, .txt, .pdf)",
                                             "sampleData" = "",
                                             "url" = "Se extraen los párrafos (el contenido de los HTML tags p) de la página web"))


  output$debug <- renderUI({
    datasetInput()
  })

  output$data_preview <- renderUI({
    datasetInput()
  })

}


shinyApp(ui,server)





