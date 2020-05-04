library(shiny)
library(tidyverse)
library(dsmodules)


ui <- fluidPage(imageInputUI("imageIn",
                             selected = "sampleData",
                             choices = list("Sample images" = "sampleData",
                                            "Load" = "fileUpload",
                                            "URL" = "url")),
                verbatimTextOutput("debug"),
                imageOutput("data_preview"),
                downloadImageUI("download", "Descarga",  formats = c("jpeg", "pdf", "svg", "png")))


server <- function(input, output, session) {

  inputImage <- callModule(imageInput,
                           "imageIn",
                           sampleFile = list("Tapete persa" = "data_sample/h0.jpg",
                                             "Relán" = "data_sample/dgw.jpeg"),
                           infoList = list("url" =  "Image address"))

  output$debug <- renderImage({
    inputImage()
  }, deleteFile = FALSE)

  output$data_preview <- renderImage({
    inputImage()
  }, deleteFile = FALSE)

  observe({
    assign("e0", inputImage(), envir = globalenv())

  })
  callModule(downloadImage, "download", graph = inputImage(), lib = "ggplot", formats = c("jpeg", "pdf", "svg", "png"))

}


shinyApp(ui,server)





