library(shiny)
library(tidyverse)
library(dsmodules)


ui <- fluidPage(imageInputUI("imageIn",
                             selected = "sampleData",
                             choices = list("Sample images" = "sampleData",
                                            "Load" = "fileUpload",
                                            "URL" = "url")),
                verbatimTextOutput("debug"),
                imageOutput("data_preview"))


server <- function(input, output, session) {

  inputImage <- callModule(imageInput,
                           "imageIn",
                           sampleFile = list("Tapete persa" = "data_sample/dgw.jpeg"),
                           infoList = list("url" =  "Image address"))

  output$debug <- renderImage({
    inputImage()
  }, deleteFile = FALSE)

  output$data_preview <- renderImage({
    inputImage()
  }, deleteFile = FALSE)

}


shinyApp(ui,server)





