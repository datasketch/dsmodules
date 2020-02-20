library(shiny)
library(tidyverse)
library(dsmodules)


ui <- fluidPage(
  imageInputUI("imageIn",
               selected = "sampleData",
               choices = list("Sample images" = "sampleData",
                              "Load" = "fileUpload",
                              "URL" = "url")),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){
  inputImage <- callModule(imageInput,
                           "imageIn",
                           sampleFile = list("Tapete persa" = "https://placeimg.com/640/480/nature"))
  output$debug <- renderImage({
    inputImage()
  })
}
shinyApp(ui,server)





