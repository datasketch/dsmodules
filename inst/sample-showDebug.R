library(shiny)
library(tidyverse)
library(dsAppModules)

## hideDebug Module

ui <- fluidPage(
  h3("This is an app"),
  useShinyjs(),
  showDebugUI("showDebug"),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){
  callModule(showDebug, "showDebug", hosts = c("127.0.0.1"))
  #callModule(showDebug, "showDebug", hosts = c("XXXX"))
  output$debug <- renderPrint({
    "This is debugging"
  })
}
shinyApp(ui,server)
