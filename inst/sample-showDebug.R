library(shiny)
library(tidyverse)
library(dsmodules)

## hideDebug Module
## showDebug(hosts = c("127.0.0.1")) uncommented showDebug(hosts = c("XXXX")) commented: debugging and errors are shown
## showDebug(hosts = c("127.0.0.1")) commented showDebug(hosts = c("XXXX")) uncommented: debugging and errors aren't shown

ui <- fluidPage(
  h3("This is an app"),
  showDebug(hosts = c("127.0.0.1")),
  # showDebug(hosts = c("XXXX")),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){
  output$debug <- renderPrint({
    "This is debugging"
  })
}
shinyApp(ui,server)
