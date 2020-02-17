library(shiny)
library(tidyverse)
library(dsmodules)
library(DT)

## Data upload module



ui <- fluidPage(
  useShinyjs(),
  selectizeInput("data","Data", c("cars","mtcars")),
  actionButton("button","Regular Button"),
  actionBusyButtonUI("busyButtonReactive","Click me reactive"),
  actionBusyButtonUI("busyButton","Click me!"),
  verbatimTextOutput("debug")
)

server <- function(input,output,session){

  inputDataName <- reactive(input$data)

  outReactive <- callModule(actionBusyButton,"busyButtonReactive", inputDataName)

  out <- callModule(actionBusyButton,"busyButton", {
    Sys.sleep(.5)
    message("HELLO")
    paste("ran expression ", Sys.time())
  })

  observeEvent(input$button,{
    message(input$button)
  })

  output$debug <- renderPrint({
    #paste(inputDataName(), out())
    #inputDataName()
    #paste(outReactive(), out())
    out()
  })

}
shinyApp(ui,server)
