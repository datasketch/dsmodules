library(shiny)
library(shinypanels)
library(dsmodules)

ui <- panelsPage(panel(title = "Test",
                       body = div(getLinkUI("link"),
                                  icon(""),
                                  uiOutput("iframe_preview"))))

server <- function(input, output, session) {

  # observeEvent(input$`link-save`, {
  # r <- input$`link-name`
  # observe({
# print(input$`link-name`)

  getLinkServer("link", FUN = paste0, "https://es.wikipedia.org/wiki/", input$`link-name`)
  # })

  output$iframe_preview <- renderUI({
    HTML(input$`link-iframe`)
  })

}

shinyApp(ui, server)

