library(shiny)
library(dsmodules)

ui <- fluidPage(getLinkUI("link"),
                uiOutput("iframe_preview"))

server <- function(input, output, session) {

  # callModule(getLink, "link")
  getLinkServer("link")

  output$iframe_preview <- renderUI({
    HTML(input$`link-iframe`)
  })

}

shinyApp(ui, server)

