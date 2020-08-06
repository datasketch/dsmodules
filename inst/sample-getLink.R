library(shiny)
library(shinypanels)
library(dsmodules)

ui <- panelsPage(panel(title = "Test",
                       body = div(getLinkUI("link",
                                            infoInputs = list(textInput("slug", "Slug"),
                                                              div(style = "color: green; font-size: 17px;",
                                                              textInput("description", "Description")),
                                                              selectInput("license", "License", choices = c("CC0", "CC-BY"))),
                                            nameLabel = "NAME LABEL",
                                            saveButtonLabel = "SAVE BUTTON LABEL",
                                            linkLabel = "LINK LABEL",
                                            iframeLabel = "IFRAME LABEL"),
                                  icon(""),
                                  uiOutput("iframe_preview"))))

server <- function(input, output, session) {

  getLinkServer("link", FUN = paste0, "https://es.wikipedia.org/wiki/", input$`link-name`)

  output$iframe_preview <- renderUI({
    HTML(input$`link-iframe`)
  })

}

shinyApp(ui, server)

