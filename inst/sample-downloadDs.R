library(shiny)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(dspins)
library(homodatum)


user_name <- "brandon"


ui <- panelsPage(panel(title = "Examples",
                       body = div(h3("Ds download module"),
                                  h4("Renderedboth from server and from ui"),
                                  br(),
                                  br(),
                                  selectInput("select", "Select letter", letters[3:6]),
                                  uiOutput("download_server"),
                                  br(),
                                  br(),
                                  downloadDsUI("download_ui",
                                               dropdownLabel = "Download",
                                               display = "dropdown",
                                               formats = c("txt", "docx", "html"),
                                               modalBody = list(textInput("slug", "Slug"),
                                                                textInput("description", "Description"),
                                                                selectInput("license", "License", choices = c("CC0", "CC-BY")),
                                                                selectizeInput("tags", "Tags", choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
                                                                selectizeInput("category", "Category", choices = list("No category" = "no-category"))))
                       ))
)

server <- function(input, output, session) {

  output$download_server <- renderUI({
    downloadDsUI("download_0",
                 dropdownLabel = "Download",
                 formats = c("csv", "xlsx", "json"))

  })

  downloadDsServer(id = "download_ui", element = "Test \n one \n and one", formats = c("txt", "docx", "html"),
                   modalFunction = print, "Testing...")

  element_0 <- reactive({
    data.frame(a = 1:3, b = input$select)
  })

  # env file needed for get link to work
  downloadDsServer(id = "download_0", element = reactive(element_0()), formats = c("csv", "xlsx", "json"),
                   modalFunction = dspin_urls, fringe(element_0()), user_name)

}


shinyApp(ui, server)
