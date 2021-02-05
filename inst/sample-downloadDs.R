library(shiny)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(dspins)
library(homodatum)


user_name <- "brandon"


ui <- panelsPage(panel(title = "Examples",
                       body = div(h3("Ds download module"),
                                  h4("Rendered both from server and from ui"),
                                  br(),
                                  br(),
                                  selectInput("select", "Select letter", letters[3:6]),
                                  uiOutput("download_server"),
                                  br(),
                                  uiOutput("download_server_default_modal_form"),
                                  br(),
                                  # rendered from UI
                                  downloadDsUI("download_ui",
                                               dropdownLabel = "Download",
                                               display = "dropdown",
                                               formats = c("txt", "docx", "html"),
                                               modalFullscreen = FALSE,
                                               modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                                               modalBody = list(textInput("slug", "Slug"),
                                                                selectInput("license", "License", choices = c("CC0", "CC-BY")),
                                                                selectizeInput("tags", "Tags", choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
                                                                selectizeInput("category", "Category", choices = list("No category" = "no-category"))))
                                  )),
                 panel(title = "E")
)

server <- function(input, output, session) {

  # rendered from server with customised modalBodyInputs
  output$download_server <- renderUI({
    downloadDsUI("download_0",
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                 dropdownLabel = "Download",
                 modalBodyInputs = c("name", "description", "sources"),
                 formats = c("csv", "xlsx", "json"))

  })

  # rendered from server with default modalBodyInputs: modalBodyInputs = c("name", "description", "sources", "license", "tags", "category")
  output$download_server_default_modal_form <- renderUI({
    downloadDsUI("download_save_pins",
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                 dropdownLabel = "Download",
                 formats = c("csv", "xlsx", "json"))

  })

  element_0 <- reactive({
    fringe(data.frame(a = 1:3, b = input$select))
  })


  dspin_urls_ <- function(x, user_name, ...) {
    x <- eval_reactives(x)
    f <- fringe(x)
    dspins_user_board_connect(user_name)
    Sys.setlocale(locale = "en_US.UTF-8")
    pins <- dspin_urls(element = f, user_name = user_name)
  }

  # env file needed for get link to work
  observe({
    req(element_0())
    downloadDsServer(id = "download_save_pins",
                     element = reactive(element_0()),
                     formats = c("csv", "xlsx", "json"),
                     errorMessage = "some error message",
                     modalFunction = dspin_urls_, reactive(element_0()),
                     user_name = user_name)
  })

  downloadDsServer(id = "download_ui", element = "Test \n one \n and one", formats = c("txt", "docx", "html"),
                   modalFunction = print, "Testing...")

  downloadDsServer(id = "download_0", element = "Test \n one \n and one", formats = c("txt", "docx", "html"),
                   modalFunction = print, "Testing...")

}


shinyApp(ui, server)
