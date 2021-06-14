library(shiny)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(dspins)
library(homodatum)
library(hgchmagic)

user_name <- "brandon"
org_name <- "test"


ui <- panelsPage(shinyjs::useShinyjs(),
                 panel(title = "Examples",
                       body = div(h3("Ds download module"),
                                  h4("Rendered both from server and from ui"),
                                  br(),
                                  br(),
                                  selectInput("select", "Select letter", letters[3:6]),
                                  h5("Rendered from server, default modalBodyInputs and default modalFunction"),
                                  uiOutput("download_server_default_modal_form"),
                                  br(),
                                  h5("Rendered from server, subset of modalBodyInputs and default modalFunction"),
                                  uiOutput("download_server"),
                                  br(),
                                  h5("Rendered from UI, customised modalBody, customised modalFunction"),
                                  downloadDsUI("download_ui",
                                               dropdownLabel = "Download",
                                               display = "dropdown",
                                               formats = c("csv", "xlsx", "json"),
                                               modalFullscreen = FALSE,
                                               modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                                               modalBody = list(textInput("name", "Name"),
                                                                selectInput("license", "License", choices = c("CC0", "CC-BY")),
                                                                selectizeInput("tags", "Tags", choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
                                                                selectizeInput("category", "Category", choices = list("No category" = "no-category"))))
                       )),
                 panel(title = "E")
)

server <- function(input, output, session) {

  element_fringe <- reactive({
    data.frame(a = 1:3, b = input$select)
  })

  element_dsviz <- reactive({
    hgchmagic::hgch_bar_Cat(data.frame(a = c("b", "c")))
  })

  # function to be passed to modalFunction (alternative to using default modalFunction)
  dspin_urls_ <- function(x, user_name, ...) {
    x <- eval_reactives(x)
    f <- fringe(x)
    dspins_user_board_connect(user_name)
    Sys.setlocale(locale = "en_US.UTF-8")
    dspin_urls(element = f, user_name = user_name)
  }


  # rendered from server with customised modalBodyInputs
  output$download_server <- renderUI({
    downloadDsUI("download_0",
                 display = "buttons",
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                 dropdownLabel = "Download",
                 modalBodyInputs = c("name", "description", "sources", "category"),
                 categoryChoicesIDs = c("category1", "no-category", "category2"),
                 categoryChoicesLabels = c("Cat 1", "No category", "Cat 2"),
                 formats = c("html", "jpeg", "pdf", "png"))

  })

  # rendered from server with default modalBodyInputs: modalBodyInputs = c("name", "description", "sources", "license", "tags", "category")
  output$download_server_default_modal_form <- renderUI({
    downloadDsUI("download_save_pins",
                 display = "buttons",
                 modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                 max_inputs_first_column = 4,
                 dropdownLabel = "Download",
                 formats = c("html", "jpeg", "pdf", "png"))

  })


  observe({
    req(element_fringe())
    downloadDsServer(id = "download_ui",
                     element = reactive(element_fringe()),
                     formats = c("csv", "xlsx", "json"),
                     errorMessage = "some error message",
                     modalFunction = dspin_urls_, reactive(element_fringe()),
                     user_name = user_name)
  })

  observe({
    req(element_dsviz())
    downloadDsServer(id = "download_save_pins",
                     element = reactive(element_dsviz()),
                     formats = c("html", "jpeg", "pdf", "png"),
                     type = "dsviz",
                     user_name = user_name,
                     org_name = org_name)
  })

  # use default modal function to save as pin
  observe({
    req(element_dsviz())
    downloadDsServer(id = "download_0",
                     element = reactive(element_dsviz()),
                     formats = c("html", "jpeg", "pdf", "png"),
                     type = "dsviz",
                     user_name = user_name)
  })

}


shinyApp(ui, server)
