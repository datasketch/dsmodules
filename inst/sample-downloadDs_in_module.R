library(shiny)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(dspins)
library(reactable)

# in ui: "download_server-download_server-hello"
# in server: "download_example_module-download_example_module-download_server-download_example_module-download_server-hello"

user_name <- "brandon"
org_name <- "test"

downloadExampleUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("download"))
  )
}

downloadExampleServer <- function(id, r) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- NS(id)

      output$download <- renderUI({
        downloadDsUI(ns("download_server"),
                     display = "buttons",
                     modalFormatChoices = c("HTML" = "html", "PNG" = "png"),
                     dropdownLabel = "Download",
                     formats = c("html"))

      })


      observe({
        req(r$element_reactable)
        downloadDsServer(id = "download_server",
                         element = reactive(r$element_reactable),
                         formats = c("html"),
                         type = "dsviz",
                         user_name = user_name,
                         org_name = org_name)
      })

    }
  )
}



ui <- panelsPage(panel(title = "Examples",
                       body = div(h3("Ds download module called from shiny module"),
                                  br(),
                                  br(),
                                  downloadExampleUI("download_example_module")
                       )),
                 panel(title = "E")
)

server <- function(input, output, session) {


  r <- reactiveValues()

  element_reactable <- reactive({
    reactable::reactable(data.frame(a = 1:3, b = "r"))
  })

  observe({
    r$element_reactable <- element_reactable()
  })


  # Call download module
  downloadExampleServer("download_example_module", r = r)

}


shinyApp(ui, server)
