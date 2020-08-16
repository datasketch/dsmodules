library(shiny)
library(shinypanels)
library(dsmodules)
library(shinyinvoer)
library(DT)

ui <-   panelsPage(
  panel(
    title = "Input Data",
    width = 250,
    body = list(
      tableInputUI("dataIn", "How do you want to upload data",
                   choices = list("Sample"="sampleData"),
                   selected = "sampleData"),
      verbatimTextOutput("debug")
    )
  ),
  panel(
    title = "Preview Palette",
    title_plugin = downloadTableUI("download_table", dropdownLabel = "Download",
                                   text = "Download", formats = c("link", "csv", "xlsx"),
                                   display = "dropdown", dropdownWidth = 170,
                                   getLinkLabel = "Get link",
                                   modalTitle = "Get link",
                                   modalBody = NULL),
    color = "chardonnay",
    can_collapse = FALSE,
    width = NULL,
    body = list(
      DT::dataTableOutput("table")
    )
  ),
  showDebug(hosts = c("127.0.0.1","randommonkey.shinyapps.io"))
)

server <- function(input, output, session) {
  inputData <- callModule(tableInput, "dataIn",
                          sampleFiles = list(Cars = cars, Mtcars = mtcars))

  output$debug <- renderPrint({
    inputData()
  })

  output$table <- DT::renderDataTable({
    inputData()
  })

  callModule(downloadTable, "download_table", table = inputData())

}

shinyApp(ui, server)


