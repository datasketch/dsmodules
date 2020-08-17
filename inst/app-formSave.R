library(shiny)
library(shinypanels)
library(dsmodules)
library(shinyinvoer)
library(DT)

ui <-   panelsPage(
  modal(id = 'saveModal', title = 'Get Link Modal',
        div(style = "display: flex; flex-direction: row; justify-content: space-between;",
            formUI("my_form", "Form label", button_label = "Submit",
                   input_list = list(
                     selectInput("formSelect", "Form Select", choices = c("A","B")),
                     textInput("formText", "Form Text", value = "Some text"))),
            div(style = "width: 50%",
                uiOutput("result")
            )
        )
  ),
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
    title_plugin = shinypanels::modalButton(id = "ss", modal_id = 'saveModal', label = 'Save Data'),
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
  inputData <- tableInputServer("dataIn",
                                sampleFiles = list(Cars = cars, Mtcars = mtcars))

  output$debug <- renderPrint({
    inputData()
  })

  output$table <- DT::renderDataTable({
    inputData()
  })

  do_something_with_form_values <- function(..., text = ""){
    form_inputs <- list(...)
    # list(...) has a list with the current input values
    Sys.sleep(1)
    HTML(paste0(h2(text),
                paste(names(form_inputs), form_inputs, sep = "=", collapse = "<br>")
    ))
  }

  result <- formServer("my_form", FUN = do_something_with_form_values, text = "Form values: ")

  output$result <- renderUI({
    result()
  })

}

shinyApp(ui, server)


