library(shiny)
library(shinypanels)
library(dsmodules)


ui <-   panelsPage(
  panel(
    title = "Input Data",
    width = 350,
    body = list(
      formUI("my_form", "Form label", button_label = "Submit",
             input_list = list(selectInput("formSelect", "Form Select", choices = c("A","B")),
                               textInput("formText", "Form Text", value = "Some text"))
      ),
      verbatimTextOutput("debug")
    )
  ),
  panel(
    title = "Preview Palette",
    title_plugin = NULL,
    color = "chardonnay",
    can_collapse = FALSE,
    width = NULL,
    body = list(
      uiOutput("result")
    )
  ),
  showDebug(hosts = c("127.0.0.1","randommonkey.shinyapps.io"))
)

server <- function(input, output, session) {


  do_something_with_form_values <- function(..., text = ""){
    form_inputs <- list(...)
    # list(...) has a list with the current input values
    Sys.sleep(1)
    HTML(paste0(h2(text),
           paste(names(form_inputs), form_inputs, sep = "=", collapse = "<br>")
           ))
    # uncomment for error
    # errorCondition("There is and error")
  }

  # if errorCondition in do_something_with_form_values uncommented:
  # - to show error message let errorMessage be NULL
  # - to show defined error message pass it through errorMessage argument
  result <- formServer("my_form",
                       errorMessage = NULL,
                       # errorMessage = "THIS message",
                       FUN = do_something_with_form_values,
                       text = "Form values: ")

  output$debug <- renderPrint({
    result()
  })

  output$result <- renderUI({
    result()
  })

}

shinyApp(ui, server)


