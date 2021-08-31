library(shiny)
library(dsmodules)

ui <- fluidPage(
  h1("Hello params"),
  verbatimTextOutput("debug"),
  url_params_ui("url_params"),
  tableOutput("table")
)


server <- function(input,output,session){

  output$debug <- renderPrint({
    query <- get_url_params(session)
    #str(query)
    str(url_params())
    str(inputData())
  })

  url_params <- url_params_server("url_params")

  inputData <- reactive({
    #url_params <- list(data = "http://127.0.0.1:5389/tmp.csv")
    x <- url_params()
    url_params_data(x)
  })

  output$table <- renderTable({
    inputData()
  })


}
shinyApp(ui,server)

# data=[{"speed":4,"dist":2},{"speed":4,"dist":10},{"speed":7,"dist":4},{"speed":7,"dist":22}]
# data=http://127.0.0.1:5389/tmp.csv
