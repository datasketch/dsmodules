library(shiny)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(hgchmagic)
library(hgchmagic)
library(reactable)
library(lfltmagic)

styles <- "

@import url('https://fonts.googleapis.com/css2?family=Lato&display=swap');

"

ui <- panelsPage(styles = styles,
                 panel(title = "Examples",
                       body = div(
                         highchartOutput("vizExp"),
                         dsmodules::downloadDistinctFormatsUI("dropdown_table",
                                                              dropdownLabel = "Descargar",
                                                              text = c( "Descarga png", "Descarga jpg", "Descarga csv", "csv2"),
                                                              formats = c( "png", "jpg", "csv", "csv "),
                                                              display = "dropdown", dropdownWidth = 300)
                       )
                 ))

server <- function(input, output, session) {


  hgchViz <- reactive({
    hgch_treemap_Cat(sample_data("Cat"))
  })

  output$vizExp <- renderHighchart({
    hgchViz()
  })


  df <- reactive({
    iris
  })

  df2 <- reactive({
    mtcars
  })


  # element <- list("vizz1" = 1, "viz2" = 2, "table" = 4)
  # formats <- list("vizz1" = c("html", "png", "jpg"), "vizz2" = c("png", "jpg"), "table" = c("csv", "xlsx"))
  # lib <- list("vizz1" = "highcharter")
  observe({
    dsmodules::downloadDistinctFormatsServer("dropdown_table",
                                            element = list("vizz1" = hgchViz(),  "table" = df(), "table2" = df2()),
                                            formats = list("vizz1" = c( "png", "jpg"),  "table" = "csv", "tbae" = "csv "),
                                            lib = list("vizz1" = "highcharter", "table" = "table", "tbae" = "table")
    )
  })
}


shinyApp(ui, server)
