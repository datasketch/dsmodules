library(shiny)
library(tidyverse)
library(dsmodules)
library(hgchmagic)
library(DT)
library(ggmagic)

## Data upload module

ui <- fluidPage(
  selectizeInput("data","Data", c("cars","mtcars")),
  downloadHtmlwidgetUI("download", "Iris"),
  downloadFileUI("downloadFile", "Iris File"),
  downloadHtmlwidgetUI("download2", "cars or mtcars"),
  verbatimTextOutput("debug"),
  highchartOutput("img_hg"),
  radioButtons("test_id", "libreria", c('gg', 'hgch')),
  downloadImagesUI("down_hgchmagic", "Descarga",  formats = c("jpeg", "pdf", "png")),
  plotOutput("img_gg"),
  downloadImagesUI("down_ggmagic", "Descarga",  formats = c("jpeg", "pdf", "svg", "png"))
)

widget <- DT::datatable(iris)
htmlwidgets::saveWidget(widget, "htmlwidget.html")

server <- function(input,output,session){

  wdata <- reactive({
    if(input$data == "cars")
      return(DT::datatable(cars))
    else
      return(DT::datatable(mtcars))
  })

  image_hg <- reactive({
     hgch_bar_CatNum(sampleData("Cat-Num"))
  })

  output$img_hg <- renderHighchart({
    image_hg()
  })

  image_gg <- reactive({
   print(gg_area_CatNum(sampleData("Cat-Num")))
  })

  output$img_gg <- renderPlot({
    image_gg()
  })
  observe({assign("e0", image_gg(), envir = globalenv())})

 callModule(downloadImages, "down_hgchmagic", graph = image_hg(), lib = "highcharter", formats = c("jpeg", "pdf", "png"))
 callModule(downloadImages, "down_ggmagic", graph = image_gg(), lib = "ggplot", formats = c("jpeg", "pdf", "svg", "png"))

 inputDataName <- reactive(input$data)

 callModule(downloadHtmlwidget,"download", widget = widget)
 callModule(downloadHtmlwidget,"download2", widget = wdata, name = inputDataName)

 callModule(downloadFile, "downloadFile", path = "htmlwidget.html", name = "myfile")

 output$debug <- renderPrint({
   wdata()
 })
}

shinyApp(ui,server)

