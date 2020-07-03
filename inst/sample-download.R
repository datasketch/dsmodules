library(shiny)
library(tidyverse)
library(dsmodules)
library(hgchmagic)
library(DT)
library(ggmagic)
library(shinyinvoer)


ui <- fluidPage(
  selectizeInput("data","Data", c("cars","mtcars")),
  textAreaInput("text", "Text", rows = 4),
  downloadTextUI("download_text", "Download", c("txt", "docx", "html")),
  downloadHtmlwidgetUI("download", text = "Iris"),
  downloadFileUI("downloadFile", "Iris File"),
  downloadHtmlwidgetUI("download_drop", label = "Descargar html", display = "dropdown"),
  downloadTableUI("download_table_drop", label = "Descargar tabla", formats = c("csv", "xlsx", "json"), display = "dropdown"),
  downloadImageUI("download_ggmagic_drop", label = "Descargar ggplot",  formats = c("jpeg", "pdf", "svg", "png"), display = "dropdown"),
  downloadImageUI("download_hgchmagic_drop", label = "Descargar highcharts",  formats = c("jpeg", "pdf", "png"), display = "dropdown"),
  downloadTextUI("download_text_drop", label = "Descargar texto", formats = c("txt", "docx", "html"), display = "dropdown"),
  downloadTableUI("download_table", label = "Descargar tabla", formats = c("csv", "xlsx", "json")),
  verbatimTextOutput("debug"),
  highchartOutput("img_hg"),
  radioButtons("test_id", "libreria", c('gg', 'hgch')),
  downloadImageUI("down_hgchmagic", "Descarga",  formats = c("jpeg", "pdf", "png")),
  plotOutput("img_gg"),
  downloadImageUI("down_ggmagic", "Descarga",  formats = c("jpeg", "pdf", "svg", "png"))#,
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
     hgch_bar_CatNum(sample_data("Cat-Num"))
  })

  output$img_hg <- renderHighchart({
    image_hg()
  })

  image_gg <- reactive({
    df <- data.frame(
      gp = factor(rep(letters[1:3], each = 10)),
      y = rnorm(30)
    )
    ggplot(df, aes(gp, y)) +
      geom_point()
   # print(gg_area_CatNum(sample_data("Cat-Num")))
  })

  output$img_gg <- renderPlot({
    # image_gg()
  })

 callModule(downloadImage, "down_hgchmagic", graph = image_hg(), lib = "highcharter", formats = c("jpeg", "pdf", "png"))
 callModule(downloadImage, "down_ggmagic", graph = image_gg(), lib = "ggplot", formats = c("jpeg", "pdf", "svg", "png"))
 callModule(downloadImage, "download_hgchmagic_drop", graph = image_hg(), lib = "highcharter", formats = c("jpeg", "pdf", "png"))
 callModule(downloadImage, "download_ggmagic_drop", graph = image_gg(), lib = "ggplot", formats = c("jpeg", "pdf", "svg", "png"))

 inputDataName <- reactive(input$data)

 callModule(downloadTable, "download_table", table = cars, formats = c("csv", "xlsx", "json"))
 callModule(downloadTable, "download_table_drop", table = cars, formats = c("csv", "xlsx", "json"))


 # callModule(downloadText,"download_text", text = reactive(input$text), formats = c("txt", "docx", "html"))
 callModule(downloadText,"download_text", text = input$text, formats = c("txt", "docx", "html"))
 callModule(downloadText,"download_text_drop", text = input$text, formats = c("txt", "docx", "html"))
 callModule(downloadHtmlwidget,"download", widget = widget)
 callModule(downloadHtmlwidget,"download_drop", widget = widget)

 callModule(downloadFile, "downloadFile", path = "htmlwidget.html", name = "myfile")

 output$debug <- renderPrint({
   wdata()
 })
}

shinyApp(ui,server)

