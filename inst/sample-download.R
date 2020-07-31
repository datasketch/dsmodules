library(shiny)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(hgchmagic)
library(ggmagic)


ui <- panelsPage(panel(title = "Examples",
                       body = div(h3("Text"),
                                  textAreaInput("text", "Text", rows = 5),
                                  downloadTextUI("dropdown_texto", dropdownLabel = "Dropdown", formats = c("txt", "docx", "html", "link"), display = "dropdown",
                                                 modalTitle = "R...", modalBody = selectInput("f", "First", 1:3)),
                                  downloadTextUI("download_text", "Download", c("txt", "docx", "html", "link")),
                                  br(),
                                  h3("Tables"),
                                  tableOutput("table"),
                                  downloadTableUI("dropdown_table", dropdownLabel = "Dropdown", formats = c("csv", "xlsx", "json", "link"), display = "dropdown"),
                                  downloadTableUI("download_table", "Download", c("csv", "xlsx", "json", "link")),
                                  br(),
                                  h3("Images"),
                                  br(),
                                  h3("Interactive"),
                                  highchartOutput("highchart"),
                                  downloadImageUI("dropdown_highchart", dropdownLabel = "Dropdown", formats = c("jpeg", "pdf", "png", "html", "link"), display = "dropdown"),
                                  downloadImageUI("download_highchart", "Download", c("jpeg", "pdf", "png", "html", "link")),
                                  br(),
                                  h3("Static"),
                                  plotOutput("ggplot"),
                                  downloadImageUI("dropdown_ggplot", dropdownLabel = "Dropdown", formats = c("jpeg", "pdf", "png", "link"), display = "dropdown"),
                                  downloadImageUI("download_ggplot", "Download", c("jpeg", "pdf", "png", "link")),
                                  br(),
                                  h3("HTML"),
                                  downloadHtmlwidgetUI("download_data_button", dropdownLabel = "lb", text = paste("dw", "HTML"), formats = c("link", "html"),
                                                       display = "dropdown", dropdownWidth = 170, getLinkLabel = "gl", modalTitle = "gl"),
                                  downloadHtmlwidgetUI("dropdown_html", dropdownLabel = "Dropdown", formats = c("html", "link"), display = "dropdown", text = "FASDFSA"),
                                  downloadHtmlwidgetUI("download_html", dropdownLabel = "Download", c("html", "link"), text = "FASDFSA")
                       ))
)

server <- function(input, output, session) {

  hg <- hgch_bar_Cat(sample_data("Cat"))
  gg <- ggplot(data.frame(a = c("w", "r"), b = 2:3), aes(x = a, y = b, fill = a)) + geom_bar(stat = "identity")

  output$table <- renderTable(data.frame(a = 1:3, b = "f"))
  output$highchart <- renderHighchart(hg)
  output$ggplot <- renderPlot(gg)

  callModule(downloadText, "download_text", text = reactive(input$text), formats = c("txt", "docx", "html", "link"),
             modalFunction = paste, modalFunctionArgs = c("printing...", "this"))
  callModule(downloadText, "dropdown_text", text = reactive(input$text), formats = c("txt", "docx", "html", "link"))
  callModule(downloadTable, "download_table", table = data.frame(a = 1:3, b = "f"), formats = c("csv", "xlsx", "json", "link"),
             modalFunction = print, modalFunctionArgs = "printing...")
  callModule(downloadTable, "dropdown_table", table = data.frame(a = 1:3, b = "f"), formats = c("csv", "xlsx", "json", "link"))
  callModule(downloadImage, "download_highchart", graph = hg, lib = "highcharter", formats = c("jpeg", "pdf", "png", "html", "link"),
             modalFunction = print, modalFunctionArgs = "printing...")
  callModule(downloadImage, "dropdown_highchart", graph = hg, lib = "highcharter", formats = c("jpeg", "pdf", "png", "html", "link"))
  callModule(downloadImage, "download_ggplot", graph = gg, lib = "ggplot", formats = c("jpeg", "pdf", "png", "link"),
             modalFunction = print, modalFunctionArgs = "printing...")
  callModule(downloadImage, "dropdown_ggplot", graph = gg, lib = "ggplot", formats = c("jpeg", "pdf", "png", "link"))
  callModule(downloadHtmlwidget, "download_html", widget = hg, formats = c("html", "link"),
             modalFunction = print, modalFunctionArgs = "printing...")
  callModule(downloadHtmlwidget, "dropdown_html", widget = hg, formats = c("html", "link"))

}


shinyApp(ui, server)
