library(shiny)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(hgchmagic)
library(ggmagic)
library(reactable)
library(lfltmagic)

styles <- "

@import url('https://fonts.googleapis.com/css2?family=Lato&display=swap');

"

ui <- panelsPage(styles = styles,
                 panel(title = "Examples",
                       body = div(h3("Text"),
                                  textAreaInput("text", "Text", rows = 5),
                                  downloadTextUI("dropdown_texto", dropdownLabel = "Dropdown", formats = c("txt", "docx", "html"), display = "dropdown"),
                                  downloadTextUI("download_textoo", "Download", c("txt", "docx", "html")),
                                  br(),
                                  h3("Tables"),
                                  tableOutput("table"),
                                  downloadTableUI("dropdown_table", dropdownLabel = "Dropdown", formats = c("csv", "xlsx", "json"), display = "dropdown"),
                                  downloadTableUI("download_table", "Download", c("csv", "xlsx", "json")),
                                  br(),
                                  h3("Images"),
                                  br(),
                                  h3("Maps"),
                                  leafletOutput("leaflet"),
                                  downloadImageUI("dropdown_leaflet", dropdownLabel = "Dropdown", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown"),
                                  downloadImageUI("download_leaflet", "Download", c("jpeg", "pdf", "png", "html")),
                                  br(),
                                  h3("Interactive"),
                                  highchartOutput("highchart"),
                                  downloadImageUI("dropdown_highchart", dropdownLabel = "Dropdown", formats = c("jpeg", "pdf", "png", "html"), display = "dropdown"),
                                  downloadImageUI("download_highchart", "Download", c("jpeg", "pdf", "png", "html")),
                                  br(),
                                  h3("Static"),
                                  plotOutput("ggplot"),
                                  downloadImageUI("dropdown_ggplot", dropdownLabel = "Dropdown", formats = c("jpeg", "pdf", "png"), display = "dropdown"),
                                  downloadImageUI("download_ggplot", "Download", c("jpeg", "pdf", "png")),
                                  br(),
                                  h3("HTML"),
                                  reactableOutput("reactable"),
                                  downloadHtmlwidgetUI("dropdown_html", dropdownLabel = "Dropdown", display = "dropdown", formats = "html"),
                                  downloadHtmlwidgetUI("download_html", dropdownLabel = "Download", formats = "html")
                       ))
)

server <- function(input, output, session) {

  opts <- dsvizopts::merge_dsviz_options(text_family = "Lato", branding_include = TRUE,
                                         title = "display a title")

  hg <- hgch_bar_Cat(sample_data("Cat"))
  gg <- ggplot(data.frame(a = c("w", "r"), b = 2:3), aes(x = a, y = b, fill = a)) + geom_bar(stat = "identity")
  lflt <- lflt_choropleth_Gnm(sample_data("Gnm"), opts = opts)

  rc <- reactable(mtcars, style = "font-family: Lato;")

  output$table <- renderTable(data.frame(a = 1:3, b = "f"))
  output$highchart <- renderHighchart(hg)
  output$ggplot <- renderPlot(gg)
  output$reactable <- renderReactable(rc)
  output$leaflet <- renderLeaflet(lflt)

  downloadTextServer("download_textoo", element = reactive(input$text), formats = c("txt", "docx", "html"), file_prefix = "text")
  downloadTextServer("dropdown_texto", element = reactive(input$text), formats = c("txt", "docx", "html"))
  downloadTableServer("download_table", element = reactive(list(xx=data.frame(a = 1:3, b = "f"), yy=data.frame(a = 1:9, b = "Z"))), formats = c("csv", "xlsx", "json"), file_prefix = "table", zip = TRUE)
  downloadTableServer("dropdown_table", element = reactive(data.frame(a = 1:3, b = "f")), formats = c("csv", "xlsx", "json"))
  downloadImageServer("download_highchart", element = hg, lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "plot")
  downloadImageServer("dropdown_highchart", element = hg, lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"))
  downloadImageServer("download_leaflet", element = lflt, lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), file_prefix = "map", opts_theme = opts$theme)
  downloadImageServer("dropdown_leaflet", element = lflt, lib = "highcharter", formats = c("jpeg", "pdf", "png", "html"), opts_theme = opts$theme)
  downloadImageServer("download_ggplot", element = gg, lib = "ggplot", formats = c("jpeg", "pdf", "png"), file_prefix = "plot")
  downloadImageServer("dropdown_ggplot", element = gg, lib = "ggplot", formats = c("jpeg", "pdf", "png"))
  downloadHtmlwidgetServer("download_html", element = rc, formats = "html")
  downloadHtmlwidgetServer("dropdown_html", element = rc, formats = "html", file_prefix = "widget")

}


shinyApp(ui, server)
