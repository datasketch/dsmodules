library(shiny)
library(dsmodules)
library(shinyinvoer)
library(shinypanels)
library(hgchmagic)
library(ggmagic)


ui <- panelsPage(panel(title = "Examples",
                       body = div(h3("Text"),

                                  highchartOutput("highchart"),
                                  downloadImageUI("download_highchart", "Download", c("jpeg", "pdf", "png", "html", "link"),
                                                  modalBody = list(textInput("slug2", "Slug")),
                                                  nameLabel = "NAME LABEL...",
                                                  saveButtonLabel = "SAVE BUTTON LABEL...",
                                                  linkLabel = "LINK LABEL...",
                                                  iframeLabel = "IFRAME LABEL..."),
                                  br(),
                                  h3("Static"),
                                  plotOutput("ggplot"),
                                  downloadImageUI("dropdown_ggplot", dropdownLabel = "Dropdown", formats = c("jpeg", "pdf", "png", "link"), display = "dropdown",
                                                  modalBody = list(textInput("slug", "Slug")),
                                                  nameLabel = "NAME LABEL...",
                                                  saveButtonLabel = "SAVE BUTTON LABEL...",
                                                  linkLabel = "LINK LABEL...",
                                                  iframeLabel = "IFRAME LABEL..."),
                                  downloadImageUI("download_ggplot", "Download", c("jpeg", "pdf", "png", "link")),
                                  br(),
                                  h3("HTML"),
                                  downloadHtmlwidgetUI("dropdown_html", dropdownLabel = "Dropdown", formats = c("html", "link"), display = "dropdown"),
                                  downloadHtmlwidgetUI("download_html", dropdownLabel = "Download", formats = c("html", "link"),
                                                       modalBody = list(textInput("slug3", "Slug")),
                                                       nameLabel = "NAME LABEL...",
                                                       saveButtonLabel = "SAVE BUTTON LABEL...",
                                                       linkLabel = "LINK LABEL...",
                                                       iframeLabel = "IFRAME LABEL...")
                       ))
)

server <- function(input, output, session) {

  hg <- hgch_bar_Cat(sample_data("Cat"))
  gg <- ggplot(data.frame(a = c("w", "r"), b = 2:3), aes(x = a, y = b, fill = a)) +
    geom_bar(stat = "identity")

  output$table <- renderTable(data.frame(a = 1:3, b = "f"))
  output$highchart <- renderHighchart(hg)
  output$ggplot <- renderPlot(gg)

  callModule(downloadText, "download_textoo", text = reactive(input$text), name = "text",
             formats = c("txt", "docx", "html", "link"),
             modalFunction = paste, "printing...")#, "this")
  callModule(downloadText, "dropdown_texto", text = reactive(input$text),
             formats = c("txt", "docx", "html", "link"))
  callModule(downloadTable, "download_table", table = data.frame(a = 1:3, b = "f"),
             name = "table", formats = c("csv", "xlsx", "json", "link"),
             modalFunction = print, "printing...")
  callModule(downloadTable, "dropdown_table", table = data.frame(a = 1:3, b = "f"),
             formats = c("csv", "xlsx", "json", "link"))
  callModule(downloadImage, "download_highchart", graph = hg, lib = "highcharter",
             name = "plot", formats = c("jpeg", "pdf", "png", "html", "link"),
             modalFunction = print, "printing...")
  callModule(downloadImage, "dropdown_highchart", graph = hg, lib = "highcharter",
             formats = c("jpeg", "pdf", "png", "html", "link"))
  callModule(downloadImage, "download_ggplot", graph = gg, lib = "ggplot", name = "plot",
             formats = c("jpeg", "pdf", "png", "link"),
             modalFunction = paste, "printing", "this")
  callModule(downloadImage, "dropdown_ggplot", graph = gg, lib = "ggplot",
             formats = c("jpeg", "pdf", "png", "link"))
  callModule(downloadHtmlwidget, "download_html", widget = hg,
             formats = c("html", "link"))
  callModule(downloadHtmlwidget, "dropdown_html", widget = hg,
             formats = c("html", "link"), name = "widget",
             modalFunction = paste0, "printing...", "this")

}


shinyApp(ui, server)
