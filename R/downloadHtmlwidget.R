#' @export
downloadHtmlwidgetUI <- function(id, text = "Download", class = NULL, display = c("buttons", "dropdown"),
                                 dropdownLabel = "Download", dropdownWidth = 150) {

  # indicators taken from https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
  ns <- shiny::NS(id)
  loadingGif <- loadingGif()

  formats_id <- ns("DownloadWidgethtml")
  formats_lb <- paste0(text, " HTML")
  names(formats_id) <- formats_lb
  choices_type <- "download"

  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))

  if (display == "dropdown") {
    dropdownActionInput(ns("dropdown"), dropdownLabel, choices = formats_id, choicesType = choices_type, width = dropdownWidth)
  } else {
    shiny::div(shiny::tagList(shiny::singleton(shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js")))),
               shiny::tagList(shiny::div(style = "text-align:center;",
                                         `data-for-btn` = formats_id,
                                         do.call(downloadButton, list(formats_id, formats_lb, class = class, style = "width: 200px; display: inline-block;")),
                                         shiny::span(class = "btn-loading-container",
                                                     shiny::img(src = loadingGif, class = "btn-loading-indicator", style = "display: none"),
                                                     shiny::HTML("<i class = 'btn-done-indicator fa fa-check' style='display: none'> </i>")))))
  }

}

#' @export
downloadHtmlwidgetServer <- function(id, element = NULL, file_prefix = "widget") {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    buttonId <- ns("downloadWidget")

    output$DownloadWidgethtml <- shiny::downloadHandler(
      filename = function() {
        session$sendCustomMessage('setButtonState', c('loading', buttonId))
        file_prefix <- eval_reactives(file_prefix)
        paste0(file_prefix, "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)), ".html")
      },
      content = function(file) {
        element <- eval_reactives(element)
        htmlwidgets::saveWidget(element, file = file, selfcontained = TRUE)
        session$sendCustomMessage('setButtonState', c('done', buttonId))
      })
  })

}
