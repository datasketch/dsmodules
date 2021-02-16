#' @export
downloadHtmlwidgetUI <- function(id, text = "Download", formats = NULL, class = NULL, display = c("buttons", "dropdown"),
                                 dropdownLabel = "Download", dropdownWidth = 150) {

  # indicators taken from https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
  ns <- shiny::NS(id)
  loadingGif <- loadingGif()

  if (is.null(formats)) formats <- "html"

  formats_id <- ns(paste0("DownloadWidget", formats))
  formats_lb <- paste0(text, " ", toupper(formats))
  names(formats_id) <- formats_lb
  choices_type <- rep("download", length(formats_id))

  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))

  if (display == "dropdown") {
    shinyinvoer::dropdownActionInput(ns("dropdown"), dropdownLabel, choices = formats_id, choicesType = choices_type, width = dropdownWidth)
  } else {
    shiny::div(shiny::tagList(shiny::singleton(shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js")))),
               lapply(seq_along(choices_type), function(z) {
                 d <- ifelse(choices_type[z] %in% c("button"), "actionButton", "downloadButton")
                 shiny::tagList(shiny::div(style = "text-align:center;",
                                           `data-for-btn` = formats_id[z],
                                           do.call(d, list(formats_id[z], formats_lb[z], class = class, style = "width: 200px; display: inline-block;")),
                                           shiny::span(class = "btn-loading-container",
                                                       shiny::img(src = loadingGif, class = "btn-loading-indicator", style = "display: none"),
                                                       shiny::HTML("<i class = 'btn-done-indicator fa fa-check' style='display: none'> </i>"))))
               }))
  }

}

#' @export
downloadHtmlwidgetServer <- function(id, element = NULL, formats, file_prefix = "widget") {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lapply(formats, function(z) {
      buttonId <- ns("downloadWidget")

      output$DownloadWidgethtml <- shiny::downloadHandler(
        filename = function() {
          session$sendCustomMessage('setButtonState', c('loading', buttonId))
          file_prefix <- eval_reactives(file_prefix)
          paste0(file_prefix, "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
        },
        content = function(file) {
          element <- eval_reactives(element)
          htmlwidgets::saveWidget(element, file = file, selfcontained = TRUE)
          session$sendCustomMessage('setButtonState', c('done', buttonId))
        })
    }
    )
  })

}
