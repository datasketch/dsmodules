#' @export
downloadHtmlwidgetUI <- function(id, text = "Download", class = NULL, label = "Download", display = "dropdown", dropdownWidth = 150) {

  # indicators taken from https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
  ns <- NS(id)
  loadingGif <- "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"

  if (is.null(formats)) formats <- "html"

  formats_id <- ns(paste0("DownloadWidget", formats))
  formats_lb <- text
  names(formats_id) <- formats_lb
  choices_type <- rep("download", length(formats_id))
  w0 <- which(formats %in% "link")
  if (sum(w0) > 0) {
    formats_lb[w0] <- getLinkLabel
    choices_type[w0] <- "button"
    if (display == "dropdown") {
      f0 <- c(formats_id[w0], "separator", formats_id[c(1:length(formats_id))[-w0]])
      names(f0) <- c(formats_lb[w0], "separator", formats_lb[c(1:length(formats_lb))[-w0]])
      choices_type <- c(choices_type[w0], NA, choices_type[c(1:length(choices_type))[-w0]])
      formats_id <- f0
    }
  }
  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))

  if (display == "dropdown") {
    ch <- ns("downloadHtmlwidget")
    names(ch) <- text
    dropdownActionInput("dropdown", label, choices = ch, width = dropdownWidth)
  } else {
    shiny::div(shiny::tagList(shiny::singleton(shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js")))),
               lapply(seq_along(choices_type), function(z) {
                 d <- ifelse(choices_type[z] == "button", "actionButton", "downloadButton")
                 shiny::tagList(shiny::div(style = "text-align:center;",
                                           `data-for-btn` = formats_id[z],
                                           do.call(d, list(formats_id[z], formats_lb[z], class = class, style = "width: 200px; display: inline-block;")),
                                           shiny::span(class = "btn-loading-container",
                                                       shiny::img(src = loadingGif, class = "btn-loading-indicator", style="display: none"),
                                                       shiny::HTML("<i class = 'btn-done-indicator fa fa-check' style='display: none'> </i>"))))
               }))
  }

}

#' @export
downloadHtmlwidget <- function(input, output, session, widget = NULL, formats, name = "widget") {

  ns <- session$ns

  lapply(formats, function(z) {
    if (z == "link") {
      observeEvent(input$DownloadWidgetlink, {
        showModal(modalDialog(getLinkUI(ns("link")), easyClose = TRUE, footer = NULL, size = "l"))
        getLinkServer("link")
      })
    } else {
      buttonId <- ns("downloadWidget")

      output$DownloadWidgethtml <- shiny::downloadHandler(
        filename = function() {
          session$sendCustomMessage('setButtonState', c('loading', buttonId))
          if (shiny::is.reactive(name))
            name <- name()
          paste0(name, "-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".html")
        },
        content = function(file) {
          if (shiny::is.reactive(widget))
            widget <- widget()
          htmlwidgets::saveWidget(widget, file = file, selfcontained = TRUE)
          session$sendCustomMessage('setButtonState', c('done', buttonId))
        })
    }
  })

}
