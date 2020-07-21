#' @export
downloadImageUI <- function(id, text = "Download", formats = NULL, class = NULL, display = c("buttons", "dropdown"), dropdownLabel = "Download", dropdownWidth = 150, getLinkLabel = "Get link") {

  ns <- NS(id)
  loadingGif <- "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"

  if (is.null(formats)) formats <- "png"

  formats_id <- ns(paste0("DownloadImg", formats))
  formats_lb <- paste0(text, " ", toupper(formats))
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
    dropdownActionInput(ns("dropdown"), dropdownLabel, choices = formats_id, choicesType = choices_type, width = dropdownWidth)
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


#' @param type Image library
#' @export
downloadImage <- function(input, output, session, graph = NULL, lib = NULL, formats,  name = "plot") {

  ns <- session$ns
  img_format <- formats

  lapply(img_format, function(z) {
    if (z == "link") {
      observeEvent(input$DownloadImglink, {
        showModal(modalDialog(getLinkUI(ns("link")), easyClose = TRUE, footer = NULL, size = "l"))
        getLinkServer("link")
      })
    } else {
      buttonId <- ns(paste0("DownloadImg", z))

      output[[paste0("DownloadImg", z)]] <- shiny::downloadHandler(
        filename = function() {
          session$sendCustomMessage('setButtonState', c('loading', buttonId))
          if (shiny::is.reactive(name))
            name <- name()
          paste0(name, "-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
        },
        content = function(file) {
          if (shiny::is.reactive(graph))
            graph <- graph()
          if (lib == "highcharter") {
            saveInteractive(viz = graph, filename = file, format = z)
          } else if (lib == "ggplot") {
            saveStatic(viz = graph, filename = file, format = z)
          } else {
            return()
          }
          session$sendCustomMessage('setButtonState', c('done', buttonId))
        }
      )
    }
  })

}


#' @export
saveInteractive <- function(viz, filename, format = NULL, width = 660, height = 500, ...) {

  if (is.null(format)) {
    format <- tools::file_ext(filename) %||% "png"
  }
  filename <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", filename)
  tmp <- paste(tempdir(), 'html', sep ='.')
  htmltools::save_html(viz, tmp)
  tmpSave <- filename
  if (format == 'html') {
    htmlwidgets::saveWidget(viz, paste0(filename, ".", format))
  } else {
    webshot::webshot(tmp, paste0(filename, ".", format), vwidth = width, vheight = height, delay = 0.7)
  }
  file.copy(filename, filename)

}


#' @export
saveStatic <- function(viz, filename, format = NULL, width = 10, height = 7, ...) {

  if (is.null(format)) {
    format <- tools::file_ext(filename) %||% "png"
  }
  filename <- sub("([^.]+)\\.[[:alnum:]]+$", "\\1", filename)
  tmp <- paste(tempdir(), 'svg', sep ='.')
  svglite::svglite(tmp, width = width, height = height)
  print(viz)
  dev.off()
  bitmap <- rsvg::rsvg(tmp, height = 500)
  if (format == 'png') {
    png::writePNG(bitmap, paste0(filename, ".", format), dpi = 144) }
  if (format == 'jpeg') {
    jpeg::writeJPEG(bitmap, paste0(filename, ".", format))}
  if (format == 'svg') {
    rsvg::rsvg_svg(tmp, paste0(filename, ".", format))}
  if (format == 'pdf') {
    rsvg::rsvg_pdf(tmp, paste0(filename, ".", format))
  }

}
