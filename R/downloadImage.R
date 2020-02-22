#' @export
downloadImagesUI <- function(id, text = "Download", formats = NULL, class = NULL) {
  ns <- NS(id)
  loadingGif <- "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"

  img_formats <- formats
  if (is.null(formats)) img_formats <- "png"

  addResourcePath(
    prefix = 'downloadInfo',
    directoryPath = system.file("aux/", package = "dsmodules")
  )

  div(
    shiny::tagList(
      shiny::singleton(
        shiny::tags$body(
          shiny::tags$script( src =  "downloadInfo/downloadGen.js")
        )
      )
    ),
    lapply(img_formats, function(z){
      tagList(
        div(
          `data-for-btn` = ns(paste0("DownloadImg", z)),
          downloadButton(ns(paste0("DownloadImg", z)), paste0(text, " ",toupper(z)), class = class),
          span(
            class = "btn-loading-container",
            #shinyjs::hidden(
            img(src = loadingGif, class = "btn-loading-indicator", style="display: none"),
            HTML("<i class = 'btn-done-indicator fa fa-check' style='display: none'> </i>")
            #)
          )
        )
      )
    })
  )
}



#' @param type Image library
#' @export
downloadImages <- function(input, output, session, graph = NULL, lib = NULL, formats,  name = "plot") {

  ns <- session$ns
  img_format <- formats

  lapply(img_format, function(z){

    buttonId <- ns(paste0("DownloadImg", z))
    loadingEl <- sprintf(paste0("[data-for-btn=%s] .", ns("btn-loading-indicator")), buttonId)
    doneEl <- sprintf(paste0("[data-for-btn=%s] .", ns("btn-done-indicator")), buttonId)
    errEl <- sprintf(paste0("[data-for-btn=%s] .",ns("btn-err")), buttonId)

    output[[paste0("DownloadImg", z)]] <- downloadHandler(
      filename = function() {
        session$sendCustomMessage('setButtonState', c('loading', buttonId))
        shinyjs::disable(buttonId)
        shinyjs::show(selector = loadingEl)
        shinyjs::hide(selector = doneEl)

        if(is.reactive(name))
          name <- name()
        paste0(name, "-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
        # paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        if (lib == "highcharter") {
          saveInteractive(viz = graph, filename = file, formats = z)
        } else if (lib == "ggplot") {
          saveStatic(viz = graph, filename = file, formats = z)
        } else {
          return()
        }
        session$sendCustomMessage('setButtonState', c('done', buttonId))
        shinyjs::enable(buttonId)
        shinyjs::hide(selector = loadingEl)
        shinyjs::show(selector = doneEl)
      }
    )
  })
}

#' @export
saveInteractive <- function(viz, filename, format = NULL, width = 660, height = 500, ...) {
  if (is.null(format)) {
    format <- tools::file_ext(filename) %||% "png"
  }
  filename <- file_path_sans_ext(filename)
  tmp <- paste(tempdir(), 'html', sep ='.')
  htmltools::save_html(viz, tmp)
  tmpSave <- filename
  if (format == 'html') {
    htmlwidgets::saveWidget(viz, paste0(filename, ".", format))
  } else {
    webshot::webshot(tmp, paste0(filename, ".", format), vwidth = width, vheight = height)
  }
  file.copy(filename, filename)
}

#' @export
saveStatic <- function(viz, filename, format = NULL, width = 10, height = 7, ...) {
  if (is.null(format)) {
    format <- tools::file_ext(filename) %||% "png"
  }
  filename <- file_path_sans_ext(filename)
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
