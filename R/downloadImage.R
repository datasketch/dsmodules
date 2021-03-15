#' @export
downloadImageUI <- function(id, text = "Download", formats = NULL, class = NULL, display = "dropdown",
                            dropdownLabel = "Download", dropdownWidth = 150) {

  ns <- shiny::NS(id)
  loadingGif <- loadingGif()

  if (is.null(formats)) formats <- "png"

  formats_id <- ns(paste0("DownloadImg", formats))
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


#' @param type Image library
#' @export
downloadImageServer <- function(id, element = NULL, lib = NULL, formats,  file_prefix = "plot") {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    img_format <- formats

    lapply(img_format, function(z) {
      buttonId <- ns(paste0("DownloadImg", z))

      output[[paste0("DownloadImg", z)]] <- shiny::downloadHandler(
        filename = function() {
          session$sendCustomMessage('setButtonState', c('loading', buttonId))
          file_prefix <- eval_reactives(file_prefix)
          paste0(file_prefix, "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
        },
        content = function(file) {
          element <- eval_reactives(element)
          if (lib == "highcharter") {
            saveInteractive(viz = element, filename = file, format = z)
          } else if (lib == "ggplot") {
            saveStatic(viz = element, filename = file, format = z)
          } else {
            return()
          }
          session$sendCustomMessage('setButtonState', c('done', buttonId))
        })
    })
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
    webshot2::webshot(tmp, paste0(filename, ".", format), vwidth = width, vheight = height, delay = 0.7)
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
