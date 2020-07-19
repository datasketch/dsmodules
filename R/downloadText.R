#'@export
downloadTextUI <- function(id, text = "Download", formats = NULL, class = NULL, label = "Download", display = "dropdown", dropdownWidth = 150) {

  ns <- shiny::NS(id)
  loadingGif <- "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"

  txt_formats <- formats
  if (is.null(formats)) txt_formats <- "txt"

  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))

  if (display == "dropdown") {
    ch <- ns(paste0("DownloadTxt", txt_formats))
    names(ch) <- paste0(text, " ", toupper(txt_formats))
    dropdownActionInput("dropdown", label, choices = ch,  width = dropdownWidth)
  } else {
    div(shiny::tagList(shiny::singleton(shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js")))),
        lapply(txt_formats, function(z) {
          shiny::tagList(shiny::div(style = "text-align: center;",
                                    `data-for-btn` = ns(paste0("DownloadTxt", z)),
                                    shiny::downloadButton(ns(paste0("DownloadTxt", z)), paste0(text, " ", toupper(z)), class = class, style = "width: 200px; display: inline-block;"),
                                    shiny::span(class = "btn-loading-container",
                                                shiny::img(src = loadingGif, class = "btn-loading-indicator", style = "display: none;"),
                                                shiny::HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none;'> </i>"))))
        }))
  }

}


#'@export
downloadText <- function(input, output, session, text = NULL, formats, name = "text") {

  ns <- session$ns
  txt_formats <- formats

  lapply(txt_formats, function(z) {
    buttonId <- ns(paste0("DownloadTxt", z))

    output[[paste0("DownloadTxt", z)]] <- shiny::downloadHandler(
      filename = function() {
        session$sendCustomMessage("setButtonState", c("loading", buttonId))
        if (shiny::is.reactive(name))
          name <- name()
        paste0(name, "-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
      },
      content = function(file) {
        if (shiny::is.reactive(text))
          text <- text()
        t0 <- text
        if (z != "txt") {
          c0 <- lapply(text, function(w) {
            # s0 <- gsub("    ", " \n ", w)
            # gsub("\n", " \n\n ", s0)
            s0 <- strsplit(text, "\n")[[1]]
            paste(t0, collapse = " \n\n ")
          })
          t0 <- unlist(c0)
        }
        saveText(t0, filename = file, format = z)
        session$sendCustomMessage("setButtonState", c("done", buttonId))
      }
    )
  })

}


#'@export
saveText <- function(txt, filename, format = NULL, ...) {

  if (is.null(txt)) {
    txt <- " "
  }
  if (!nzchar(txt)) {
    txt <- " "
  }
  if (is.null(format)) {
    format <- tools::file_ext(filename) %||% "txt"
  }
  filename <- gsub("([^.]+)\\.[[:alnum:]]+$", "\\1", filename)
  tmpl <- system.file("template_download-text.Rmd", package = "dsmodules")
  if (format == "txt") {
    writeLines(txt, paste0(filename, ".txt"))
  } else {
    pander::Pandoc.brew(text = txt, output = filename, convert = format, options = "-s", open = FALSE)
  }

}
