#'@export
downloadTextUI <- function(id, text = "Download", formats = NULL, class = NULL, display = c("buttons", "dropdown"),
                           dropdownLabel = "Download", dropdownWidth = 150) {

  ns <- shiny::NS(id)
  loadingGif <- loadingGif()

  if (is.null(formats)) formats <- "txt"

  formats_id <- ns(paste0("DownloadTxt", formats))
  formats_lb <- paste0(text, " ", toupper(formats))
  names(formats_id) <- formats_lb
  choices_type <- rep("download", length(formats_id))

  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))

  if (display == "dropdown") {
    dropdownActionInput(ns("dropdown"), dropdownLabel, choices = formats_id, choicesType = choices_type, width = dropdownWidth)
  } else {
    div(shiny::tagList(shiny::singleton(shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js")))),
        lapply(seq_along(choices_type), function(z) {
          d <- ifelse(choices_type[z] %in% c("button"), "actionButton", "downloadButton")
          shiny::tagList(shiny::div(style = "text-align: center;",
                                    `data-for-btn` = formats_id[z],
                                    do.call(d, list(formats_id[z], formats_lb[z], class = class, style = "width: 200px; display: inline-block;")),
                                    shiny::span(class = "btn-loading-container",
                                                shiny::img(src = loadingGif, class = "btn-loading-indicator", style = "display: none;"),
                                                shiny::HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none;'> </i>"))))
        }))
  }

}


#'@export
downloadTextServer <- function(id, element = NULL, formats, file_prefix = "text") {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    txt_formats <- formats

    lapply(txt_formats, function(z) {
      buttonId <- ns(paste0("DownloadTxt", z))

      output[[paste0("DownloadTxt", z)]] <- shiny::downloadHandler(
        filename = function() {
          session$sendCustomMessage("setButtonState", c("loading", buttonId))
          file_prefix <- eval_reactives(file_prefix)
          paste0(file_prefix, "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
        },
        content = function(file) {
          element <- eval_reactives(element)
          t0 <- element
          if (z != "txt") {
            c0 <- lapply(element, function(w) {
              # s0 <- gsub("    ", " \n ", w)
              # gsub("\n", " \n\n ", s0)
              s0 <- strsplit(element, "\n")[[1]]
              paste(t0, collapse = " \n\n ")
            })
            t0 <- unlist(c0)
          }
          saveText(t0, filename = file, format = z)
          session$sendCustomMessage("setButtonState", c("done", buttonId))
        })
    })
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
