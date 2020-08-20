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
  # w0 <- which(formats %in% "link")
  # modal_content <- ""
  # if (sum(w0) > 0) {
  #   formats_lb[w0] <- getLinkLabel
  #   choices_type[w0] <- "modalShinypanels"
  #   if (is.null(modalBody)) {
  #     modalBody <- list(textInput(ns("slug"), "Slug"),
  #                       textInput(ns("description"), "Description"),
  #                       selectInput(ns("license"), "License", choices = c("CC0", "CC-BY")),
  #                       selectizeInput(ns("tags"), "Tags", choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
  #                       selectizeInput(ns("category"), "Category", choices = list("No category" = "no-category")))
  #   }
  #   modal_content <- do.call(getLinkUI, list(id = ns("link"), infoInputs = modalBody, ...))
  #   if (display == "dropdown") {
  #     f0 <- c(formats_id[w0], "separator", formats_id[c(1:length(formats_id))[-w0]])
  #     names(f0) <- c(formats_lb[w0], "separator", formats_lb[c(1:length(formats_lb))[-w0]])
  #     choices_type <- c(choices_type[w0], NA, choices_type[c(1:length(choices_type))[-w0]])
  #     formats_id <- f0
  #   }
  # }

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

# downloadTextUI <- function(id, text = "Download", formats = NULL, class = NULL, display = c("buttons", "dropdown"),
#                            dropdownLabel = "Download", dropdownWidth = 150, getLinkLabel = "Get link",
#                            modalTitle = "Get link", modalBody = NULL, ...) {
#
#   ns <- shiny::NS(id)
#   loadingGif <- "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"
#
#   if (is.null(formats)) formats <- "txt"
#
#   formats_id <- ns(paste0("DownloadTxt", formats))
#   formats_lb <- paste0(text, " ", toupper(formats))
#   names(formats_id) <- formats_lb
#   choices_type <- rep("download", length(formats_id))
#   w0 <- which(formats %in% "link")
#   modal_content <- ""
#   if (sum(w0) > 0) {
#     formats_lb[w0] <- getLinkLabel
#     choices_type[w0] <- "modalShinypanels"
#     if (is.null(modalBody)) {
#       modalBody <- list(textInput(ns("slug"), "Slug"),
#                         textInput(ns("description"), "Description"),
#                         selectInput(ns("license"), "License", choices = c("CC0", "CC-BY")),
#                         selectizeInput(ns("tags"), "Tags", choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
#                         selectizeInput(ns("category"), "Category", choices = list("No category" = "no-category")))
#     }
#     modal_content <- do.call(getLinkUI, list(id = ns("link"), infoInputs = modalBody, ...))
#     if (display == "dropdown") {
#       f0 <- c(formats_id[w0], "separator", formats_id[c(1:length(formats_id))[-w0]])
#       names(f0) <- c(formats_lb[w0], "separator", formats_lb[c(1:length(formats_lb))[-w0]])
#       choices_type <- c(choices_type[w0], NA, choices_type[c(1:length(choices_type))[-w0]])
#       formats_id <- f0
#     }
#   }
#
#   addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))
#
#   if (display == "dropdown") {
#     div(modal(id = paste0("md-", ns("DownloadTxtlink")), title = modalTitle, modal_content),
#         dropdownActionInput(ns("dropdown"), dropdownLabel, choices = formats_id, choicesType = choices_type, width = dropdownWidth))
#   } else {
#     div(shiny::tagList(shiny::singleton(shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js")))),
#         modal(id = "md-button_text-DownloadTxtlink", title = modalTitle, modal_content),
#         lapply(seq_along(choices_type), function(z) {
#           d_modal <- ""
#           if (choices_type[z] == "modalShinypanels") {
#             class <- paste0(class, " modal-trigger")
#             d_modal <- "md-button_text-DownloadTxtlink"
#           }
#           d <- ifelse(choices_type[z] %in% c("button", "modalShinypanels"), "actionButton", "downloadButton")
#           shiny::tagList(shiny::div(style = "text-align: center;",
#                                     `data-for-btn` = formats_id[z],
#                                     do.call(d, list(formats_id[z], formats_lb[z], class = class, style = "width: 200px; display: inline-block;", `data-modal` = d_modal)),
#                                     shiny::span(class = "btn-loading-container",
#                                                 shiny::img(src = loadingGif, class = "btn-loading-indicator", style = "display: none;"),
#                                                 shiny::HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none;'> </i>"))))
#         }))
#   }
#
# }


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

# downloadText <- function(input, output, session, text = NULL, formats, name = "text", modalFunction = NULL, ...) {
#
#   ns <- session$ns
#   txt_formats <- formats
#
#   lapply(txt_formats, function(z) {
#     if (z == "link") {
#       if (is.function(modalFunction)) {
#         getLinkServer("link", modalFunction, ...)
#       }
#     } else {
#       buttonId <- ns(paste0("DownloadTxt", z))
#
#       output[[paste0("DownloadTxt", z)]] <- shiny::downloadHandler(
#         filename = function() {
#           session$sendCustomMessage("setButtonState", c("loading", buttonId))
#           if (shiny::is.reactive(name))
#             name <- name()
#           paste0(name, "-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
#         },
#         content = function(file) {
#           if (shiny::is.reactive(text))
#             text <- text()
#           t0 <- text
#           if (z != "txt") {
#             c0 <- lapply(text, function(w) {
#               # s0 <- gsub("    ", " \n ", w)
#               # gsub("\n", " \n\n ", s0)
#               s0 <- strsplit(text, "\n")[[1]]
#               paste(t0, collapse = " \n\n ")
#             })
#             t0 <- unlist(c0)
#           }
#           saveText(t0, filename = file, format = z)
#           session$sendCustomMessage("setButtonState", c("done", buttonId))
#         })
#     }
#   })
#
# }


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
