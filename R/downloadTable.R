#'@export
downloadTableUI <- function(id, text = "Download", formats = NULL, class = NULL, display = c("buttons", "dropdown"), dropdownLabel = "Download",
                            dropdownWidth = 150, getLinkLabel = "Get link",
                            modalTitle = "Get link", modalBody = NULL) {

  ns <- shiny::NS(id)
  loadingGif <- "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"

  if (is.null(formats)) formats <- "csv"

  formats_id <- ns(paste0("DownloadTbl", formats))
  formats_lb <- paste0(text, " ", toupper(formats))
  names(formats_id) <- formats_lb
  choices_type <- rep("download", length(formats_id))
  w0 <- which(formats %in% "link")
  modal_body <- ""
  if (sum(w0) > 0) {
    formats_lb[w0] <- getLinkLabel
    choices_type[w0] <- "modalShinypanels"
    modal_body <- modalBody
    if (is.null(modal_body)) {
      modal_body <- getLinkUI(ns("link"))
    }
    if (display == "dropdown") {
      f0 <- c(formats_id[w0], "separator", formats_id[c(1:length(formats_id))[-w0]])
      names(f0) <- c(formats_lb[w0], "separator", formats_lb[c(1:length(formats_lb))[-w0]])
      choices_type <- c(choices_type[w0], NA, choices_type[c(1:length(choices_type))[-w0]])
      formats_id <- f0
    }
  }

  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))

  if (display == "dropdown") {
    div(modal(id = paste0("md-", ns("DownloadTbllink")), title = modalTitle, modal_body),
        dropdownActionInput(ns("dropdown"), dropdownLabel, choices = formats_id, choicesType = choices_type, width = dropdownWidth))
  } else {
    div(shiny::tagList(shiny::singleton(shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js")))),
        modal(id = "md-button_table-DownloadTbllink", title = modalTitle, modal_body),
        lapply(seq_along(choices_type), function(z) {
          d_modal <- ""
          if (choices_type[z] == "modalShinypanels") {
            class <- paste0(class, " modal-trigger")
            d_modal <- "md-button_table-DownloadTbllink"
          }
          d <- ifelse(choices_type[z] %in% c("button", "modalShinypanels"), "actionButton", "downloadButton")
          shiny::tagList(shiny::div(style = "text-align: center;",
                                    `data-for-btn` = formats_id[z],
                                    do.call(d, list(formats_id[z], formats_lb[z], class = class, style = "width: 200px; display: inline-block;", `data-modal` = d_modal)),
                                    shiny::span(class = "btn-loading-container",
                                                shiny::img(src = loadingGif, class = "btn-loading-indicator", style = "display: none;"),
                                                shiny::HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none;'> </i>"))))
        }))
  }

}


#'@export
downloadTable <- function(input, output, session, table = NULL, formats, name = "table", modalFunction = NULL, modalFunctionArgs = list()) {

  ns <- session$ns
  tbl_formats <- formats

  lapply(tbl_formats, function(z) {
    if (z == "link") {
      if (is.function(modalFunction)) {
        getLinkServer("link", modalFunction, modalFunctionArgs)
      }
    } else {
      buttonId <- ns(paste0("DownloadTbl", z))

      output[[paste0("DownloadTbl", z)]] <- shiny::downloadHandler(
        filename = function() {
          session$sendCustomMessage("setButtonState", c("loading", buttonId))
          if (shiny::is.reactive(name))
            name <- name()
          paste0(name, "-", gsub(" ", "_", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
        },
        content = function(file) {
          if (shiny::is.reactive(table))
            table <- table()
          saveTable(table, filename = file, format = z)
          session$sendCustomMessage("setButtonState", c("done", buttonId))
        }
      )
    }
  })

}


#'@export
saveTable <- function(tbl, filename, format = NULL, ...) {

  if (is.null(format)) {
    format <- tools::file_ext(filename) %||% "csv"
  }
  tmp <- paste(tempdir(), "csv", sep ='.')
  c0 <- c()
  lapply(1:nrow(tbl), function (i) {
    c0[i] <<- all(is.na(tbl[i, ]))
  })
  tbl <- tbl[!c0, ]
  write.csv(tbl, tmp)
  tmpSave <- filename
  filename <- gsub("([^.]+)\\.[[:alnum:]]+$", "\\1", filename)
  if (format == "csv") {
    write.csv(tbl, paste0(filename, ".csv"))
  }
  if (format == "xlsx") {
    openxlsx::write.xlsx(tbl, paste0(filename, ".xlsx"))
  }
  if (format == "json") {
    jsonlite::write_json(tbl, paste0(filename, ".json"))
  }

}
