#'@export
downloadTableUI <- function(id, text = "Download", formats = NULL, class = NULL, display = c("buttons", "dropdown"),
                            dropdownLabel = "Download", dropdownWidth = 150) {

  ns <- shiny::NS(id)
  loadingGif <- loadingGif()

  if (is.null(formats)) formats <- "csv"

  formats_id <- ns(paste0("DownloadTbl", formats))
  formats_lb <- paste0(text, " ", toupper(formats))
  names(formats_id) <- formats_lb
  choices_type <- rep("download", length(formats_id))

  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))


  if (display == "dropdown") {
    shinyinvoer::dropdownActionInput(ns("dropdown"), dropdownLabel, choices = formats_id, choicesType = choices_type, width = dropdownWidth)
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
downloadTableServer <- function(id, element = NULL, formats, file_prefix = "table", zip = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tbl_formats <- formats

    lapply(tbl_formats, function(z) {
      buttonId <- ns(paste0("DownloadTbl", z))

      output[[paste0("DownloadTbl", z)]] <- shiny::downloadHandler(
        filename = function() {
          session$sendCustomMessage("setButtonState", c("loading", buttonId))
          file_prefix <- eval_reactives(file_prefix)
          if (zip) {
            ext_file <- paste0("_",z, ".zip")
          } else {
            ext_file <- paste0(".", z)
          }
          paste0(file_prefix, "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)), ext_file)

        },
        content = function(file) {
          element <- eval_reactives(element)
          str(element)
          if (zip) {
            x <- saveZip(element, filename = file, format = z)
          } else {
            x <- saveTable(element, filename = file, format = z)
          }
          x
          session$sendCustomMessage("setButtonState", c("done", buttonId))
        })
    })
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

#'@export
saveZip <- function(list_tbl, filename, format = NULL, ...) {
print(as.data.frame(list_tbl[[1]]))
  if (is.null(format)) format <- tools::file_ext(filename) %||% "csv"

  tmp <- tempdir()

  filename <- gsub("([^.]+)\\.[[:alnum:]]+$", "\\1", filename)
  filename <- gsub(".csv|.xlsx|.json", "",filename)

  if (format == "csv") {
    purrr::map(seq_along(list_tbl), function(i) {
      readr::write_csv(list_tbl[[i]], paste0(names(list_tbl)[i],"_", i, ".csv"))
    })
  }
  if (format == "xlsx") {
    purrr::map(seq_along(list_tbl), function(i) {
      openxlsx::write.xlsx(list_tbl[[i]], paste0(names(list_tbl)[i],"_", i, ".xlsx"))
    })
  }
  if (format == "json") {
    purrr::map(seq_along(list_tbl), function(i) {
      jsonlite::write_json(list_tbl[[i]], paste0(names(list_tbl)[i],"_", i,".json"))
    })
  }

  purrr::map(seq_along(list_tbl), function(i){
    zip(paste0(filename, ".zip"), paste0(names(list_tbl)[i],"_", i, "." ,format))
  })

  unzip(paste0(filename, ".zip"))

}

# WARN: Contact David (david@datasketch.co) before deleting these two functions
#'@export
downloadTableServer2 <- function(id, element = NULL, formats, file_prefix = "table", zip = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tbl_formats <- formats

    lapply(tbl_formats, function(z) {
      buttonId <- ns(paste0("DownloadTbl", z))

      output[[paste0("DownloadTbl", z)]] <- shiny::downloadHandler(
        filename = function() {
          session$sendCustomMessage("setButtonState", c("loading", buttonId))
          file_prefix <- eval_reactives(file_prefix)
          if (zip) {
            ext_file <- paste0("_",z, ".zip")
          } else {
            ext_file <- paste0(".", z)
          }
          paste0(file_prefix, "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)), ext_file)

        },
        content = function(file) {
          element <- eval_reactives(element)
          str(element)
          if (zip) {
            x <- saveZip2(element, filename = file, format = z)
          } else {
            x <- saveTable(element, filename = file, format = z)
          }
          x
          session$sendCustomMessage("setButtonState", c("done", buttonId))
        })
    })
  })

}

#'@export
saveZip2 <- function(list_tbl, filename, format = NULL, ...) {

  if (is.null(format)) format <- tools::file_ext(filename) %||% "csv"

  tmp <- tempdir()
  filename <- gsub("([^.]+)\\.[[:alnum:]]+$", "\\1", filename)
  filename <- gsub(".csv|.xlsx|.json", "",filename)
  files <- c()

  if (format == "csv") {
    filenames <- purrr::map(seq_along(list_tbl), function(i) {
      current_file <- paste0(tmp, "/", names(list_tbl)[i],"_", i, ".csv")
      readr::write_csv(list_tbl[[i]], current_file)
      current_file
    })
    files <- append(files, unlist(filenames))
  }
  if (format == "xlsx") {
    filenames <- purrr::map(seq_along(list_tbl), function(i) {
      current_file <- paste0(tmp, "/", names(list_tbl)[i],"_", i, ".xlsx")
      openxlsx::write.xlsx(list_tbl[[i]], current_file)
      current_file
    })
    files <- append(files, unlist(filenames))
  }
  if (format == "json") {
    filenames <- purrr::map(seq_along(list_tbl), function(i) {
      current_file <- paste0(tmp, "/", names(list_tbl)[i],"_", i,".json")
      jsonlite::write_json(list_tbl[[i]], current_file)
      current_file
    })
    files <- append(files, unlist(filenames))
  }

  zip(paste0(filename, ".zip"), files = files, flags = "-r9Xj")
}