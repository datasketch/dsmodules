#' @export
downloadDistinctFormatsUI <-
  function (id, text = "Download", formats = NULL, class = NULL, display = "dropdown",
            dropdownLabel = "Download", dropdownWidth = 150) {
    ns <- shiny::NS(id)
    loadingGif <- loadingGif()

    if (is.null(formats)) return()

    formats_id <- ns(paste0("DownloadDistFormats", formats))
    formats_lb <- paste0(text, " ", toupper(formats))
    names(formats_id) <- formats_lb
    choices_type <- rep("download", length(formats_id))

    addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))

    if (display == "dropdown") {
      purrr::map(seq_along(dropdownLabel), function(dl) {
        dl <- dropdownLabel[[dl]]
        print(dl)
      shinyinvoer::dropdownActionInput(ns("dropdown"),  dl, choices = formats_id, choicesType = choices_type, width = dropdownWidth)
      })
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
downloaDistinctFormatsServer <-
  function (id, element = NULL, formats, lib, file_prefix = "downFile") {

    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      purrr::map(seq_along(formats), function (ln) {
        file_format <- formats[[ln]]
        element <- element[[ln]]
        lapply(file_format, function(z) {
          buttonId <- ns(paste0("DownloadDistFormats", z))
          output[[paste0("DownloadDistFormats", z)]] <- shiny::downloadHandler(
            filename = function() {
              z <- gsub("[0-9]", "", z)
              session$sendCustomMessage('setButtonState', c('loading', buttonId))
              file_prefix <- eval_reactives(file_prefix)
              paste0(file_prefix, "_", gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19)), ".", z)
            },
            content = function(file) {
              element <- eval_reactives(element)
              lib <- lib[[ln]]
              z <- gsub("[0-9]", "", z)
              print(z)
              if (lib == "highcharter") {
                saveInteractive(viz = element, filename = file, format = z)
              } else if (lib == "ggplot") {
                saveStatic(viz = element, filename = file, format = z)
              } else if (lib == "table") {
                saveTable(element, filename = file, format = z)
              } else {
                return()
              }
              session$sendCustomMessage('setButtonState', c('done', buttonId))
            })
        })
      })
    })
  }
