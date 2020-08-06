#' @export
getLinkUI <- function(id, infoInputs = list(), nameLabel = "Name", saveButtonLabel = "Save in dslibrary", linkLabel = "URL", iframeLabel = "Copy to embed") {

  ns <- shiny::NS(id)
  loadingGif <- "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"
  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))
  bt <- div(shiny::singleton(shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js"))),
            style = "text-align: center; display: flex; align-items: baseline;",
            actionButton(ns("save"), saveButtonLabel, style = "margin-left: 0; margin-right: 0;"),
            span(class = "btn-loading-container",
                 img(style = "display: none; margin-left: 18px;",
                     class = "btn-loading-indicator",
                     src = "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"),
                 HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>")))

    div(style = "display: flex; justify-content: center; padding: 2rem 4rem;",
      div(style = "display: flex; flex-direction: column; justify-content: space-between; width: 450px;",
          div(style = "display: flex; flex-direction: column; justify-content: flex-start;",
          textInput(ns("name"), nameLabel),
          tagList(infoInputs)),
          bt),
      div(style = "background-color: #ddd; margin: 0rem 6rem; width: 2px;"),
      div(style = "width: 340px;",
          textInput(ns("url"), linkLabel),
          textAreaInput(ns("iframe"), iframeLabel, rows = 9, cols = 4)))

}

#' @export
getLinkServer <- function(id, FUN, ...) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    vl <- reactiveValues(url = NULL)
    observeEvent(input$save, {
      session$sendCustomMessage("setButtonState", c("none", ns("save")))
      session$sendCustomMessage("setButtonState", c("loading", ns("save")))
      args <- list(...)
      vl$url <- do.call(FUN, args)
      session$sendCustomMessage("setButtonState", c("done", ns("save")))
    })

    observe({
      if (!is.null(vl$url)) {
        updateTextInput(session = session, inputId = "url", value = vl$url)
        updateTextAreaInput(session = session, inputId = "iframe", value = paste0("<iframe src='", vl$url, "'></iframe>"))
      } else {

      }
    })
  })

}
