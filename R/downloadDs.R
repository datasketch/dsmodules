#' @export

downloadDsUI <- function(id, text = "Download",
                         formats = NULL, class = NULL, display = c("buttons", "dropdown"),
                         dropdownLabel = "Download", dropdownWidth = 150, getLinkLabel = "Get link",
                         modalTitle = "Get link", modalBody = NULL,
                         modalButtonLabel = "Submit",
                         modalLinkLabel = "URL",
                         modalIframeLabel = "Copy to embed", ...) {

  ns <- NS(id)
  dwn_mdl <- from_formats_to_module(formats)
  modal_content <- div(style = "display: flex; justify-content: center; padding: 2rem 4rem;",
                       div(style = "display: flex; flex-direction: column; justify-content: space-between; width: 450px;",
                           formUI(ns("modal_form"), "", button_label = modalButtonLabel, input_list = modalBody)),
                       div(style = "background-color: #ddd; margin: 0rem 6rem; width: 2px;"),
                       div(style = "width: 340px;",
                           textInput(ns("url"), modalLinkLabel),
                           textAreaInput(ns("iframe"), modalIframeLabel, rows = 9, cols = 4)))
  md <- modal(id = paste0("md-", ns("get_link")), title = modalTitle, modal_content)
  download_module <- do.call(paste0(dwn_mdl, "UI"), list(id = ns(id), text = text, formats = formats, class = class,
                                                         display = display, dropdownLabel = dropdownLabel, dropdownWidth = dropdownWidth))
  if (display == "dropdown") {
    link <- paste0("\\[{\"id\":\"",
                   ns("get_link"),
                   "\",\"image\":\"dropdownAction/images/share_link.svg\",\"label\":\"",
                   getLinkLabel,
                   "\",\"type\":\"modalShinypanels\"},")
    download_module$attribs$`data-options` <- HTML(gsub("\\[", link, as.character(download_module$attribs$`data-options`)))
  } else {
    link <- tagList(div(style = "text-align:center;",
                        `data-for-btn` = ns("get_link"),
                        actionButton(ns("get_link"), getLinkLabel, class = paste0(class, " modal-trigger"), style = "width: 200px; display: inline-block;",`data-modal` = paste0("md-", ns("get_link"))),
                        span(class = "btn-loading-container",
                             img(src = loadingGif(), class = "btn-loading-indicator", style = "display: none"),
                             HTML("<i class = 'btn-done-indicator fa fa-check' style='display: none'> </i>"))))
    download_module$children <- c(link, download_module$children)
  }
  tagList(md, download_module)

}

#' @export
downloadDsServer <- function(id, element = NULL, formats, modalFunction = NULL, ...) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    urls <- formServer("modal_form", FUN = modalFunction, ...)

    observe({
      urls <- urls()
      # if (!is.null(urls)) {
      updateTextInput(session = session, inputId = "url", value = urls)
      updateTextAreaInput(session = session, inputId = "iframe", value = paste0("<iframe src='", urls, "'></iframe>"))
      # }
    })
    dwn_mdl <- from_formats_to_module(formats)
    do.call(paste0(dwn_mdl, "Server"), list(id = id, element = element, formats = formats))
  })

}
