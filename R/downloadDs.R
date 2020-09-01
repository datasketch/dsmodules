#' @export
downloadDsUI <- function(id, text = "Download",
                         formats = NULL, class = NULL, display = c("buttons", "dropdown"),
                         dropdownLabel = "Download", dropdownWidth = 150, getLinkLabel = "Get link",
                         modalTitle = "Get link", modalBody = NULL,
                         modalButtonLabel = "Submit",
                         modalLinkLabel = "Link",
                         modalFormatChoices = c("HTML" = "html"),
                         modalPermalinkLabel = "Permalink",
                         modalIframeLabel = "Copy to embed", ...) {

  ns <- NS(id)
  dwn_mdl <- from_formats_to_module(formats)

  tab_styles <- ".recalculating {
                 opacity: 1;
                 }
                 #tab_id_here {
                 margin-bottom: 27px;
                 margin-top: 22px;
                 }
                 #tab_id_here div.shiny-options-group {
                 display: flex;
                 }
                 #tab_id_here div.radio label input + span {
                 border-radius: 0.35rem;
                 cursor: pointer;
                 margin: 6px 2px 6px 0;
                 padding: 10px;
                 }
                 #tab_id_here div.radio label input:checked + span {
                 background-color: #da1c95;
                 color: #ffffff;
                 font-size: 13px;
                 font-weight: 700;
                 letter-spacing: 0.7px;
                 }
                 #tab_id_here input[type='radio'] {
                 display: none;
                 }"
  tab_styles <- gsub("tab_id_here", ns("tab-formats"), tab_styles)
  modal_content <- div(singleton(tags$head(tags$style(HTML(tab_styles)))),
                       style = "display: flex; justify-content: center; padding: 2rem 4rem;",
                       div(style = "margin: -20px 0;",
                           formUI(ns("modal_form"), "", button_label = modalButtonLabel, input_list = modalBody)),
                       div(style = "background-color: #eee; margin: 0rem 3rem; width: 1px;"),
                       div(style = "width: 340px;",
                           div(class = "form-group",
                               tags$label(class = "control-label", modalLinkLabel),
                               uiOutput(ns("link"), class = "form-control", style = "min-height: 27px; overflow-x: auto;")),
                           radioButtons(ns("tab-formats"), "", modalFormatChoices),
                           div(class = "form-group",
                               tags$label(class = "control-label", modalPermalinkLabel),
                               uiOutput(ns("permalink"), class = "form-control", style = "min-height: 27px; overflow-x: auto;")),
                           div(class = "form-group",
                               tags$label(class = "control-label", modalIframeLabel),
                               uiOutput(ns("iframe"), class = "form-control", style = "min-height: 173px; overflow-x: auto;"))))
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
downloadDsServer <- function(id, element = NULL, formats, errorMessage = NULL, modalFunction = NULL, ...) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    urls <- formServer("modal_form", errorMessage = errorMessage, FUN = modalFunction, ...)

    output$link <- renderUI({as.list(urls())$share[[1]]$link})
    output$permalink <- renderUI({as.list(urls())$share[[input$`tab-formats`]]$permalink})
    output$iframe <- renderUI({as.list(urls())$share[[input$`tab-formats`]]$embed})

    element <- eval_reactives(element)
    dwn_mdl <- from_formats_to_module(formats)
    lb <- ""
    if (dwn_mdl == "downloadImage") {
      lb <- ifelse(grepl("ggplot|ggmagic", paste0(class(element), collapse = "")), "ggplot", "highcharter")
      names(lb) <- "lib"
    }
    do.call(paste0(dwn_mdl, "Server"), c(list(id = id, element = element, formats = formats), lb))
  })

}
