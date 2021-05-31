#' @export
downloadDsUI <- function(id, text = "Download",
                         formats = NULL,
                         class = NULL,
                         display = "dropdown",
                         dropdownLabel = "Download",
                         dropdownWidth = 150,
                         getLinkLabel = "Save / Publish",
                         modalFullscreen = TRUE,
                         modalTitle = "Save / Publish",
                         modalBody = NULL,
                         modalBodyInputs = c("name", "description", "sources", "license", "tags", "category"),
                         modalButtonLabel = "Submit",
                         modalLinkLabel = "Link",
                         modalFormatChoices = c("HTML" = "html"),
                         modalPermalinkLabel = "Permalink",
                         modalIframeLabel = "Copy to embed",
                         nameLabel = "Name",
                         descriptionLabel = "Description",
                         sourceLabel = "Source",
                         sourceTitleLabel = "Title",
                         sourcePathLabel = "URL",
                         licenseLabel = "License",
                         tagsLabel = "Tags",
                         tagsPlaceholderLabel = "Type tag(s) and press enter after each one",
                         categoryLabel = "Category",
                         categoryChoicesLabels = c("No category"),
                         categoryChoicesIDs = c("no-category"),
                         ...) {

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
                 }
                 "
  tab_styles <- gsub("tab_id_here", ns("tab-formats"), tab_styles)
  # provisonal
  if (modalFullscreen) {
    tab_styles <- paste0(tab_styles, ".panel-header {
                         position: inherit;
                         z-index: inherit;
                         }")
  }

  if(is.null(modalBody)){

    modalBody <- modalBody_saveFile(id = id,
                                    include_inputs = modalBodyInputs,
                                    nameLabel = nameLabel,
                                    descriptionLabel = descriptionLabel,
                                    sourceLabel = sourceLabel,
                                    sourceTitleLabel = sourceTitleLabel,
                                    sourcePathLabel = sourcePathLabel,
                                    licenseLabel = licenseLabel,
                                    tagsLabel = tagsLabel,
                                    tagsPlaceholderLabel = tagsPlaceholderLabel,
                                    categoryLabel = categoryLabel,
                                    categoryChoicesLabels = categoryChoicesLabels,
                                    categoryChoicesIDs = categoryChoicesIDs)
  }

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

  md <- shinypanels::modal(id = paste0("md-", ns("get_link")), title = modalTitle, modal_content) # provisional, fullscreen = modalFullscreen)

  download_id <- "downloadDSid"
  download_module <- do.call(paste0(dwn_mdl, "UI"), list(id = ns(download_id), text = text, formats = formats, class = class,
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
                        actionButton(ns("get_link"), getLinkLabel, class = paste0(class, " modal-trigger"), style = "width: 200px; display: inline-block;", `data-modal` = paste0("md-", ns("get_link"))),
                        span(class = "btn-loading-container",
                             img(src = loadingGif(), class = "btn-loading-indicator", style = "display: none"),
                             HTML("<i class = 'btn-done-indicator fa fa-check' style='display: none'> </i>"))))
    download_module$children <- c(link, download_module$children)
  }
  tagList(md, download_module)
  # tagList(singleton(md), download_module)

}

#' @export
downloadDsServer <- function(id, formats, errorMessage = NULL, modalFunction = NULL, ...) {

  args <- list(...)
  element <- args$element
  opts_theme <- args$opts_theme
  if(is.null(element)) stop("Need an 'element' to save.")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # use default modalFunction to save file with dspin_urls if no modalFunction is specified
    if(is.null(modalFunction)){
      modalFunction <- modalFunction_saveFile
    }

    urls <- formServer("modal_form", errorMessage = errorMessage, FUN = modalFunction, ...)

    # update name field when name was not entered
    observe({
      name_field <- "modal_form-name"
      if(is.null(input$`modal_form-name`)){
        name_field <- paste0("modal_form-",id,"-name")
      }

      input_name <- input[[name_field]]

      if(!is.null(input_name)){
        if(!nzchar(input_name) & !is.null(urls())){
          namePlaceholder <- sub('.*\\/', '', urls()$link)
          updateTextInput(session, name_field,
                          value = namePlaceholder)
        }
      }
    })

    # populate link, permalink and iframe fields after saving
      output$link <- renderUI({"link"
        if(!is.null(urls())) urls()$link})

      output$permalink <- renderUI({"permalink"
        if(!is.null(urls())) urls()$permalink})

      output$iframe <- renderUI({"iframe"
        if(!is.null(urls())) urls()$iframe_embed})

    element <- eval_reactives(element)
    dwn_mdl <- from_formats_to_module(formats)
    download_id <- "downloadDSid"

    if (dwn_mdl == "downloadImage") {
      lib <- ifelse(grepl("ggplot|ggmagic", paste0(class(element), collapse = "")), "ggplot", "highcharter")
      names(lib) <- "lib"
      do.call(paste0(dwn_mdl, "Server"), list(id = download_id, element = element, formats = formats, lib = lib, opts_theme = opts_theme))
    } else if(dwn_mdl == "downloadHtmlwidget"){
      do.call(paste0(dwn_mdl, "Server"), list(id = download_id, element = element, formats = formats, opts_theme = opts_theme))
    } else {
      do.call(paste0(dwn_mdl, "Server"), list(id = download_id, element = element, formats = formats))
    }
  })

}
