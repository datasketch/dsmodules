#' @export
downloadDsUI <- function(id, text = "Download",
                         formats = NULL,
                         class = NULL,
                         display = "dropdown",
                         dropdownLabel = "Download",
                         dropdownWidth = 150,
                         getLinkLabel = "Save / Publish",
                         max_inputs_first_column = NULL,
                         displayLinks = TRUE,
                         displayLinksBody = NULL,
                         modalFullscreen = TRUE,
                         modalTitle = "Save / Publish",
                         modalBody = NULL,
                         modalBodyInputs = c("name", "description", "sources", "license", "tags", "category", "access"),
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
                         accessLabel = "Visibility",
                         accessChoicesLabels = c("Public", "Private"),
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
                 .form_before_success {
                 display: flex;
                 justify-content: center;
                 padding: 2rem 2rem;
                 flex-direction: row;
                 width: 800px;
                 }
                 .form_after_success {
                 display: flex;
                 justify-content: space-between;
                 padding: 2rem 2rem;
                 flex-direction: row;
                 width: 1100px;
                 }
                 .main_display_before_success {
                 width: 700px;
                 }
                 .main_display_after_success {
                 width: 70%;
                 }
                 .additional_display_before_success {
                 display: none;
                 }
                 .additional_display_after_success {
                 width: 30%;
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
                                    categoryChoicesIDs = categoryChoicesIDs,
                                    accessLabel = accessLabel,
                                    accessChoicesLabels = accessChoicesLabels)
  }


  if(displayLinks){

    if(is.null(displayLinksBody)){

      displayLinksBody <- div(div(style = "border-left: 1px solid #eee; height: 300px; position: absolute; top: 25%;"),
                              div(style = "margin-left: 25px;",
                                  div(class = "form-group",
                                      tags$label(class = "control-label", modalLinkLabel),
                                      div(uiOutput(ns("link"),
                                                   class = "form-control",
                                                   style = "min-height: 27px; overflow-x: auto; width: 80% !important; float: left;"),
                                          shinyCopy2clipboard::CopyButton(
                                            "copybtn_link",
                                            label = "",
                                            icon = icon("copy"),
                                            text = "No Text Found",
                                            modal = TRUE
                                          ))),
                                  radioButtons(ns("tab-formats"), "", modalFormatChoices),
                                  div(class = "form-group",
                                      tags$label(class = "control-label", modalPermalinkLabel),
                                      div(
                                        uiOutput(ns("permalink"),
                                                 class = "form-control",
                                                 style = "min-height: 27px; overflow-x: auto; width: 80% !important; float: left;"),
                                        shinyCopy2clipboard::CopyButton(
                                          "copybtn_permalink",
                                          label = "",
                                          icon = icon("copy"),
                                          text = "No Text Found",
                                          modal = TRUE
                                        ))),
                                  div(class = "form-group",
                                      tags$label(class = "control-label", modalIframeLabel),
                                      div(uiOutput(ns("iframe"),
                                                   class = "form-control",
                                                   style = "min-height: 100px; overflow-x: auto; width: 80% !important; float: left;"),
                                          shinyCopy2clipboard::CopyButton(
                                            "copybtn_embed",
                                            label = "",
                                            icon = icon("copy"),
                                            text = "No Text Found",
                                            modal = TRUE
                                          )))))

    }

  }


  modal_content <- div(singleton(tags$head(tags$style(HTML(tab_styles)))),
                       # style = "display: flex; justify-content: center; padding: 2rem 4rem;",
                       div(formUI(ns("modal_form"), "", button_label = modalButtonLabel, input_list = modalBody, max_inputs_first_column = max_inputs_first_column,
                                  additional_display_body = displayLinksBody)))

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
downloadDsServer <- function(id, formats, errorMessage = NULL, displayLinks = FALSE, modalFunction = NULL, ...) {

  args <- list(...)
  element <- args$element
  opts_theme <- args$opts_theme
  page_title <- args$page_title
  if(is.null(element)) stop("Need an 'element' to save.")

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # use default modalFunction to save file with dspin_urls if no modalFunction is specified
    if(is.null(modalFunction)){
      modalFunction <- modalFunction_saveFile
    }

    urls <- formServer("modal_form", errorMessage = errorMessage, show_additional_display_on_success = displayLinks, FUN = modalFunction, ...)

    r <- reactiveValues(element_name = NULL,
                        links = NULL)

    # update name field when name was not entered
    observe({
      name_field <- "modal_form-name"
      if(is.null(input$`modal_form-name`)){
        name_field <- paste0("modal_form-",id,"-name")
      }

      input_name <- input[[name_field]]

      r$element_name <- input_name

      if(!is.null(input_name)){
        if(!nzchar(input_name) & !is.null(urls())){
          namePlaceholder <- sub('.*\\/', '', urls()$link)
          r$element_name <- namePlaceholder
          updateTextInput(session, name_field,
                          value = namePlaceholder)
        }
      }
    })


    observe({
      req(r$element_name)
      if(displayLinks){

        type <- args$type
        element_slug <- dspins::create_slug(r$element_name)

        formats <- NULL
        if(type == "fringe"){
          formats <- c("csv", "json")
        } else if(type == "dsviz"){
          viz_type <- dspins::dsviz_type(element)
          if(viz_type == "gg")
            formats <- c("png", "svg")
          if(viz_type == "htmlwidget")
            formats <- c("html", "png")
        }

        folder <- args$org_name
        if(is.null(folder)) folder <- args$user_name

        all_links <- dspins::create_ds_links(slug = element_slug, folder = folder, formats = formats, element_type = type)

        links_share_selected <- all_links$share[[input$`tab-formats`]]

        r$links <- list(link = links_share_selected$link,
                        permalink = links_share_selected$permalink,
                        embed = links_share_selected$embed)

      }
    })

    observe({
      req(r$links)
      if(displayLinks){
        shinyCopy2clipboard::CopyButtonUpdate(session,
                                              id = "copybtn_link",
                                              label = "",
                                              icon = icon("copy"),
                                              text = as.character(r$links$link))
        shinyCopy2clipboard::CopyButtonUpdate(session,
                                              id = "copybtn_permalink",
                                              label = "",
                                              icon = icon("copy"),
                                              text = as.character(r$links$permalink))
        shinyCopy2clipboard::CopyButtonUpdate(session,
                                              id = "copybtn_embed",
                                              label = "",
                                              icon = icon("copy"),
                                              text = as.character(r$links$embed))
      }
    })

    # populate link, permalink and iframe fields after saving
    output$link <- renderUI({"link"
      req(r$links)
      r$links$link
    })

    output$permalink <- renderUI({"permalink"
      req(r$links)
      r$links$permalink
    })

    output$iframe <- renderUI({"iframe"
      req(r$links)
      r$links$embed
    })

    element <- eval_reactives(element)
    dwn_mdl <- from_formats_to_module(formats)
    download_id <- "downloadDSid"

    params <- list(id = download_id, element = element, formats = formats)

    if (dwn_mdl == "downloadImage") {
      lib <- ifelse(grepl("ggplot|ggmagic", paste0(class(element), collapse = "")), "ggplot", "highcharter")
      names(lib) <- "lib"
      params$lib <- lib
      params$opts_theme <- opts_theme
      params$page_title <- page_title
    } else if(dwn_mdl == "downloadHtmlwidget"){
      params$opts_theme <- opts_theme
      params$page_title <- page_title
    }
    do.call(paste0(dwn_mdl, "Server"), params)
  })

}
