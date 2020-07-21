#' @export
getLinkUI <- function(id) {

  ns <- shiny::NS(id)

  div(style = "display: flex; padding: 1%;",
      div(style = "border-right: 2px solid #ddd; padding: 4%; width: 60%;",
          textInput(ns("name"), "Name"),
          textInput(ns("slug"), "Slug"),
          textInput(ns("description"), "Description"),
          selectInput(ns("license"), "License", choices = c("CC0", "CC-BY")),
          selectizeInput(ns("tags"), "Tags", choices = list("No tag" = "no-tag"), multiple = TRUE, options = list(plugins= list('remove_button', 'drag_drop'))),
          selectizeInput(ns("category"), "Category", choices = list("No category" = "no-category")),
          actionButton(ns("save"), "Save in dslibrary")),
      div(style = "padding: 4%; width: 40%;",
          textInput(ns("url"), "Link"),
          textAreaInput(ns("iframe"), "Copy to embed", rows = 6, cols = 4)))

}

# link o botón para compartir
# getLinkUI <- function(id, text = "Get link", class = NULL, width = 150) {
# }
#' @export
getLinkServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    url <- eventReactive(input$save, {
      Sys.sleep(3)
      "https://app.infraestructuravisible.org/shiny/infra-viz/"
    })

    observe({
      if (nzchar(url())) {
        updateTextInput(session = session, inputId = "url", value = url())
        updateTextAreaInput(session = session, inputId = "iframe", value = paste0("<iframe src='", url(), "'></iframe>"))
      } else {

      }
    })
  })

}
