#' @export
formUI <- function(id, label, button_label = "Submit", input_list = NULL) {

  ns <- shiny::NS(id)
  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))
  bt <- div(shiny::singleton(
    shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js"))),
    style = "text-align: center; display: flex; align-items: baseline;",
    actionButton(ns("form_button"), button_label, style = "margin-left: 0; margin-right: 0;"),
    span(class = "btn-loading-container",
         img(style = "display: none; margin-left: 18px;",
             class = "btn-loading-indicator",
             src = loadingGif()),
         HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>")
    )
  )

  input_ns <- lapply(input_list, updateInputNS, ns)

  tagList(
    shiny:::shinyInputLabel("inputId", label = label),
    div(style = "display: flex; justify-content: center;  margin-bottom: 20px",
        div(style = "display: flex; flex-direction: column; justify-content: space-between; width: 450px;",
            div(style = "display: flex; flex-direction: column; justify-content: flex-start;",
                tagList(input_ns)),
            bt
        )
    )
  )

}

#' @export
formServer <- function(id, FUN, ...) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    form_input_list <- shiny::reactive({
      form_input_names <- ns(names(input))
      l <- reactiveValuesToList(input)
      names(l) <- form_input_names
      l[[ns("form_button")]] <- NULL
      l
    })

    save_result <- eventReactive(input$form_button, {
      session$sendCustomMessage("setButtonState", c("none", ns("form_button")))
      session$sendCustomMessage("setButtonState", c("loading", ns("form_button")))
      more_args <- list(...)
      form_input_list <- form_input_list()
      args <- c(form_input_list, more_args)
      fun_result <- do.call(FUN, args)
      session$sendCustomMessage("setButtonState", c("done", ns("form_button")))
      fun_result
    })

    save_result

  })
}

# save_result <- observeEvent(input$save, {
#   session$sendCustomMessage("setButtonState", c("none", ns("save")))
#   session$sendCustomMessage("setButtonState", c("loading", ns("save")))
#   args <- list(...)
#   fun_result <- do.call(FUN, args)
#   session$sendCustomMessage("setButtonState", c("done", ns("save")))
#   fun_result
# })
#
# save_result


updateInputNS <- function(x, ns){
  # textInput
  if(x$children[[2]]$name == "input"){
    x$children[[2]]$attribs$id <- ns(x$children[[2]]$attribs$id )
    return(x)
  }
  # selectInput
  if(x$children[[2]]$children[[1]]$name == "select"){
    x$children[[2]]$children[[1]]$attribs$id <- ns(x$children[[2]]$children[[1]]$attribs$id)
    x$children[[2]]$children[[2]]$attribs$`data-for` <- ns(x$children[[2]]$children[[2]]$attribs$`data-for`)
    return(x)
  }
  stop("Input unknown to form")
}


