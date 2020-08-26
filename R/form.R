#' @export
formUI <- function(id, label, button_label = "Submit", input_list = NULL) {

  ns <- shiny::NS(id)
  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))
  bt <- div(shiny::singleton(
    shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js"))),
    style = "text-align: center; display: flex; align-items: baseline;",
    actionButton(ns("form_button"), button_label, style = "margin: 10px 0;"),
    span(class = "btn-loading-container",
         img(style = "display: none; margin-left: 18px;",
             class = "btn-loading-indicator",
             src = loadingGif()),
         HTML("<i class = 'btn-done-indicator fa fa-check' style = 'display: none; margin-left: 18px;'> </i>"),
         HTML("<i class = 'btn-error-indicator fa fa-exclamation' style = 'display: none; margin-left: 18px;'> </i>"),
         uiOutput(ns("error_message"), inline = TRUE)
    )
  )

  input_ns <- lapply(input_list, updateInputNS, ns)

  div(class = "formUI",
    tags$label(label, class = "control-label-formUI",
               style = "font-weight:500; color: #435b69; margin-bottom: 10px;"),
    div(style = "display: flex; justify-content: center;  margin: 20px 0;",
        div(style = "display: flex; flex-direction: column; justify-content: space-between; width: 450px;",
            div(style = "display: flex; flex-direction: column; justify-content: flex-start;",
                tagList(input_ns)),
            bt
        )
    )
  )

}

#' @export
formServer <- function(id, errorMessage = NULL, FUN, ...) {
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
      fun_result <- tryCatch(do.call(FUN, args), error = function(e) e)
      if ("error" %in% class(fun_result)) {
        session$sendCustomMessage("setButtonState", c("error", ns("form_button")))
        if (!is.null(errorMessage)) {
          output$error_message <- renderUI({errorMessage})
        } else {
          output$error_message <- renderUI({as.character(fun_result)})
        }
      } else {
        session$sendCustomMessage("setButtonState", c("done", ns("form_button")))
        fun_result
      }
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


