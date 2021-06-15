#' @export
formUI <- function(id, label, button_label = "Submit", input_list = NULL, max_inputs_first_column = NULL, additional_display_body = "") {

  ns <- shiny::NS(id)
  addResourcePath(prefix = "downloadInfo", directoryPath = system.file("js", package = "dsmodules"))
  bt <- div(shiny::singleton(
    shiny::tags$body(shiny::tags$script(src = "downloadInfo/downloadGen.js"))),
    style = "text-align: center; display: flex; align-items: baseline; margin-top: 20px;",
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

  inputs <- div(class = "flex-container",
                style = "display: flex; flex-direction: column; justify-content: flex-start;",
                tagList(input_ns))

  if(!is.null(max_inputs_first_column)){
    inputs_left <- input_ns[1:max_inputs_first_column]
    inputs_right <- input_ns[max_inputs_first_column + 1 : length(input_ns)]

    inputs <- div(class = "flex-container",
                  style = "display: flex; flex-direction: row; justify-content: space-between;",
                  div(class = "flex-left",
                      style = "width: 47%;",
                      tagList(inputs_left)),
                  div(class = "flex-right",
                      style = "width: 47%;",
                      tagList(inputs_right)))
  }

  div(id = "form_complete",
      class = "form_before_success",
    div(id = "main_display",
        class = "main_display_before_success",
        div(class = "formUI",
            tags$label(label, class = "control-label-formUI",
                       style = "font-weight:500; color: #435b69; margin-bottom: 10px;"),
            div(id = "form_inputs",
                style = "display: flex; justify-content: center;  margin: 0px 25px;",
                div(style = paste0("display: flex; flex-direction: column; justify-content: space-between; width: 100%;"),
                    inputs,
                    bt
                )
            ))),
    div(id="additional_display",
        class="additional_display_before_success",
        # style="display: none;",
        additional_display_body
    ))

}

#' @export
formServer <- function(id, errorMessage = NULL, show_additional_display_on_success = FALSE, FUN, ...) {
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
      output$error_message <- renderUI({""})
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
        if(show_additional_display_on_success){
          shinyjs::runjs(code = '$("#form_complete").removeClass("form_before_success");')
          shinyjs::runjs(code = '$("#form_complete").addClass("form_after_success");')
          shinyjs::runjs(code = '$("#main_display").removeClass("main_display_before_success");')
          shinyjs::runjs(code = '$("#main_display").addClass("main_display_after_success");')
          shinyjs::runjs(code = '$("#additional_display").removeClass("additional_display_before_success");')
          shinyjs::runjs(code = '$("#additional_display").addClass("additional_display_after_success");')
        }
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
  if(identical(x$attribs$role, "radiogroup")){
    x$attribs$id <- ns(x$attribs$id)
    x$children[[2]]$children[[1]][[1]]$children[[1]]$attribs$name <- ns(x$children[[2]]$children[[1]][[1]]$children[[1]]$attribs$name)
    x$children[[2]]$children[[1]][[2]]$children[[1]]$attribs$name <- ns(x$children[[2]]$children[[1]][[2]]$children[[1]]$attribs$name)
    return(x)
  }

  if("shiny.tag.list" %in% class(x)){
    # chipsInput
    if(x[[2]]$attribs$class == "shinyinvoer-chips-input"){
      x[[2]]$attribs$id <- ns(x[[2]]$attribs$id)
      return(x)
    }
  }
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


