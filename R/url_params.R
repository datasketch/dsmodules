#' @export
url_params_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("url_params_inputs"))
  )
}

#' @export
url_params_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    params <- shiny::reactive({
      get_url_params(session)
    })

    output$url_params_inputs <- renderUI({
      params_list <- purrr::map2(names(params()), params(), ~ list(id = .x, value = .y))
      shiny::tagList(
        div(style = "display:none",
            lapply(params_list, function(par){
              shiny::textInput(ns(par$id), par$id, value = par$value)
            })
        )
      )
    })

    params

  })
}


#' @export
url_params_data <- function(url_params){
  if(is.null(url_params)) return()
  if(is.reactive(url_params))
    url_params <- url_params()
  d <- url_params$data
  data <- read_data_from_url(d)
  data
}

get_url_params <- function(session){
  parseQueryString(session$clientData$url_search)
}



is_url <- function(x){
  grepl("^http", x)
}

read_data_from_url <- function(x){
  if(is.null(x)) return()
  if(is_url(x)){
    ext <- file_ext(x)
    data <- read_from_ext(x, ext)
  } else {
    data <- jsonlite::fromJSON(x)
  }
  data
}

read_from_ext <- function(d, ext){
  if(ext == ".csv"){
    data <- readr::read_csv(d)
  }
  if(ext == ".json"){
    data <-  jsonlite::fromJSON(x)
  }
  return(data)
}


file_ext <- function (x){
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
