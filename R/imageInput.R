#' @export
imageInputUI <- function (id,
                          choices = c("fileUpload", "sampleData", "url", "dsLibrary"),
                          choicesInline = FALSE,
                          selected = "fileUpload") {

  ns <- shiny::NS(id)
  shiny::tagList(shiny::div(id = ns("imageInput"),
              class = "tableInput",
              shiny::radioButtons(ns("imageInput"), "", choices = choices, selected = selected, inline = choicesInline),
              shiny::uiOutput(ns("imageInputControls"))),
              shiny::div(class = "box-tableInputInfo", #style = info_style,
                         shiny::uiOutput(ns("imageInputInfo"))))
}


#' @export
imageInput <- function (input, output, session,
                        infoList = NULL,
                        uploadLabel = "Choose image", uploadButtonLabel = "Browse...", uploadPlaceholder = "No file selected",
                        sampleLabel = "Select sample image", sampleFiles = NULL, sampleSelected = NULL,
                        urlLabel = "Image URL", urlValue = "", urlPlaceholder = NULL) {


  output$imageInputControls <- shiny::renderUI({
    ns <- session$ns

    if (shiny::is.reactive(sampleFiles))
      sampleFiles <- sampleFiles()

    if (!is.null(input$imageInput) && input$imageInput == "sampleData") {
      if (!all(map_lgl(sampleFiles, file.exists)))
        stop("All sample files must exist")
    }

    imageInputControls <- list(
      # pasted = textAreaInput(ns("inputDataPasted"),
      #                        label = "Paste", placeholder = "placeholder", rows = 5),
      fileUpload = shiny::fileInput(ns("inputDataUpload"), uploadLabel, buttonLabel = uploadButtonLabel, placeholder = uploadPlaceholder,
                                    accept = c("image/png", "image/jpeg")),
      sampleData = shiny::selectInput(ns("inputDataSample"), sampleLabel, choices = sampleFiles, selected = sampleSelected),
      url = shiny::textInput(ns("inputURL"), urlLabel, value = urlValue, placeholder = urlPlaceholder)#,
      # dsLibrary = dsDataInputUI(ns("dsFileInput"))
      )

    if (is.null(input$imageInput)) {
      return()
    } else {
      imageInputControls[[input$imageInput]]
    }
  })

  queryData <- reactive({
    query <- shiny::parseQueryString(session$clientData$url_search)
    json_str <- query[["json_data"]]
    data <- NULL
    if (!is.null(json_str)) {
      data <- jsonlite::fromJSON(URLdecode(json_str))
    }
    data
  })

  output$imageInputInfo <- renderUI({
    ns <- session$ns
    imageInputInfo <- infoList[[input$imageInput]]
    if (is.null(imageInputInfo)) return()
    imageInputInfo
  })

  inputData <- shiny::reactive({
    if (is.null(input$imageInput)) {
      warning("inputType must be one of fileUpload, sampleData, url, dsLibrary")
      return()
    }

    inputType <- input$imageInput
    queryData <- queryData()
    if (!is.null(queryData)) {
      return(queryData)
    }

    if (inputType == "fileUpload") {
      if (is.null(input$inputDataUpload))
        return()
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(), input$inputDataUpload$name)
      file.copy(old_path, path)
      df <- list(src = path)
    } else if (inputType == "sampleData") {
      file <- input$inputDataSample
      df <- list(src = file)
    } else if (inputType == "url") {
      if (sum(is.null(input$inputURL) | nzchar(input$inputURL)) == 0)
        return()
      url <- input$inputURL
      path <- file.path(tempdir(), "url0")
      t0 <- tryCatch(download.file(url, path, mode = "wb"), error = function(e) e)
      if (any(grepl("error", class(t0)))) {
        df <- list(src = "")
      } else {
        df <- list(src = path)
      }
    } else if (inputType == "dsLibrary") { # ADAPTAR PARA IMÁGENES
      df <- callModule(dsDataInput, "dsFileInput")
      df <- df()
    }
    return(df)
  })
  inputData
}
