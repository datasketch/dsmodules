#' @export
textDocumentInputUI <- function(id,
                                choices = c("pasted", "fileUpload", "sampleData", "googleSheet", "url", "dsLibrary"),
                                selected = "pasted") {

  ns <- shiny::NS(id)
  shiny::tagList(shiny::div(id = ns("textDocumentInput"),
                            class = "tableInput",
                            shiny::radioButtons(ns("textDocumentInput"), "", choices = choices, selected = selected),
                            shiny::uiOutput(ns("textDocumentInputControls"))),
                 shiny::div(class = "box-tableInputInfo", #style = info_style,
                            shiny::uiOutput(ns("textDocumentInputInfo"))))
}


#' @export
textDocumentInput <- function(input, output, session, sampleFiles = NULL, infoList = NULL) {

  output$textDocumentInputControls <- shiny::renderUI({
    ns <- session$ns

    if (shiny::is.reactive(sampleFiles))
      sampleFiles <- sampleFiles()

    if (!is.null(input$textDocumentInput) && input$textDocumentInput == "sampleData") {
      if (!all(map_lgl(sampleFiles, file.exists)))
        stop("All sample files must exist")
    }

    textDocumentInputControls <- list(pasted = shiny::textAreaInput(ns("inputDataPasted"), label = "Paste", placeholder = "placeholder", rows = 5),
                                      fileUpload = shiny::fileInput(ns("inputDataUpload"), "Choose text, pdf file", accept = c("text/plain", ".txt", ".docx", ".pdf")),
                                      sampleData = shiny::selectInput(ns("inputDataSample"), "Select sample data", choices = sampleFiles),
                                      url = shiny::textInput(ns("inputURL"), "Page URL"),
                                      googleSheet = list(shiny::textInput(ns("inputDataGoogleSheet"), "GoogleSheet URL"), shiny::numericInput(ns("inputDataGoogleSheetSheet"), "Sheet", 1))#,
                                      # dsLibrary = dsDataInputUI(ns("dsFileInput"))
    )

    if (is.null(input$textDocumentInput)) {
      return()
    } else {
      textDocumentInputControls[[input$textDocumentInput]]
    }
  })

  queryData <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    json_str <- query[["json_data"]]
    data <- NULL
    if (!is.null(json_str)) {
      data <- jsonlite::fromJSON(URLdecode(json_str))
    }
    data
  })

  output$textDocumentInputInfo <- renderUI({
    ns <- session$ns
    textDocumentInputInfo <- infoList[[input$textDocumentInput]]
    if (is.null(textDocumentInputInfo)) return()
    textDocumentInputInfo
  })

  inputData <- shiny::reactive({
    if (is.null(input$textDocumentInput)) {
      warning("inputType must be one of pasted, fileUpload, sampleData, url, googlesheet, dsLibrary")
      return()
    }

    inputType <- input$textDocumentInput
    queryData <- queryData()
    if (!is.null(queryData)) {
      return(queryData)
    }

    if (inputType == "pasted") {
      if (is.null(input$inputDataPasted))
        return()
      if (input$inputDataPasted == "")
        return()
      tx <- input$inputDataPasted
    } else if (inputType == "fileUpload") {
      if (is.null(input$inputDataUpload))
        return()
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(), input$inputDataUpload$name)
      file.copy(old_path, path)
      print(path)
      tx <- readtext::readtext(path)$text
    } else if (inputType == "sampleData") {
      file <- input$inputDataSample
      tx <- readLines(file) %>%
        paste(collapse = "<br/>")
    } else if (inputType == "url") {
      url <- input$inputURL
      tx <- xml2::read_html(url) %>%
        xml2::xml_find_all("//p") %>%
        xml2::xml_text() %>%
        paste(collapse = "<br/>")
    } else if (inputType == "googleSheet") {
      if (is.null(input$inputDataGoogleSheet))
        return()
      if (input$inputDataGoogleSheet == "")
        return()
      # url <- input$inputDataGoogleSheet
      # ws <- input$inputDataGoogleSheetSheet
      # s <- gs_url(url)
      # tabs <- gs_ws_ls(s)
      # df <- gs_read_csv(s, ws = ws)
    } else if (inputType == "dsLibrary") { # ADAPTAR PARA IMÁGENES
      # tx <- callModule(dsDataInput, "dsFileInput")
      # tx <- df()
    }
    return(tx)
  })
  inputData
}
