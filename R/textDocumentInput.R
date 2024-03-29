#' @export
textDocumentInputUI <- function(id,
                                choices = c("pasted", "fileUpload", "sampleData", "googleSheet", "url", "dsLibrary"),
                                choicesInline = FALSE,
                                selected = "pasted") {

  ns <- shiny::NS(id)
  shiny::tagList(shiny::div(id = ns("textDocumentInput"),
                            class = "tableInput",
                            shiny::radioButtons(ns("textDocumentInput"), "", choices = choices, selected = selected, inline = choicesInline),
                            shiny::uiOutput(ns("textDocumentInputControls"))),
                 shiny::div(class = "box-tableInputInfo", #style = info_style,
                            shiny::uiOutput(ns("textDocumentInputInfo"))))
}


#' @export
textDocumentInput <- function(input, output, session,
                              infoList = NULL,
                              pasteLabel = "Paste", pasteValue = "", pastePlaceholder = "Select your text and paste it here", pasteRows = 5,
                              uploadLabel = "Choose DOC/TXT/PDF file", uploadButtonLabel = "Browse...", uploadPlaceholder = "No file selected",
                              sampleLabel = "Select a sample data", sampleFiles = NULL, sampleSelected = NULL,
                              urlLabel = "Page URL", urlValue = "", urlPlaceholder = NULL,
                              googleDocLabel = "Google document URL", googleDocValue = "", googleDocPlaceholder = "https://docs.google.com/spreadsheets/...") {

  output$textDocumentInputControls <- shiny::renderUI({
    ns <- session$ns

    if (shiny::is.reactive(sampleFiles))
      sampleFiles <- sampleFiles()

    # if (!is.null(input$textDocumentInput) && input$textDocumentInput == "sampleData") {
    #   if (!all(unlist(lapply(sampleFiles, file.exists))))
    #     stop("All sample files must exist")
    # }

    textDocumentInputControls <- list(pasted = shiny::textAreaInput(ns("inputDataPasted"), label = pasteLabel, placeholder = pastePlaceholder, rows = pasteRows),
                                      fileUpload = shiny::fileInput(ns("inputDataUpload"), uploadLabel, buttonLabel = uploadButtonLabel, placeholder = uploadPlaceholder,
                                                                    accept = c("text/plain", ".txt", ".docx", ".pdf")),
                                      sampleData = shiny::selectInput(ns("inputDataSample"), sampleLabel, choices = sampleFiles, selected = sampleSelected),
                                      url = shiny::textInput(ns("inputURL"), urlLabel, value = urlValue, placeholder = urlPlaceholder),
                                      googleSheet = shiny::textInput(ns("inputDataGoogleDoc"), googleDocLabel, value = googleDocValue, placeholder = googleDocPlaceholder)#,
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

  output$textDocumentInputInfo <- shiny::renderUI({
    ns <- session$ns
    textDocumentInputInfo <- infoList[[input$textDocumentInput]]
    if (is.null(textDocumentInputInfo)) return()
    textDocumentInputInfo
  })

  inputData <- shiny::reactive({
    req(input$textDocumentInput)
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
      path <- input$inputDataUpload$datapath
      tx <- readtext::readtext(path)$text
    } else if (inputType == "sampleData") {
      if (is.null(input$inputDataSample))
        return()
      file <- input$inputDataSample
      if (grepl(".txt", as.character(input$inputDataSample))) {
      tx <- readLines(file) %>%
        paste(collapse = "<br/>")
      } else {
        tx <-  as.character(input$inputDataSample[[1]])
      }
    } else if (inputType == "url") {
      if (is.null(input$inputURL))
        return()
      if (input$inputURL == "")
        return()
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
