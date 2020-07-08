
#' @export
tableInputUI <- function(id,
                         choices = c("pasted", "fileUpload", "sampleData", "googleSheets"),
                         choicesInline = FALSE,
                         selected = "pasted", ...) {
  # UI
  ns <- shiny::NS(id)
  #choiceNames <-  choiceNames %||% choices
  #names(choices) <- choiceNames

  #info_style <- ifelse(is.null(uiOutput(ns("tableInputInfo"))), "display:flex;", "display:none;")

  shiny::tagList(shiny::div(id = ns("tableInput"),class="tableInput",
                            shiny::radioButtons(ns("tableInput"), "",
                                                choices = choices, selected = selected, inline = choicesInline),
                            shiny::uiOutput(ns("tableInputControls"))),
                 shiny::div(class = "box-tableInputInfo", #style = info_style,
                            shiny::uiOutput(ns("tableInputInfo"))))

}

#' @export
tableInput <- function(input, output, session,
                       infoList = NULL,
                       pasteLabel = "Paste", pasteValue = "", pastePlaceholder = "Select your data and paste it here", pasteRows = 5,
                       uploadLabel = "Choose CSV File", uploadButtonLabel = "Browse...", uploadPlaceholder = "No file selected",
                       sampleLabel = "Select a sample data", sampleFiles = NULL, sampleSelected = NULL,
                       googleSheetLabel = "Data from Google Sheet", googleSheetValue = "", googleSheetPlaceholder = "https://docs.google.com/spreadsheets/...",
                       googleSheetPageLabel = "Sheet",
                       ...) {

  output$tableInputControls <- shiny::renderUI({

    # str(session)
    # if(!exists(session))
    #   stop("No session defined in server.")

    ns <- session$ns

    if (shiny::is.reactive(sampleFiles))
      sampleFiles <- sampleFiles()

    if(input$tableInput == "sampleData"){
      if (!all(unlist(lapply(sampleFiles, file.exists))))
        stop("All Sample Files must exist")
    }

    tableInputControls <- list(pasted = textAreaInput(ns("inputDataPasted"), label = pasteLabel, value = pasteValue, placeholder = pastePlaceholder, rows = pasteRows),
                               fileUpload =  fileInput(ns("inputDataUpload"), uploadLabel, buttonLabel = uploadButtonLabel, placeholder = uploadPlaceholder,
                                                         accept = c("text/csv", "text/comma-separated-values, text/plain", ".csv", ".xls", ".xlsx")),
                               sampleData = selectInput(ns("inputDataSample"), sampleLabel, choices = sampleFiles, selected = sampleSelected),
                               googleSheets = list(shiny::textInput(ns("inputDataSheet"), label = googleSheetLabel, value = googleSheetValue, placeholder = googleSheetPlaceholder),
                                                   shiny::numericInput(ns("inputDataGoogleSheetSheet"), googleSheetPageLabel, 1))
    )
    tableInputControls[[input$tableInput]]
  })

  output$tableInputInfo <- shiny::renderUI({
    ns <- session$ns
    tableInputInfo <- infoList[[input$tableInput]]
    if (is.null(tableInputInfo)) return()
    tableInputInfo
  })

  inputData <- shiny::reactive({
    inputType <- input$tableInput
    #readDataFromInputType(inputType)
    if(inputType == "pasted"){
      if (is.null(input$inputDataPasted)) return()
      if(input$inputDataPasted == "")
        return()
      df <- read_tsv(input$inputDataPasted)
    }
    if(inputType ==  "fileUpload"){
      if(is.null(input$inputDataUpload)) return()
      old_path <- input$inputDataUpload$datapath
      path <- file.path(tempdir(),input$inputDataUpload$name)
      file.copy(old_path,path)
      df <- rio::import(path)
    }
    if(inputType ==  "sampleData"){
      if (is.null(input$inputDataSample)) return()
      file <- as.character(input$inputDataSample)
      df <- readr::read_csv(file)
    }
    if (inputType == "googleSheets") {
      if (is.null(input$inputDataSheet)) return()
      if (input$inputDataSheet == "") return()
      library(googlesheets4)
      googlesheets4::sheets_deauth()
      id_file <- gsub(".*\\/d\\/|\\/edit.*", '', input$inputDataSheet)
      googlesheets4::sheets_get(id_file)
      df <- googlesheets4::read_sheet(id_file)
    }
    return(df)
  })

  inputData
}

