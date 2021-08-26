
#' @export
tableInputUI <- function(id, label,
                         choices = c("pasted", "fileUpload", "sampleData", "googleSheets"),
                         choicesInline = FALSE,
                         selected = "pasted", ...) {
  # UI
  ns <- shiny::NS(id)
  #choiceNames <-  choiceNames %||% choices
  #names(choices) <- choiceNames

  #info_style <- ifelse(is.null(uiOutput(ns("tableInputInfo"))), "display:flex;", "display:none;")
  styles <- ".unticked {
                 display: none;
                 }
                 .ticked {
                 display: block;
                 }
                 "

  shiny::tagList(singleton(tags$head(tags$style(HTML(styles)))),
                 shiny::div(id = ns("tableInput"), class="tableInput",
                            shiny:::shinyInputLabel("inputId", label = label),
                            shiny::radioButtons(ns("tableInput"), "",
                                                choices = choices, selected = selected,
                                                inline = choicesInline),
                            shiny::uiOutput(ns("tableInputControls"))),
                 shiny::div(class = "box-tableInputInfo", #style = info_style,
                            shiny::uiOutput(ns("tableInputInfo"))))

}

#' @export
tableInputServer <- function(id,
                             infoList = NULL,
                             pasteLabel = "Paste", pasteValue = "",
                             pastePlaceholder = "Select your data and paste it here", pasteRows = 5,
                             uploadLabel = "Choose CSV/XLS file", uploadButtonLabel = "Browse...",
                             uploadPlaceholder = "No file selected",
                             sampleLabel = "Select a sample data",
                             sampleFiles = NULL, sampleSelected = NULL,
                             googleSheetLabel = "Data from Google Sheet", googleSheetValue = "",
                             googleSheetPlaceholder = "https://docs.google.com/spreadsheets/...",
                             googleSheetPageLabel = "Sheet",
                             showAdvancedOptionsButton = FALSE,
                             advancedOptionsLabel = "Advanced options",
                             delimiterLabel = "Delimiter",
                             delimiterChoiceLabels = c("Comma", "Tab", "Space", "Semi-colon"),
                             decimalMarkLabel = "Decimal mark",
                             decimalMarkChoiceLabels = c("Point", "Comma"),
                             ...){

  moduleServer(id,function(input, output, session) {

    ns <- session$ns

    accept_formats <- c("text/csv", "text/comma-separated-values, text/plain", ".csv", ".xls", ".xlsx")

    # define input UIs for pasted, fileUpload, sampleData, and googleSheets

    sampleDataUI <- function(sampleLable, sampleFiles, sampleSelected){
      sampleData_html <- NULL

      sampleFiles <- eval_reactives(sampleFiles)

      if(all(unlist(lapply(sampleFiles, class)) == "character")){
        sampleData_html <- shiny::selectInput(ns("inputDataSample"), sampleLabel,
                                              choices = sampleFiles, selected = sampleSelected)
      } else if (all(unlist(lapply(sampleFiles, class)) == "data.frame")){
        if(is.null(names(sampleFiles)))
          stop("sampleFiles list must be named")
        sampleData_html <- shiny::selectInput(ns("inputDataSample"), sampleLabel,
                                              choices = names(sampleFiles), selected = sampleSelected)
      }
      else{
        stop("All sample data must be either file paths or data.frames")
      }
      sampleData_html
    }

    googleSheetsUI <- list(shiny::textInput(ns("inputDataSheet"), googleSheetLabel, value = googleSheetValue,
                                            placeholder = googleSheetPlaceholder),
                           shiny::numericInput(ns("inputDataGoogleSheetSheet"),
                                               googleSheetPageLabel, 1))

    pastedUI <- shiny::textAreaInput(ns("inputDataPasted"), label = pasteLabel, value = pasteValue,
                                     placeholder = pastePlaceholder, rows = pasteRows)

    fileUploadUI <- shiny::fileInput(ns("inputDataUpload"), uploadLabel,buttonLabel = uploadButtonLabel,
                                     placeholder = uploadPlaceholder, accept = accept_formats)

    advancedOptionsButton <- shiny::checkboxInput(ns("advancedOptions"), label = advancedOptionsLabel)

    delimiterUI <- shiny::radioButtons(ns("delimiter"), label = delimiterLabel, choiceValues = c("comma", "tab", "space", "semi-colon"),
                                       choiceNames = delimiterChoiceLabels, selected = "comma", inline = FALSE)

    decimalMarkUI <- shiny::radioButtons(ns("decimalMark"), label = decimalMarkLabel, choiceValues = c("point", "comma"),
                                         choiceNames = decimalMarkChoiceLabels, selected = "point", inline = FALSE)

    observe({

      if(identical(input$tableInput,"pasted")){
        updateRadioButtons(session, "delimiter", selected = "tab")
      }

    })

    # observe({
    #   if(!is.null(input$delimiter)){
    #     if(input$delimiter == "comma"){
    #       if(input$decimalMark == "comma"){
    #         updateRadioButtons(session, "decimalMark", selected = "point")
    #       }
    #     }
    #   }
    # })


    if(showAdvancedOptionsButton){

      advancedOptions <- div(id = "adv_opts", class = "unticked",
                             delimiterUI,
                             decimalMarkUI)


      pastedUI <- list(pastedUI,
                       advancedOptionsButton,
                       advancedOptions)

      fileUploadUI <- list(fileUploadUI,
                           advancedOptionsButton,
                           advancedOptions)

    }

    observeEvent(input$advancedOptions,{
      if(input$advancedOptions){
        shinyjs::runjs(code = '$("#adv_opts").removeClass("unticked");
                          $("#adv_opts").addClass("ticked");')
      } else {
        shinyjs::runjs(code = '$("#adv_opts").removeClass("ticked");
                          $("#adv_opts").addClass("unticked");')
      }
    })



    output$tableInputControls <- shiny::renderUI({
      # define list of input UIs
      tableInputControls <- list(pasted = pastedUI,
                                 fileUpload = fileUploadUI,
                                 sampleData = sampleDataUI(sampleLable, sampleFiles, sampleSelected),
                                 googleSheets = googleSheetsUI)
      tableInputControls[[input$tableInput]]
    })


    output$tableInputInfo <- shiny::renderUI({
      ns <- session$ns
      tableInputInfo <- infoList[[input$tableInput]]
      if (is.null(tableInputInfo)) return()
      tableInputInfo
    })


    inputData <- shiny::reactive({
      req(input$tableInput)

      delimiter <- ","
      if(identical(input$tableInput,"pasted")){
        delimiter <- "\t"
      }
      if(!is.null(input$delimiter)){
        if(!input$delimiter %in% c("comma", "tab", "space", "semi-colon")) stop("Delimiter needs to be one of 'comma', 'tab', 'space', or 'semi-colon'.")
        if(input$delimiter == "comma") delimiter <- ","
        if(input$delimiter == "tab") delimiter <- "\t"
        if(input$delimiter == "space") delimiter <- " "
        if(input$delimiter == "semi-colon") delimiter <- ";"
      }

      decimal_mark <- "."
      if(!is.null(input$decimalMark)){
        if(input$decimalMark == "comma") decimal_mark <- ","
      }

      inputType <- input$tableInput
      if(inputType == "pasted"){
        if (is.null(input$inputDataPasted)) return()
        if(input$inputDataPasted == "")
          return()

        df <- tryCatch(readr::read_delim(input$inputDataPasted, locale = readr::locale(decimal_mark = decimal_mark), delim = delimiter),
                       error=function(cond) return())

      }
      if(inputType ==  "fileUpload"){
        if(is.null(input$inputDataUpload)) return()
        path <- input$inputDataUpload$datapath

        if (grepl(".csv", path)) {
          df <- readr::read_delim(path, locale = readr::locale(decimal_mark = decimal_mark), delim = delimiter)
        } else if (grepl(".xlsx", path)){
          df <- openxlsx::read.xlsx(path, detectDates = TRUE)
        } else {
          df <- tryCatch(rio::import(path, fread = FALSE, check.names = FALSE),
                         error = function(e) rio::import(path))
        }
      }
      if(inputType ==  "sampleData"){
        if (is.null(input$inputDataSample)) return()
        inputDataSample <- input$inputDataSample
        # message("in server")
        # str(sampleFiles)
        sampleFiles <- eval_reactives(sampleFiles)
        # str(sampleFiles)
        # message("in server end")

        # str(unlist(lapply(sampleFiles, class)))

        df <- NULL

        if(all(unlist(lapply(sampleFiles, class)) == "character")){
          file <- as.character(inputDataSample)
          if(!grepl(".csv", file)) return()
          df <- readr::read_csv(file)
        }else if(all(unlist(lapply(sampleFiles, class)) == "data.frame")){
          if(!inputDataSample %in% names(sampleFiles)) return()
          df <- sampleFiles[[inputDataSample]]
        }
        else{
          stop("Error reading sampleFile all sample data must be either file paths or data.frames")
        }
        df
      }
      if (inputType == "googleSheets") {
        if (is.null(input$inputDataSheet)) return()
        if (input$inputDataSheet == "") return()
        library(googlesheets4)
        googlesheets4::gs4_deauth()
        id_file <- gsub(".*\\/d\\/|\\/edit.*", '', input$inputDataSheet)
        googlesheets4::gs4_get(id_file)

        sheet <- input$inputDataGoogleSheetSheet
        if(is.null(sheet)) sheet <- 1

        df <- googlesheets4::read_sheet(id_file, sheet = sheet)
      }
      df <- discard_all_na_cols(df)
      return(df)
    })
    inputData
  })
}

