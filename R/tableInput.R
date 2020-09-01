
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

  shiny::tagList(
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
tableInputServer <- function(id, infoList = NULL,
                             pasteLabel = "Paste", pasteValue = "",
                             pastePlaceholder = "Select your data and paste it here", pasteRows = 5,
                             uploadLabel = "Choose CSV/XLS file", uploadButtonLabel = "Browse...",
                             uploadPlaceholder = "No file selected",
                             sampleLabel = "Select a sample data",
                             sampleFiles = NULL, sampleSelected = NULL,
                             googleSheetLabel = "Data from Google Sheet", googleSheetValue = "",
                             googleSheetPlaceholder = "https://docs.google.com/spreadsheets/...",
                             googleSheetPageLabel = "Sheet",
                             ...){

  moduleServer(id,function(input, output, session) {

    output$tableInputControls <- shiny::renderUI({

      # str(session)
      # if(!exists(session))
      #   stop("No session defined in server.")

      ns <- session$ns

      # if (shiny::is.reactive(sampleFiles))
      #   sampleFiles <- sampleFiles()

      # if(input$tableInput == "sampleData"){
      #   if (!all(unlist(lapply(sampleFiles, file.exists))))
      #     stop("All Sample Files must exist")
      # }

      accept_formats <- c("text/csv", "text/comma-separated-values, text/plain", ".csv", ".xls", ".xlsx")

      sampleDataUI <- function(sampleLable, sampleFiles, sampleSelected){
        sampleData_html <- NULL

        # message("sampleFiles")
        # str(sampleFiles)
        sampleFiles <- eval_reactives(sampleFiles)
        # str(sampleFiles)

        if(all(unlist(lapply(sampleFiles, class)) == "character")){
          sampleData_html <- selectInput(ns("inputDataSample"), sampleLabel,
                                         choices = sampleFiles, selected = sampleSelected)
        } else if (all(unlist(lapply(sampleFiles, class)) == "data.frame")){
          if(is.null(names(sampleFiles)))
            stop("sampleFiles list must be named")
          sampleData_html <- selectInput(ns("inputDataSample"), sampleLabel,
                                         choices = names(sampleFiles), selected = sampleSelected)
        }
        else{
          stop("All sample data must be either file paths or data.frames")
        }
        sampleData_html
      }


      tableInputControls <- list(
        pasted = textAreaInput(ns("inputDataPasted"), label = pasteLabel, value = pasteValue,
                               placeholder = pastePlaceholder, rows = pasteRows),
        fileUpload =  fileInput(ns("inputDataUpload"), uploadLabel,buttonLabel = uploadButtonLabel,
                                placeholder = uploadPlaceholder, accept = accept_formats),
        sampleData = sampleDataUI(sampleLable, sampleFiles, sampleSelected),
        googleSheets = list(textInput(ns("inputDataSheet"), googleSheetLabel, value = googleSheetValue,
                                      placeholder = googleSheetPlaceholder),
                            numericInput(ns("inputDataGoogleSheetSheet"),
                                         googleSheetPageLabel, 1))
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
      if(inputType == "pasted"){
        if (is.null(input$inputDataPasted)) return()
        if(input$inputDataPasted == "")
          return()
        df <- readr::read_tsv(input$inputDataPasted)
      }
      if(inputType ==  "fileUpload"){
        if(is.null(input$inputDataUpload)) return()
        old_path <- input$inputDataUpload$datapath
        path <- file.path(tempdir(),input$inputDataUpload$name)
        file.copy(old_path, path)
        df <- tryCatch(rio::import(path, fread = FALSE, check.names = FALSE),
                       error = function(e) rio::import(path))
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
          df <- readr::read_csv(file)
        }else if(all(unlist(lapply(sampleFiles, class)) == "data.frame")){
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
        googlesheets4::sheets_deauth()
        id_file <- gsub(".*\\/d\\/|\\/edit.*", '', input$inputDataSheet)
        googlesheets4::sheets_get(id_file)
        df <- googlesheets4::read_sheet(id_file)
      }
      return(df)
    })
    inputData
  })
}

