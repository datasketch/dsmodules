
#' @export
tableInputUI <- function(id,
                         choices = c("pasted","fileUpload","sampleData", "googleSheets"),
                         selected = "pasted", ...) {
  # UI
  ns <- NS(id)
  #choiceNames <-  choiceNames %||% choices
  #names(choices) <- choiceNames

  #info_style <- ifelse(is.null(uiOutput(ns("tableInputInfo"))), "display:flex;", "display:none;")

  tagList(
    div(id=ns("tableInput"),class="tableInput",
        radioButtons(ns("tableInput"), "",
                     choices = choices, selected = selected),
        uiOutput(ns("tableInputControls"))
    ),
    div(class = "box-tableInputInfo", #style = info_style,
        uiOutput(ns("tableInputInfo"))
    )
  )
}

#' @export
tableInput <- function(input,output,session,
                       sampleFiles = NULL, infoList = NULL, ...){

  output$tableInputControls <- renderUI({

    # str(session)
    # if(!exists(session))
    #   stop("No session defined in server.")

    ns <- session$ns

    if(is.reactive(sampleFiles))
      sampleFiles <- sampleFiles()

    if(input$tableInput == "sampleData"){
      if(!all(map_lgl(sampleFiles,file.exists)))
        stop("All Sample Files must exist")
    }

    tableInputControls <- list(
      "pasted" = textAreaInput(ns("inputDataPasted"),label = "Paste",
                               placeholder = "Select your data and paste it here",
                               rows = 5),
      "fileUpload" =  fileInput(ns('inputDataUpload'), 'Choose CSV File',
                                accept=c('text/csv',
                                         'text/comma-separated-values,text/plain',
                                         '.csv','.xls', '.xlsx')),
      "sampleData" = selectInput(ns("inputDataSample"),"Select a sample data",
                                 choices = sampleFiles),
      "googleSheets" = textInput(ns("inputDataSheet"), label = "Data from Google Sheet", placeholder = "https://docs.google.com/spreadsheets/...")
    )
    tableInputControls[[input$tableInput]]
  })

  output$tableInputInfo <- renderUI({
    ns <- session$ns
    tableInputInfo <- infoList[[input$tableInput]]
    if (is.null(tableInputInfo)) return()
    tableInputInfo
  })

  inputData <- reactive({
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
      df <- read_csv(file)
    }
    if (inputType == "googleSheets") {
      if (is.null(input$inputDataSheet)) return()
      if (input$inputDataSheet == "") return()
      googlesheets4::sheets_deauth()
      id_file <- gsub(".*\\/d\\/|\\/edit.*", '', input$inputDataSheet)
      googlesheets4::sheets_get(id_file)
      df <- googlesheets4::read_sheet(id_file)
    }
    return(df)
  })

  inputData
}

