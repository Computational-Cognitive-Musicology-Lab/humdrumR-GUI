# load packages ----

library(shiny)
library(humdrumR)
library(shinyWidgets)
library(billboarder)
library(shinyAce)

# prep options ----
humdrumR(syntax = FALSE)


# Functions used by server ----


getFieldsTree <- function(humdrumR){
  fields <- fields(humdrumR)
  fields <- fields(humdrumR)[,list(list(Name)), by = Type]
  fields <- setNames(fields$V1, fields$Type)
  lapply(fields, \(fs) if (length(fs) == 1L) list(fs) else fs)
}




showHumdrumData <- function(humdrumR, cat = TRUE) {
  x <- humdrumR[1]
  # show(x)
  lines <- as.lines(x)
  
  lines[grepl('^!!!', lines)] <- gsub('\t*', '', lines[grepl('^!!!', lines)])
  if (cat) cat(lines, sep = '\n') else paste(lines, collapse = '\n')
  
}

showHumdrumNotation <- function(humdrumR) {
  x <- humdrumR[1]
  
  lines <- as.lines(x)
  output <- paste(lines, collapse = '\n')
  
  randomID <- paste0(sample(letters, 100, replace = TRUE), collapse = '')
  
  fluidPage(tags$script(paste0("displayHumdrum({source: '", randomID,  "', autoResize: 'true'});")),
            tags$script(id = randomID, type = 'text/x-humdrum', output))
  
  
}



# shingServer ----

shinyServer(function(input, output, session) {
    observeEvent(input$startGUI, {
      updateTabsetPanel(session, 'hidden_tabs', selected = 'GUI_panel')
    })
    
  
    shinyEnv <- environment()
    
    reactiveVals <- reactiveValues()
    reactiveVals$fieldChoices <- c()
    
    humData <- reactiveVal() # humData is the humdrumR data!
    
    observeEvent(input$filepath,
                             {
                                 file <- input$filepath
                                 req(file)
                                 hum <- readHumdrum(file$datapath)
                                 
                                 reactiveVals$fieldChoices <- getFieldsTree(hum)
                                 humData(hum)
                                 
                                 reactiveVals$SpineChoice <- c(1L, max(spines(humData())$Spines))
                             })
       
    # output$print <- renderPrint(show(humData()[[,reactiveVals$SpineChoice[1]:reactiveVals$SpineChoice[2]]]))
    
    # 
  
    
    # observeEvent(input$activeFields, {
    #     humData(humdrumR:::selectFields(humData(), input$activeFields))
    # })
    # 
    # output$fieldsUI <- renderUI({
    #     selectInput('activeFields', 'Fields',
    #                 multiple = TRUE, selected = 'Token',
    #                 choices = reactiveVals$fieldChoices)
    # })
    
    # output$spineFilterUI <- renderUI({
    #     maxspines <- max(spines(humData())$Spines)
    #     sliderInput('spineFilterNumeric', 'Spine', min = 1, max = maxspines, step = 1L, round = TRUE,
    #                 value = c(1L,maxspines))
    # })
    

    ## Rendering ui_summary ----
    output$summary_census <- renderPrint({
      req(humData())
      census(humData())
    })
    output$summary_reference <- renderPrint({
        req(humData())
        reference(humData())
    })
    output$summary_spines <- renderPrint({
        req(humData())
        spines(humData())
    })
    output$summary_interpretations <- renderPrint({
        req(humData())
        interpretations(humData())
    })
    
    ## Rendering ui_view ----
    
    
    output$view_fileSelect <- renderUI( {
      req(humData())
      
      files <- census(humData())$Filenames
      files <- setNames(seq_along(files), files)
      fluidPage(shiny::p(humdrumR:::num2print(length(humData()), capitalize = TRUE), 'files available to view.'),
                if (input$view_type %in% c('humdrumR', 'data.frame')) {
                  selectInput('view_fileSelect', 
                              'Select files to view' , multiple = TRUE,
                              choices = files, selected = 1:length(humData())) 
                  } else {
                    selectInput('view_fileSelect', 
                                'Select file to view' , multiple = FALSE,
                                choices = files, selected = 1)   
                              })
    })
    output$view_fieldSelect <- renderUI( {
      req(humData())
      
      fields <- reactiveVals$fieldChoices
      if (input$view_type %in% c('humdrumR', 'data.frame') && length(fields) > 1L) {
        
        fluidPage(shiny::p(humdrumR:::num2print(length(humData()), capitalize = TRUE), 'data fields available to view.'),
                  
                  selectInput('view_fieldSelect', 
                              'Select data fields to view' , multiple = TRUE,
                              choices = fields, selected = 'Token') )
      }

    })
    
    
    

    output$view_data <- renderPrint({ 
        req(humData())
      
        humdata <- humData()[as.integer(input$view_fileSelect)]
        if (!is.null(input$view_fieldSelect)) humdata <- humdata |> select(input$view_fieldSelect)
        print(humdata, view = if (input$view_type == 'data.frame') 'table' else 'humdrum')
      })
    
    
    observeEvent(input$view_type, {
      view_type <- switch(input$view_type,
                          humdrumR = ,
                          data.frame = 'view_data',
                          'Raw file' = 'view_ace',
                          Notation = 'view_notation')
      updateTabsetPanel(session, 'view_tabs', selected = view_type)
    })
    observeEvent(input$view_fileSelect, {
      
      req(humData())
      x <- humData()[as.integer(input$view_fileSelect)]
      humString <- showHumdrumData(x, cat = FALSE)
      updateAceEditor(session, 'ace', value = humString)
    })

    output$view_notation <- renderUI({
      req(humData())
      x <- humData()[as.integer(input$view_fileSelect)]
      showHumdrumNotation(x)
    })
    
    
    
    ## Transform tab
    # reactiveVals$Save <- FALSE
    
    ## Render Score Tab
    
    # output$'myhumdrum-humdrum' <- renderPrint(printHumdrum(humData(), as.numeric(input$fileChoice)))
    # outputOptions(output, "myhumdrum-humdrum", suspendWhenHidden = FALSE)
    
    # reactiveVals$Test <- 'this is a test'
    # 
    # output$transformSelect <- renderUI({
    #     choices <- list(Pitch = c('kern', 'semits', 'pitch'),
    #                     Rhythm = c('recip', 'duration'))[[input$transformType]]
    #     selectInput('transformSelect_', 'transformFunction', choices = choices,  
    #                 selected = if (input$transformType == 'Pitch') 'kern' else 'recip')
    # })
    # output$transformArguments <- renderUI({
    #     if (input$transformType == 'Pitch') {
    #         verticalLayout(
    #             switchInput('pitchArg_complex', onLabel = 'complex', offLabel = 'simple', value = TRUE, size = 'mini'),
    #             switchInput('pitchArg_specific', onLabel = 'specific', offLabel = 'generic', value = TRUE, size = 'mini'))
    #     } else {
    #         checkboxInput('placeholder', 'rhythm arguments')
    #     }
    #     
    # })
    # 
    # output$transformFieldName <- renderUI({
    #     textInput('transformFieldname_', 'New Field Name', value = 'pipe')
    # })
    # 
    # output$transformButton <- renderUI({
    #     actionButton('transformAction', 'Execute and save to new field')
    # })
    # 
    # output$transformed <- renderPrint({
    #   show(humData())
    # })
    #   
    # observeEvent({input$transformAction},
    #     {
    #            
    #          func <- rlang::sym(input$transformSelect_)
    #          fieldName <- rlang::sym(input$transformFieldname_)
    #          # funccall <- call(func, alist(x = Token) )#, 
    #                           # complex = pitchArg_complex,
    #                           # specific = pitchArg_specific))
    #          funccall <- rlang::expr((!!fieldName) <- (!!func)(x = Token, 
    #                                         complex = !!input$pitchArg_complex,
    #                                         specific = !!input$pitchArg_specific))
    #          
    #          formula <- rlang::new_formula(quote(do), funccall)#,
    #                                        # env = as.environment(list(pitchArg_complex = input$pitchArg_complex,
    #                                                                  # pitchArg_specific = input$pitchArg_specific)))
    #          # reactiveVals$Test <- formula
    #          humData(eval(rlang::expr(within(humData(), !!funccall))))
    #          # humData(within(humData(), formula))
    #          updateSelectInput(session, "activeFields", selected = selectedFields(humData()))
    #          # transformed_data <- within(humData(), formula)
    #          reactiveVals$fieldChoices <- getFieldsTree(humData())
    #     })
    # 
    # output$plotButton <- renderUI({
    #   actionButton('plotAction', 'Plot')
    # })
    # 
    # output$plotSelect <- renderUI({
    #   choices <- c('hist', 'boxplot')
    #   selectInput('plotSelect_', 'Plot Type', choices = choices,  
    #               selected = 'hist')
    # })
    # 
    # observeEvent({input$plotAction}, {
    #   func <- rlang::sym(input$plotSelect_)
    #   activeField <- rlang::sym(selectedFields(humData())[1])
    #   funccall <- rlang::expr((!!func)(!!activeField))
    #   formula <- rlang::new_formula(quote(do), funccall)
    #   output$plotRender <- renderPlot(within(humData(), formula))
    # })
})


