# load packages ----

library(shiny)
library(humdrumR)
library(shinyWidgets)
library(shinyAce)

# prep options ----
humdrumR(syntax = FALSE)

# Library of commands

func_library <- list(Pitch = c('kern', 'pitch', 'solfa', 'semits', 'mint', 'hint'),
                     Rhythm = c('recip', 'duration', 'dur', 'timeline', 'count', 'metcount'))

args_TF <- rbind(data.frame(Func = func_library$Pitch, Default = 'complex', Option = 'simple'),
                 data.frame(Func = func_library$Pitch, Default = 'specific', Option = 'generic'))
# Functions used by server ----

getTransformFunctions <- function(input) {
  selected <- c()
  for (type in paste0('transformFunctions_', names(func_library))) {
    selected <- c(selected, input[[type]])
  }
  selected
}



makeFuncCaller <- function(func) {
  args <- list()
  argsTable <- subset(args_TF, Func == func)
  for (i in seq_len(nrow(argsTable))) {
    row <- argsTable[i, ]
   args <- c(args, 
             list(switchInput(paste0(func, '_arg_', row$Default), onLabel = row$Default, offLabel = row$Option, value = TRUE, size = 'mini')))
  }
          do.call(tags$tr, lapply(c(list(paste0(func, '('), args, ')')), tags$td))
  
  
}


transformExpression <- function(input) {
  # simple <- if (!input$pitchArg_complex) 'simple = TRUE'
  # generic <- if (!input$pitchArg_specific) 'generic = TRUE'
  # args <- paste(c('Token', simple, generic), collapse = ', ')
  
  funcs <- getTransformFunctions(input)
  
  calls <- sapply(funcs, funcExpression, input = input)
  
  fieldName <- stringr::str_to_title(funcs)
  
  funcs <- paste(paste0(fieldName, ' = ', calls), collapse = ',\n         ')
  paste0('humdrumData |>\n',
         '  ', 'mutate(', funcs,')')
}

funcExpression <- function(func, input) {
  argsTable <- subset(args_TF, Func == func)
  
  inputs <- unlist(lapply(argsTable$Default, \(default) input[[paste0(func, '_arg_', default)]]))
  argsTable <- argsTable[!inputs, ]
  
  args <- c('Token', if (nrow(argsTable)) with(argsTable, paste0(Option, ' = TRUE')))
  
  args <- paste(args, collapse = ', ')
  
  paste0(func, '(', args, ')')
  
  
  
  
}


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
    
    
    # THIS THING DOES ALL THE WORK TO READ FILES!
    observeEvent(input$filepath,
                             {
                                 files <- input$filepath
                                 req(file)
                                 for (i in 1:nrow(files)) {
                                   file.rename(files$datapath[i], files$name[i])
                                 }
                                 hum <- readHumdrum(files$name)
                                 for (file in files$name) file.remove(file)
                                 # hum@Humtable[ , Filename := paste0('Test_', Filename)]
                                 # hum@Humtable[ , Filename := files$name[match(Filename, files$datapath)]]
                                 
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
      
      files <- unlist(census(humData())$Filenames)
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
      
      fields <- fields(humData(), 'Data')$Name 
      if (input$view_type %in% c('humdrumR', 'data.frame') && length(fields) > 1L) {
        
        fluidPage(shiny::p(humdrumR:::num2print(length(fields), capitalize = TRUE), 'data fields available to view.'),
                  
                  selectInput('view_fieldSelect', 
                              'Select data fields to view' , multiple = TRUE,
                              choices = fields, selected = 'Token') )
      }

    })
    
    
    

    output$view_data <- renderPrint({ 
        req(humData())
      
        humdata <- humData()[as.integer(input$view_fileSelect)]
        if (!is.null(input$view_fieldSelect)) humdata <- humdrumR:::selectFields(humdata, input$view_fieldSelect)
        print(humdata, 
              view = if (input$view_type == 'data.frame') 'table' else 'humdrum', 
              dataTypes = if (input$view_type == 'humdrumR') 'GLIMDd' else 'D')
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
    
    
    output$transformSelect <- renderUI({
      tabpanels <- lapply(names(func_library),
             \(type) {
               tabPanel(type, style = 'height:300px;',
                        selectizeInput(inputId = paste0('transformFunctions_', type),
                                       label = paste0(type, ' functions'),
                                       choices = func_library[[type]], multiple = TRUE, selected = NULL))
             })
      
       do.call('tabsetPanel', tabpanels)
    })
    
    output$transformFunctions <- renderUI({
      funcs <- getTransformFunctions(input)
      
      if (length(funcs)) {
        do.call(tags$table, lapply(funcs, makeFuncCaller))
        } else {
          div('No functions selected')
        }
      
    })
    # 
 
    # 
    output$transformFieldName <- renderUI({
        textInput('transformFieldname', 'New Field Name', 
                  value = stringr::str_to_title(input$transformSelect))
    })
    # 
    output$transformButton <- renderUI({
        actionButton('transformAction', 'Execute and save to new field')
    })
    # 
    output$transformExpression <- renderUI({
      pre(transformExpression(input))
    })
    #   
    observeEvent({input$transformAction},
        {
             expr <- transformExpression(input)
             expr <- gsub('humdrumData', 'humData()', expr)
             
             humData(eval(parse(text = expr)[[1]]))
        })
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


