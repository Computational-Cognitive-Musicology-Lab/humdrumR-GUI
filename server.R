# load packages ----

library(shiny)
library(humdrumR)
library(shinyWidgets)
library(shinyAce)

# prep options ----
humdrumR(syntax = FALSE)

# Library of commands

func_library <- list(Pitch = c('kern', 'pitch', 'solfa', 'semits', 'mint', 'hint'),
                     Rhythm = c('recip', 'duration', 'dur'),
                     Time = c('timeline', 'timestamp'),
                     Meter = c('count', 'metcount'))

args_library <- rbind(data.table(Func = func_library$Pitch, Type = 'TF', Arg = 'complex', Options = list(c('complex', 'simple'))), 
                      data.table(Func = func_library$Pitch, Type = 'TF', Arg = 'specific', Options = list(c('specific', 'generic'))),
                      data.table(Func = func_library$Rhythm, Type = 'select', Arg = 'scale',  Options = list(c('1', '2', '4', '8', '16', '32'))))

# Functions used by server ----



makeFuncCaller <- function(func) {
  args <- list()
  
  args_library <- args_library[Func == func]
  for (i in seq_len(nrow(args_library))) {
    with(args_library[i], 
         {
           id <- paste0(func, '_arg_', Arg)
           arg <- if (Type == 'TF') {
             switchInput(inputId = id,
                         onLabel = Options[[1]][1], offLabel = Options[[1]][2],
                         value = TRUE, size = 'mini')
           } else {
                      selectInput(inputId = id,
                                  Arg, choices = Options[[1]],
                                  selected = Options[[1]][1])
             
           }
             
           args <<- c(args, list(arg))
           
         })
  }
  # 
  do.call(tags$tr, lapply(c(list(paste0(func, '(')), 
                            args,  
                            list(')')), tags$td))
  
  
}


humdrumRexpression <- function(input, humdata) {
  # simple <- if (!input$pitchArg_complex) 'simple = TRUE'
  # generic <- if (!input$pitchArg_specific) 'generic = TRUE'
  # args <- paste(c('Token', simple, generic), collapse = ', ')
  
  expression <- 'humdrumData'

  expression <- paste(c(expression, 
                        filterCall(input, humdata),
                        mutateCall(input)), 
                      collapse = ' |>\n  ')
  expression

}

mutateCall <- function(input) {
  transformFuncs <- input$transformFunctions
  
  if (length(transformFuncs) == 0L) return(NULL)
  transformCalls <- sapply(transformFuncs, funcExpression, input = input)
  
  fieldNames <- stringr::str_to_title(transformFuncs)
  transformCalls <- paste(paste0(fieldNames, ' = ', transformCalls), 
                          collapse = ',\n         ')
  paste0('  ', 'mutate(', transformCalls, ')')
  
  
}

filterCall <- function(input, humdata) {
  predicates <- c()
  
  # filter exclusive
  exclusive <- input$filter_exclusive
  
  if (length(exclusive)) {
    exclusive <- paste0("'", exclusive, "'")
    
    if (length(exclusive) > 1L) exclusive <- paste0('c(', paste(exclusive, sep = ', '), ')')
    predicates <- c(predicates, paste0('Exclusive %in% ', exclusive))
  }
  
  # filter spine
  range <- range(humdata$Spine)
  predicates <- c(predicates,
                  if (range[1] != input$filter_spine[1]) paste0(input$filter_spine[1], ' <= Spine'),
                  if (range[2] != input$filter_spine[2]) paste0('Spine <= ', input$filter_spine[2]))
  
  
  if (length(predicates)) paste0('  filter(', paste(predicates, collapse = ' & '), ')')
}


funcExpression <- function(func, input) {
  args <- c('Token')
  
  args_library <- args_library[Func == func]
  args_library$Input <- unlist(lapply(args_library$Arg, \(default) input[[paste0(func, '_arg_', default)]]))
  
  args <- c(args, 
            args_library[Type == 'TF' & Input == FALSE, if (length(Options)) paste0(sapply(Options, '[', 2L), ' = TRUE')],
            args_library[Type == 'select', if (length(Options)) paste0(Arg, ' = "', Input, '"')])
  
  args <- paste(args, collapse = ', ')
  
  paste0(func, '(', args, ')')
  
  
  
  
}

evalExpression <- function(input, humdrumData) {
  expr <- humdrumRexpression(input, humdrumData)
  
  eval(parse(text = expr)[[1]])
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
    
    # Rendering ui_filter ----
    
    output$filter_exclusive <- renderUI( {
      req(humData())
      
      choices <- unique(humData()$Exclusive)
      
      selectizeInput('filter_exclusive', label = 'Exclusive interpretations',
                     choices = setNames(choices, paste0('**', choices)),
                     selected = NULL, multiple = TRUE)
    })
    
    output$filter_spine <- renderUI( {
      req(humData())
      
      range <- range(humData()$Spine)
      sliderInput('filter_spine',
                  'Spine',
                  min = range[1], max = range[2],
                  value = range, step = 1)
     
    })
    
    # Rendering ui_transform ----
    
    
    output$transformSelect <- renderUI({
                        selectizeInput(inputId = 'transformFunctions',
                                       label = 'Select transform functions', 
                                       choices = func_library, multiple = TRUE, selected = NULL)
      })
      
    
    output$transformFunctions <- renderUI({
      funcs <- input$transformFunctions
      
      if (length(funcs)) {
        do.call(tags$table, lapply(funcs, makeFuncCaller))
        } else {
          div('No functions selected')
        }
      
    })
    # 
 
    
    # 
    output$humdrumRexpression1 <- renderUI({ pre(humdrumRexpression(input, humData())) })
    output$humdrumRexpression2 <- renderUI({ pre(humdrumRexpression(input, humData())) })
    #   
  
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
      
      fields <- c('Token', stringr::str_to_title(input$transformFunctions))
      if (input$view_type %in% c('humdrumR', 'data.frame') && length(fields) > 1L) {
        
        fluidPage(shiny::p(humdrumR:::num2print(length(fields) , capitalize = TRUE), 'data fields available to view.'),
                  
                  selectInput('view_fieldSelect', 
                              'Select data fields to view' , multiple = TRUE,
                              choices = fields, selected = 'Token') )
      }

    })
    
    
    

    output$view_data <- renderPrint({ 
        req(humData())
      
        humdata <- humData()[as.integer(input$view_fileSelect)]
        humdata <- evalExpression(input, humdata)
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
    
    ## Rendering ui_tabulate ----
    
    output$tabulate <- renderPlot({
      req(humData())
      
      humdata <- humData()[as.integer(input$view_fileSelect)]
      humdata <- evalExpression(input, humdata)
      if (!is.null(input$view_fieldSelect)) humdata <- humdrumR:::selectFields(humdata, input$view_fieldSelect)
      
      tally(humdata) |> draw()
      
    })

    
})


