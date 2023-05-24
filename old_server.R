#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(humdrumR)
library(shinyWidgets)
library(billboarder)
library(shinyAce)

humdrumR(syntax = FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
    shinyEnv <- environment()
    
    reactiveVals <- reactiveValues()
    reactiveVals$fieldChoices <- c()
    
    humData <- reactiveVal()
    
    observeEvent(input$filepath,
                             {
                                 file <- input$filepath
                                 req(file)
                                 hum <- readHumdrum(file$datapath)
                                 
                                 reactiveVals$fieldChoices <- getFieldsTree(hum)
                                 humData(hum)
                                 
                                 reactiveVals$SpineChoice <- c(1L, max(spines(humData())$Spines))
                             })
       
    output$print <- renderPrint(show(humData()[[,reactiveVals$SpineChoice[1]:reactiveVals$SpineChoice[2]]]))
    
    # updateAceEditor(session, 'ace', value = reactiveVals$humString)
    # 
    observeEvent({humData()}, {
      humString <- paste(capture.output(
        print(printHumdrum(humData(), 
                     file = as.numeric(input$fileChoice), 
                     spine_start = reactiveVals$SpineChoice[1],
                     spine_end = reactiveVals$SpineChoice[2]))),
        collapse = '\n')
      updateAceEditor(session, 'ace', value = humString)
    })
    
    
    observeEvent(input$activeFields, {
        humData(humdrumR:::selectFields(humData(), input$activeFields))
    })
    
    output$fieldsUI <- renderUI({
        selectInput('activeFields', 'Fields',
                    multiple = TRUE, selected = 'Token',
                    choices = reactiveVals$fieldChoices)
    })
    
    output$spineFilterUI <- renderUI({
        maxspines <- max(spines(humData())$Spines)
        sliderInput('spineFilterNumeric', 'Spine', min = 1, max = maxspines, step = 1L, round = TRUE,
                    value = c(1L,maxspines))
    })
    
    observeEvent(input$spineFilterNumeric, {
      reactiveVals$SpineChoice <- input$spineFilterNumeric
      humString <- paste(capture.output(
        print(printHumdrum(humData(), 
                           file = as.numeric(input$fileChoice), 
                           spine_start = reactiveVals$SpineChoice[1],
                           spine_end = reactiveVals$SpineChoice[2]))),
        collapse = '\n')
      updateAceEditor(session, 'ace', value = humString)
    })

    ## Summary tab
    output$census <- renderPrint({
        req(humData())
        census(humData())
    })
    output$reference <- renderPrint({
        req(humData())
        reference(humData())
    })
    output$spines <- renderPrint({
        req(humData())
        spines(humData())
    })
    output$interpretations <- renderPrint({
        req(humData())
        interpretations(humData())
    })
    ## Console Tab
    # codeInput <- reactive({input$command})
    # output$console <- renderPrint({
    #     expr <- try(parse(text = codeInput()), silent = TRUE)
    #     if (class(expr) != 'try-error') {
    #         eval(expr, envir = shinyEnv)
    #     } else {
    #         'type a complete R expression'
    #     }
    # })
    # 
    
    ## Transform tab
    reactiveVals$Save <- FALSE
    
    ## Render Score Tab
    
    output$fileChoicesUI <- renderUI({
      selectInput(
        'fileChoice', 
        'Choose File To Render:',
        selected = '1',
        choices = as.character(1:length(humData())))
    })
    
    outputOptions(output, "fileChoicesUI", suspendWhenHidden = FALSE)
    
    observeEvent(
      {input$fileChoice}, 
      {
        
        if(!is.null(humData())) {
          humString <- paste(capture.output(
            printHumdrum(humData(), 
                         file = as.numeric(input$fileChoice),
                         spine_start = reactiveVals$SpineChoice[1],
                         spine_end = reactiveVals$SpineChoice[2])),
            collapse = '\n')
          updateAceEditor(session, 'ace', value = humString)
        }
        output$'myhumdrum-humdrum' <- renderText(printHumdrum(humData(), 
                                                              file = as.numeric(input$fileChoice),
                                                              spine_start = reactiveVals$SpineChoice[1],
                                                              spine_end = reactiveVals$SpineChoice[2]))
        outputOptions(output, "myhumdrum-humdrum", suspendWhenHidden = FALSE)
        }
      )
    # output$'myhumdrum-humdrum' <- renderPrint(printHumdrum(humData(), as.numeric(input$fileChoice)))
    # outputOptions(output, "myhumdrum-humdrum", suspendWhenHidden = FALSE)
    
    # reactiveVals$Test <- 'this is a test'
    
    output$transformSelect <- renderUI({
        choices <- list(Pitch = c('kern', 'semits', 'pitch'),
                        Rhythm = c('recip', 'duration'))[[input$transformType]]
        selectInput('transformSelect_', 'transformFunction', choices = choices,  
                    selected = if (input$transformType == 'Pitch') 'kern' else 'recip')
    })
    output$transformArguments <- renderUI({
        if (input$transformType == 'Pitch') {
            verticalLayout(
                switchInput('pitchArg_complex', onLabel = 'complex', offLabel = 'simple', value = TRUE, size = 'mini'),
                switchInput('pitchArg_specific', onLabel = 'specific', offLabel = 'generic', value = TRUE, size = 'mini'))
        } else {
            checkboxInput('placeholder', 'rhythm arguments')
        }
        
    })
    
    output$transformFieldName <- renderUI({
        textInput('transformFieldname_', 'New Field Name', value = 'pipe')
    })
    
    output$transformButton <- renderUI({
        actionButton('transformAction', 'Execute and save to new field')
    })
    
    output$transformed <- renderPrint({
      show(humData())
    })
      
    observeEvent({input$transformAction},
        {
               
             func <- rlang::sym(input$transformSelect_)
             fieldName <- rlang::sym(input$transformFieldname_)
             # funccall <- call(func, alist(x = Token) )#, 
                              # complex = pitchArg_complex,
                              # specific = pitchArg_specific))
             funccall <- rlang::expr((!!fieldName) <- (!!func)(x = Token, 
                                            complex = !!input$pitchArg_complex,
                                            specific = !!input$pitchArg_specific))
             
             formula <- rlang::new_formula(quote(do), funccall)#,
                                           # env = as.environment(list(pitchArg_complex = input$pitchArg_complex,
                                                                     # pitchArg_specific = input$pitchArg_specific)))
             # reactiveVals$Test <- formula
             humData(eval(rlang::expr(within(humData(), !!funccall))))
             # humData(within(humData(), formula))
             updateSelectInput(session, "activeFields", selected = selectedFields(humData()))
             # transformed_data <- within(humData(), formula)
             reactiveVals$fieldChoices <- getFieldsTree(humData())
        })
    
    output$plotButton <- renderUI({
      actionButton('plotAction', 'Plot')
    })
    
    output$plotSelect <- renderUI({
      choices <- c('hist', 'boxplot')
      selectInput('plotSelect_', 'Plot Type', choices = choices,  
                  selected = 'hist')
    })
    
    observeEvent({input$plotAction}, {
      func <- rlang::sym(input$plotSelect_)
      activeField <- rlang::sym(selectedFields(humData())[1])
      funccall <- rlang::expr((!!func)(!!activeField))
      formula <- rlang::new_formula(quote(do), funccall)
      output$plotRender <- renderPlot(within(humData(), formula))
    })
})



### Tools ----

getFieldsTree <- function(humdrumR){
    fields <- fields(humdrumR)
    fields <- fields(humdrumR)[,list(list(Name)), by = Type]
    fields <- setNames(fields$V1, fields$Type)
    lapply(fields, \(fs) if (length(fs) == 1L) list(fs) else fs)
}




printHumdrum <- function(humdrumR, file=1, spine_start=1, spine_end=max(spines(humdrumR)$Spines)) {
  x <- humdrumR[file]
  x <- x[[ , spine_start:spine_end]]
  # show(x)
  lines <- as.lines(x)
  paste(lines, collapse = '\n') 
  
}
