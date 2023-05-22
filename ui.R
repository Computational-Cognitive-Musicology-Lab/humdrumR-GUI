#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(humdrumR)
library(htmlwidgets)
library(shinyAce)


# Define pieces of UI ----

## Ace Editor

ui_aceEditor <- div(
  # tags$script(src = "https://raw.githubusercontent.com/humdrum-tools/verovio-humdrum-viewer/gh-pages/scripts/ace/mode-humdrum.js"),
  # tags$script(src = "https://raw.githubusercontent.com/humdrum-tools/verovio-humdrum-viewer/gh-pages/scripts/ace/theme-humdrum_light.js"),
  aceEditor(
  'ace', 
  " ", 
  mode = "humdrum",
  theme = "humdrum_light",
  readOnly = TRUE,
  tabSize = 8,
  useSoftTabs = TRUE),
  includeScript(path="js/mode-humdrum.js"),
  includeScript(path="js/theme-humdrum_light.js")
)

## Command Line interface

ui_commandLine <- verticalLayout(
    textInput("command",
              "Command line",
              value = '', width = '600px'),
    verbatimTextOutput('console'))

## Data fields ui -----

ui_fields <- uiOutput('fieldsUI')


## Filter ui -----

ui_filter <- verticalLayout(
    selectInput('filterBy', 'Filter by', choices = c('File', 'Spine', 'Bar', 'Record')),
    actionButton('filterAction', 'Do Filter')

)

## apply transforms to data ---- 

ui_transform <- verticalLayout(
    splitLayout(selectInput('transformType', 'Transform Type', choices = c('Pitch', 'Rhythm')),
              verticalLayout(uiOutput('transformSelect') ,uiOutput('transformArguments'))),
    splitLayout(uiOutput('transformButton'), uiOutput('transformFieldName')),
    verbatimTextOutput('transformed')

)

## data plot UI
ui_plot <- verticalLayout(
  splitLayout(uiOutput('plotButton'), uiOutput('plotSelect')),
  plotOutput('plotRender')
)

## score rendering UI -----

ui_score <- div(
      # tags$script(src="https://plugin.humdrum.org/scripts/humdrum-notation-plugin-worker.js"),
      # tags$script(id="example", )
      textOutput('myhumdrum-humdrum', container = function(id, class){div(id=id, class=class, hidden='hidden')}),
      tags$script('var options = {
    		adjustPageHeight: 1,
    		font:             "Leipzig",
    		inputFrom:        "auto",
    		pageHeight:       "80%",
    		pageWidth:        1350,
    		humType:          1,
    		scale:            40,
    		spacingStaff:     12,
    		spacingSystem:    12,
    		pageMarginTop:    100,
    		spacingNonLinear: 0.6,
    		minLastJustification: 0.80,
    		spacingLinear:    0.25,
    		lyricTopMinMargin: 2.0,
    		evenNoteSpacing:  0};'), 
      tags$script("
          document.getElementById('myhumdrum-humdrum').addEventListener('DOMSubtreeModified', function() {
            updateSvgDisplay('myhumdrum', options);
          });
        "),
      div(id='myhumdrum-svg')
)

ui_Ace_andScore <- splitLayout(
  cellWidths = c("30%", "70%"), 
  verticalLayout(
    uiOutput('fileChoicesUI'),
    ui_aceEditor), 
  ui_score)


# Define UI 
shinyUI(fluidPage(
    # headers
    
    tags$head(tags$script(src="http://verovio-script.humdrum.org/scripts/verovio-toolkit.js"),
              tags$script(JS('var vrvToolkit = new verovio.toolkit()')),
              includeScript(path="js/updateVerovioSVG.js")),
    
    # Application title
    titlePanel("humdrumR"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput('filepath', 'Select a File', multiple = TRUE),
            uiOutput('spineFilterUI'),
            ui_fields,
            # ui_filter
           
        ),

        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel('View HumdrumR', verbatimTextOutput('print')),
                        tabPanel('Console', ui_commandLine),
                        tabPanel('Transform Data', ui_transform),
                        tabPanel('Plot Data', ui_plot),
                        tabPanel('Data Summary', 
                                 tabsetPanel(type = 'pills', 
                                             tabPanel('Census', verbatimTextOutput('census')),
                                             tabPanel('Reference', verbatimTextOutput('reference')),
                                             tabPanel('Spines', verbatimTextOutput('spines')),
                                             tabPanel('Interpretations', verbatimTextOutput('interpretations'))
                                             )
                                 ),
                        tabPanel('View Humdrum with Score', ui_Ace_andScore)
                        # tabPanel('View Humdrum', ui_aceEditor),
                        # tabPanel('Rendered Score', ui_score)
                        
                        )
            )
    )
))
