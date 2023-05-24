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

## File load ui ----


ui_fileLoad <- fluidPage(
  h1("Welcome to humdrumR!"),
  shiny::p("HumdrumR is a software toolk fit analyzing musical data encoded in the humdrum syntax.",
    "The main page for HumdrumR is", a('here', href= "https://computational-cognitive-musicology-lab.github.io/humdrumR/index.html"), "."),
  shiny::p("This page provides a graphical user interface (GUI) for using the humdrumR toolkit. To get started, use the dialog below to load humdrum",
           "files from your computer."),
  fileInput('filepath', 'Select a File', multiple = TRUE))

## Ace Editor


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

## data plot UI ----

ui_plot <- verticalLayout(
  splitLayout(uiOutput('plotButton'), uiOutput('plotSelect')),
  plotOutput('plotRender')
)

## data summary UI ----

ui_summary <- tabsetPanel(type = 'pills', 
                          tabPanel('Census', verbatimTextOutput('summary_census')),
                          tabPanel('Reference', verbatimTextOutput('summary_reference')),
                          tabPanel('Spines', verbatimTextOutput('summary_spines')),
                          tabPanel('Interpretations', verbatimTextOutput('summary_interpretations')))

## view UI -----


ui_aceEditor <- div(
  # tags$script(src = "https://raw.githubusercontent.com/humdrum-tools/verovio-humdrum-viewer/gh-pages/scripts/ace/mode-humdrum.js"),
  # tags$script(src = "https://raw.githubusercontent.com/humdrum-tools/verovio-humdrum-viewer/gh-pages/scripts/ace/theme-humdrum_light.js"),
  aceEditor(
    'ace',
    "Load some humdrum data!",
    mode = "humdrum",
    theme = "humdrum_light",
    readOnly = FALSE,
    showLineNumbers = TRUE,
    fontSize = 14,
    tabSize = 30,
    useSoftTabs = TRUE),
  includeScript(path="js/mode-humdrum.js"),
  includeScript(path="js/theme-humdrum_light.js")
)


ui_view <- sidebarLayout(sidebarPanel(uiOutput("view_fileSelect")),
                         mainPanel(tabsetPanel(type = 'pills',
                                               tabPanel('Data view', ui_aceEditor),
                                               # tabPanel('Data view', verbatimTextOutput('view_data')),
                                               tabPanel('Notation view',  htmlOutput("view_notation")))) )
# ui_score <- div(
#       # tags$script(src="https://plugin.humdrum.org/scripts/humdrum-notation-plugin-worker.js"),
#       # tags$script(id="example", )
#       textOutput('myhumdrum-humdrum', container = function(id, class){div(id=id, class=class, hidden='hidden')}),
#       tags$script('var options = {
#     		adjustPageHeight: 1,
#     		font:             "Leipzig",
#     		inputFrom:        "auto",
#     		pageHeight:       "80%",
#     		pageWidth:        1350,
#     		humType:          1,
#     		scale:            40,
#     		spacingStaff:     12,
#     		spacingSystem:    12,
#     		pageMarginTop:    100,
#     		spacingNonLinear: 0.6,
#     		minLastJustification: 0.80,
#     		spacingLinear:    0.25,
#     		lyricTopMinMargin: 2.0,
#     		evenNoteSpacing:  0};'), 
#       tags$script("
#           document.getElementById('myhumdrum-humdrum').addEventListener('DOMSubtreeModified', function() {
#             updateSvgDisplay('myhumdrum', options);
#           });
#         "),
#       div(id='myhumdrum-svg')
# )

# ui_Ace_andScore <- splitLayout(
#   cellWidths = c("30%", "70%"), 
#   verticalLayout(
#     uiOutput('fileChoicesUI'),
#     ui_aceEditor), 
#   ui_score)




# Define UI ----

shinyUI(fluidPage(

  tags$head(tags$script(src = "https://plugin.humdrum.org/scripts/humdrum-notation-plugin-worker.js")),
  
  titlePanel("humdrumR"),
  mainPanel(
    tabsetPanel(type = 'tabs',
                tabPanel('Upload humdrum', ui_fileLoad),
                tabPanel('Filter data', ui_fileLoad),
                tabPanel('Data summaries', ui_summary),
                tabPanel('View Humdrum', ui_view),
                # tabPanel('Transform Data', ui_transform),
                # tabPanel('Plot Data', ui_plot)
                ))
  ))

