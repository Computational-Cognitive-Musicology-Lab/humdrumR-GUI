library(humdrumR)
library(shiny)

readHumdrum('~/Bridge/Research/Data/Humdrum/Kern/JSBach/371chorales/chor00') -> chorales
n <- 1
showHumdrumNotation <- function(humdrumR, id = 'my-score') {
  humdrumR <- humdrumR[1]
  
  lines <- as.lines(humdrumR)
  
  output <- paste(lines, collapse = '\n')
  
  
 fluidPage(tags$h1(id),
           tags$script(paste0("displayHumdrum({source: '", id,  "', autoResize: 'true'});")),
           tags$script(id = id, type = 'text/x-humdrum', output))
   

}

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://plugin.humdrum.org/scripts/humdrum-notation-plugin-worker.js")
  ),
  selectInput('pickFile', 'Pick File', 1:length(chorales), selected = 1),
  htmlOutput('scoreviewer'))


server <- function(input, output, session) {
  output$scoreviewer <- renderUI(showHumdrumNotation(chorales[as.numeric(input$pickFile)]))
}

shinyApp(ui, server)
