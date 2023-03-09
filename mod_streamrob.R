# library(shiny)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(plotly)

streamrobUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  plotlyOutput(ns("streamrob"))
}

streamrobServer <- function(id, indata, hatmatrix, comparison){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      output$streamrob <- renderPlotly({
          getflowplot(indata, hatmatrix, comparison)
      })
    }
  )
}

# ui <- fluidPage(
#   streamrobUI("streamrob")
# )
# 
# server <- function(input, output, session) {
#   streamrobServer("streamrob")
# }
# 
# shinyApp(ui = ui, server = server)