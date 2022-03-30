library(shiny)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(plotly)

ui <- fluidPage(
    plotlyOutput("streamrob")
)

server <- function(input, output, session) {

    output$streamrob <- renderPlotly({
        getflowplot(indata, hatmatrix, comparison)
    })
    
}


shinyApp(ui = ui, server = server)