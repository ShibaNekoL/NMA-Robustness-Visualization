library(shiny)

# Define UI for data upload app ----
uploadfileUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    # App title ----
    titlePanel(ns("Uploading Files")),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Select a file ----
        fileInput(ns("file1"), "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput(ns("header"), "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons(ns("sep"), "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),
        
        # Input: Select quotes ----
        radioButtons(ns("quote"), "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Select number of rows to display ----
        radioButtons(ns("disp"), "Display",
                     choices = c(Head = "head",
                                 All = "all"),
                     selected = "head")
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output: Data file ----
        tableOutput(ns("contents"))
        
      )
      
    )
  )
}

# Define server logic to read selected file ----
uploadfileServer <- function(id, stringsAsFactors){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
    
      output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        if(input$disp == "head") {
          return(head(df))
        }
        else {
          return(df)
        }
    })
  }
)}


ui <- fluidPage(
  uploadfileUI("uploadfile")
)

server <- function(input, output, session) {
  uploadfileServer("uploadfile")
}


# Run the app ----

shinyApp(ui = ui, server = server)