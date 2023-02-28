library(shiny)
library(DT)
library(shinyjs)
library(dplyr)
source("./flow_contribution/R/hatmatrix.R")

# Define UI for data upload app ----
uploadfileUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  shinyjs::useShinyjs()
  
  tagList(
    # App title ----
    titlePanel("Uploading File"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        # Input: Select a file ----
        fileInput(
          ns("file1"),
          "Choose CSV File",
          multiple = F,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput(ns("header"), "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons(
          ns("sep"),
          "Separator",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ","
        ),
        
        # Input: Select quotes ----
        radioButtons(
          ns("quote"),
          "Quote",
          choices = c(
            None = "",
            "Double Quote" = '"',
            "Single Quote" = "'"
          ),
          selected = ""
        ),
        
        # # Horizontal line ----
        # tags$hr()
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(# Output: Data file ----
                
                dataTableOutput(ns("table")))
      
    ),
    
    tags$hr(),
    
    hidden(
      tags$div(
        id = ns('selecting_columns_title'),
        titlePanel("Selecting Columns")
      )
    )
    ,
    
    # Input: Select data type ----
    hidden(
      radioButtons(
        ns("radio_data_type"),
        label = "Data Type",
        choices = list(# "Wide Binary" = "wide_binary",
          # "Wide Continuous" = "wide_continuous",
          "Long Binary" = "long_binary",
          "Long Continuous" = "long_continuous"),
        selected = "long_binary"
      )
    )
    ,
    
    # Input: Select summary measure
    hidden(
      tags$div(
        id = ns('hmat'),
        # "RD", "RR", "OR", "ASD", "HR", "MD", "SMD", or "ROM"
        radioButtons(
          ns("box_sm"),
          label = "Summary Measure",
          choices = list(
            "RD" = "RD",
            "RR" = "RR",
            "OR" = "OR",
            "ASD" = "ASD",
            "HR" = "HR",
            "MD" = "MD",
            "SMD" = "SMD",
            "ROM" = "ROM"
          )
        )
      )
    )
    ,
    hidden(
      tags$div(
        id = ns('choose_column_long_binary'),
        # treat=t,event=r,n=n, data=D, studlab = id, sm= sm
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_treat"), label = "Treatment",
              choices = NULL
            )),
        
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_event"), label = "Event",
              choices = NULL
            )),
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_n"), label = "n",
              choices = NULL
            )),
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_studlab"), label = "Study",
              choices = NULL
            )),
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_rob"), label = "Risk of Bias",
              choices = NULL
            ))
      )
    ),
    
    hidden(
      tags$div(
        id = ns('choose_column_long_continuous'),
        
        # treat=t,mean=y,sd=sd,n=n,data=D, studlab =id, sm=sm
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_treat_c"), label = "Treatment",
              choices = NULL
            )),
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_mean_c"), label = "Mean",
              choices = NULL
            )),
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_sd_c"), label = "sd",
              choices = NULL
            )),
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_n_c"), label = "n",
              choices = NULL
            )),
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_studlab_c"), label = "Study",
              choices = NULL
            )),
        div(style = "display: inline-block;vertical-align:top; width: 150px;",
            selectInput(
              ns("box_rob_c"), label = "Risk of Bias",
              choices = NULL
            ))
      )
    ),
    hidden(
      actionButton(ns("btn_confirm"), "Start Analyzing")
    )

  )
}

# Define server logic to read selected file ----
uploadfileServer <- function(id, stringsAsFactors) {
  moduleServer(id,
               ## Below is the module function ----
               function(input, output, session) {
                 observeEvent(input$file1, {
                   # data <- input$file1
                   
                   # if (is.null(data)) {
                   #   hide(id = "radio_data_type")
                   #   hide(id = "choose_column_long_binary")
                   #   hide(id = "choose_column_long_continuous")
                   #   hide(id = "btn_confirm")
                   #   hide(id = "selecting_columns_title")
                   #   hide(id = "hmat")
                   #   # print(F)
                   # }
                   # else{
                     output$table <-
                       renderDataTable(data(), options = list(pageLength = 5))
                     
                     show(id = "radio_data_type")
                     show(id = "selecting_columns_title")
                     show(id = "btn_confirm")
                     show(id = "hmat")
                     
                     col_list <- reactive({
                       as.list(colnames(data()))
                     })
                     
                     # long binary ----
                     if (input$radio_data_type == "long_binary") {
                       show(id = "choose_column_long_binary")
                       hide(id = "choose_column_long_continuous")
                       
                       updateSelectInput(
                         session,
                         inputId = "box_treat",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )
                       
                       updateSelectInput(
                         session,
                         inputId = "box_event",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )

                       updateSelectInput(
                         session,
                         inputId = "box_n",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )

                       updateSelectInput(
                         session,
                         inputId = "box_studlab",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )
                       
                       updateSelectInput(
                         session,
                         inputId = "box_rob",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )
                     }
                     # long continuous ----
                     if (input$radio_data_type == "long_continuous") {
                       show(id = "choose_column_long_continuous")
                       hide(id = "choose_column_long_binary")

                       updateSelectInput(
                         session,
                         inputId = "box_treat_c",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )
                       
                       updateSelectInput(
                         session,
                         inputId = "box_mean_c",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )
                       
                       updateSelectInput(
                         session,
                         inputId = "box_sd_c",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )
                       
                       updateSelectInput(
                         session,
                         inputId = "box_n_c",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )

                       updateSelectInput(
                         session,
                         inputId = "box_studlab_c",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )

                       updateSelectInput(
                         session,
                         inputId = "box_rob_c",
                         label = NULL,
                         choices = col_list(),
                         selected = NULL
                       )
                     }
                 })
                 
                 data <- reactive({
                   df <- read.csv(
                     input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote
                   )
                   df
                 })
                 
                 newdata <- eventReactive(input$btn_confirm, {

                   if (input$radio_data_type == "long_binary") {
                     newdata <- data() %>% rename_("t" = input$box_treat,
                                                  "r" = input$box_event,
                                                  "n" = input$box_n,
                                                  "study" = input$box_studlab,
                                                  "rob" = input$box_rob
                                                  )
                   }
                   if (input$radio_data_type == "long_continuous") {
                     newdata <- data() %>% rename_("t" = input$box_treat_c,
                                                  "y" = input$box_mean_c,
                                                  "sd" = input$box_sd_c,
                                                  "n" = input$box_n_c,
                                                  "study" = input$box_studlab_c,
                                                  "rob" = input$box_rob
                                                  )
                   }
                   # print(newdata)
                   return(newdata)
                 })
                 
                 hatmatrix <- eventReactive(input$btn_confirm, {
                   getHatMatrix(newdata(), input$radio_data_type, sm = input$box_sm)
                 })

                 return(list(indata = newdata,
                            hatmatrix = hatmatrix,
                            btn_confirm = reactive(input$btn_confirm)
                          )
                        )
                 
               })
}


# ui <- fluidPage(
#   uploadfileUI("uploadfile")
# )
#
# server <- function(input, output, session) {
#   uploadfileServer("uploadfile")
# }
#
#
# # Run the app ----
#
# shinyApp(ui = ui, server = server)