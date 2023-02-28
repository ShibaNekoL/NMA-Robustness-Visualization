source("./flow_contribution/R/streamstatistics2.R")
source("./flow_contribution/R/hatmatrix.R")

source("./getflow.R")
source("./getflowplot.R")
source("./robustness.R")
source("./robustnetplot.R")
source("./contrast_robustnetplot.R")

library(netmeta)
library(shinyjs)

source("mod_uploadfile.R")
source("mod_streamrob.R")
source("mod_streamrobustness.R")
source("mod_networkrobustness.R")


ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  tabsetPanel(id = "tabs",
    tabPanel("Upload file", fluid = TRUE, 
      uploadfileUI("uploadfile")
    )
  )
)

server <- function(input, output, session, uploadfile, networkrobustbess) {
  
  uploadfile_vals <- uploadfileServer("uploadfile")
  # output$table <- renderDataTable(uploadfile_vals$indata(), options = list(pageLength = 5))
  
  observe({
    if(uploadfile_vals$btn_confirm() > 0){
      
      indata <- uploadfile_vals$indata()
      hatmatrix <- uploadfile_vals$hatmatrix()
      
      networkrobustbessServer("networkrobustbess",
                              indata = indata,
                              hatmatrix = hatmatrix
      )
      # show(id="div.networkrobustbess")
      
      updateTabsetPanel(session, "tabs",
                        selected = "Robustness of NMA")
      
      if(uploadfile_vals$btn_confirm() == 1){
        
        appendTab(inputId = "tabs",
                  tabPanel("Robustness of NMA", fluid = TRUE,
                           # hidden(
                           # Since hide and show in shinyjs can only control html element, instead of a module.
                           # I wrap the module with a div tag.
                           # I don't know why hidden also works on the module though.
                           div(
                             id="div.networkrobustbess", networkrobustbessUI("networkrobustbess")
                           )
                  )
        )
      }
    }
  })
}

shinyApp(ui = ui, server = server)