source("./flow_contribution/R/streamstatistics2.R")
source("./flow_contribution/R/hatmatrix.R")
source("./flow_contribution/R/contributionrow.R")
source("./flow_contribution/R/studycontribution.R")

source("./getflow.R")
source("./getflowplot.R")
source("./robustness.R")
source("./robustnetplot.R")
source("./contrast_robustnetplot.R")

source("./mod_uploadfile.R")
source("./mod_streamrob.R")
source("./mod_streamrobustness.R")
source("./mod_networkrobustness.R")

library(netmeta)
library(shinyjs)
library(shinybusy)
library(plyr); library(dplyr)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  add_busy_bar(color = "#5599FF"),
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
    # tab 1 to tab 2 ----
    if(uploadfile_vals$btn_confirm() > 0){
      
      indata <- uploadfile_vals$indata()
      hatmatrix <- uploadfile_vals$hatmatrix()
      sm <- uploadfile_vals$sm()
      
      networkrobustbess_vals <- networkrobustbessServer("networkrobustbess",
                                indata = indata,
                                hatmatrix = hatmatrix,
                                sm = sm
                                )
      
      # show(id="div.networkrobustbess")
      
      # Go to tab 2
      updateTabsetPanel(session, "tabs",
                        selected = "NMA Robustness")
      
      # append tab 2 ----
      if(uploadfile_vals$btn_confirm() == 1){
        
        appendTab(inputId = "tabs",
                  tabPanel("NMA Robustness", fluid = TRUE,
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
      
      
      # create tab 3 dropdown ----

      observeEvent(networkrobustbess_vals$btn_contrasts(), {
        edges <- networkrobustbess_vals$edge.selected()
        
        if (length(edges) > 0){
          removeTab(inputId = "tabs", target = "Contrast Robustness")
          
          appendTab(inputId = "tabs",
                    navbarMenu("Contrast Robustness")
          )
          hideTab("tabs", "Contrast Robustness")
          
          # print(networkrobustbess_vals$edge.selected())
          
          
          i = 1
          for(e in edges){
            streamrobustbessServer(
              # Don't use ":" in server id! It took me 3 hours to debug. ;-;
              id=paste0("robust_server_", i),
              indata=indata,
              hatmatrix=hatmatrix,
              comparison=e
            )

            streamrobServer(
              # Don't use ":" in server id! It took me 3 hours to debug. ;-;
              id=paste0("rob_server_", i),
              indata=indata,
              hatmatrix=hatmatrix,
              comparison=e
            )
            
            appendTab(inputId = "tabs",
                      tabPanel(title = e, 
                               streamrobustbessUI(paste0("robust_server_", i)), 
                               titlePanel("Stream Risk of Bias Bar Plot"),
                               streamrobUI(paste0("rob_server_", i))
                      ),
                      menuName = "Contrast Robustness"
                      # navbarMenu(id = "Contrast Robustness",
                      #            tabPanel(edges, streamrobustbessUI(edges[1]))
                      # )
            )
            i = i + 1
          }
          
          showTab("tabs", "Contrast Robustness")
          
          # Go to tab 3
          updateTabsetPanel(session, "tabs",
                            selected = edges[1])
        }
        
      })
    }

  })
}

shinyApp(ui = ui, server = server)