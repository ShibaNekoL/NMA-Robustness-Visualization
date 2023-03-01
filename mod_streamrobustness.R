library(shiny)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(dplyr)
library(DT)
library(visNetwork)
library(tidygraph)

streamrobustbessUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  tagList(
    # reset button for stream robustness plot
    shinyjs::useShinyjs(),
    actionButton(ns("resetplot"), "Reset"),

    # stream robustness plot
    visNetworkOutput(ns("visStreamRobustness")),

    # switch to highrisk
    switchInput(ns("highriskswitcher"), "Highrisk", onLabel = "ON", offLabel = "OFF"),

    # node size selector
    selectInput(ns("nodesize"), "Node size by", choices=c("Sample size", "Number of studies", "Equal size"), selected="Sample size"),

    # edge width selector
    selectInput(ns("edgewidth"), "Edge width by", choices=c("Stream contribution", "Equal width"), selected="Stream Contribution"),

    # slider filter robustness
    sliderInput(ns("streamfilterrobustness"), "Fliter streams by robustness: ",
                min=0,
                max=1,
                value=c(0,1)),
    # slider filter contribution
    sliderInput(ns("streamfiltercontribution"), "Fliter streams by contribution: ",
                min=0,
                max=1,
                value=c(0,1)),
    
    # data table for streams
    # data table for contrast
    
    splitLayout(
        splitLayout(
            actionButton(ns("deselect_vis"), "Deselect"),
            actionButton(ns("deselect_vis_all"), "Deselect all"),
        ),
        splitLayout(
            actionButton(ns("select_vis"), "Select"),
            actionButton(ns("select_vis_all"), "Select all")
        )
    ),
    splitLayout(
        dataTableOutput(ns("visselected")),
        dataTableOutput(ns("visunselected"))
    )
  )
}

streamrobustbessServer <- function(id, indata, hatmatrix, comparison){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {
      ns <- session$ns
      
      # print(indata)
      plot <- get.interactive.robustnetworkplot(indata, hatmatrix, comparison, highrisk=F, nodesizeby=1, edgesizeby = 1)
      data <- plot$x
      nodes <- data$nodes
      edges <- data$edges

      # Update Edges function --------------------------------------------------
      updatanewargument <- function(highrisk=F, edgesizeby=0, hover.edge=NULL, select.edge=NULL, robustslider=NULL, contrislider=NULL, tabledeselectededge=NULL, tableselectededge=NULL){
          # highrisk switcher
          if(highrisk==T){
              edges$color <- edges$color_T
          }else if(highrisk==F){
              edges$color <- edges$color_F
          }

          # width select
          if(edgesizeby==0){
              edges$width <- edges$width_0
          }else if(edgesizeby==1){
              edges$width <- edges$width_1
          }
          # sliders filter
          if(length(robustslider) > 0){
              edges[which(edges$id %in% robustslider), ]$hidden <- T
          }
          if(length(contrislider) > 0){
              edges[which(edges$id %in% contrislider), ]$hidden <- T
          }

          # select edges
          if(length(select.edge)>0){
              if(highrisk==T){
                  edges$color <- ifelse(edges$id %in% select.edge, edges$color, "rgba(200,200,200,0)")
              }else{
                  edges$color <- ifelse(edges$id %in% select.edge, edges$color, "rgba(200,200,200,0.5)")
              }
          }
          # hover edge
          if(length(hover.edge)>0){
              edges[ which( edges$id %in% hover.edge ), ]$width <- edges[ which( edges$id %in% hover.edge ), ]$width + 3
              if(all(hover.edge %in% select.edge) == F){
                  if(highrisk==T){
                      edges$color <- edges$color_T
                  } else{
                      edges$color <- edges$color_F
                  }
              }
          }

          # table deselect edge
          if(length(tabledeselectededge) > 0){
              edges[ which( edges$path_id %in% tabledeselectededge ), ]$color <- "red"
              # edges[ which( edges$path_id %in% tabledeselectededge ), ]$shadow <- T
          }

          # table select edge
          if(length(tableselectededge) > 0){
              edges[ which( edges$path_id %in% tableselectededge ), ]$color <- "green"
          }

          visNetworkProxy(ns("visStreamRobustness")) %>% visUpdateEdges(edges)
      }


      # robustness filter slider --------------------------------------------------
      filterrobust <- reactiveValues(value=NULL)

      # fire to make other streams invisable
      observeEvent(input$streamfilterrobustness, {

          filterrobust$value <- edges[which((edges$path_p > input$streamfilterrobustness[2]) | (edges$path_p < input$streamfilterrobustness[1])), ]$id
          updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value)

      })

      # contribution filter slider ------------------------------------------------
      filtercontri <- reactiveValues(value=NULL)

      # fire to make other streams invisable
      observeEvent(input$streamfiltercontribution, {

          filtercontri$value <- edges[which((edges$flow > input$streamfiltercontribution[2]) | (edges$flow < input$streamfiltercontribution[1])), ]$id
          updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)

      })


      # render get.interactive.robustnetworkplot ---------------------------------------

      render_get.interactive.robustnetworkplot <- function(){ return(
          renderVisNetwork({
              plot %>%
                  visInteraction(hover = TRUE, tooltipStay=0) %>%  #, tooltipStyle="position: static;left: 10px; top: 10px"
                  visEdges(hoverWidth=0, shadow=list(x=0,y=0,size=15)) %>%
                  # click an edge
                  visEvents(
                      click = sprintf('function(properties) {
                          Shiny.onInputChange("%s", properties.edges);
                            ;}', ns("current_edges_selection")),
                      hoverEdge = sprintf('function(edges) {
                          Shiny.onInputChange("%s", edges.edge);
                            ;}', ns("current_edge_hover")),
                      blurEdge = sprintf('function(edges) {
                          Shiny.onInputChange("%s", 0);
                            ;}', ns("current_edge_hover"))
                  )
          })
      )}

      output$visStreamRobustness <- render_get.interactive.robustnetworkplot()

      # node size selector -------------------------------------------------------
      # node size by: Sample size, Number of studies, Equal size
      nodesize_reactV <- reactiveValues(value=1)

      # store reactivevalues
      observeEvent(input$nodesize, {
          if(input$nodesize == "Sample size"){
              nodesize_reactV$value <- 1
          }else if (input$nodesize == "Number of studies"){
              nodesize_reactV$value <- 2
          }else if (input$nodesize == "Equal size"){
              nodesize_reactV$value <- 0
          }
          updatenodes(size=nodesize_reactV$value)
      })

      # reacting function
      updatenodes <- function(size){
          if(size == 1){
              nodes$size <- data$nodes$size_1
          }else if(size == 2){
              nodes$size <- data$nodes$size_2
          }else if(size == 0){
              nodes$size <- data$nodes$size_0
          }
          visNetworkProxy(ns("visStreamRobustness")) %>% visUpdateNodes(nodes)
      }

      # edge size selector ----------------------------------------------------

      # initial width = Stream contribution
      edgewidth_reactV <- reactiveValues(value=0)

      # use updatanewargument function to change edge color since we need to load another data
      observeEvent(input$edgewidth, {
          if(input$edgewidth == "Equal width"){
              edgewidth_reactV$value <- 0
          }else if (input$edgewidth == "Stream contribution"){
              edgewidth_reactV$value <- 1
          }
          updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)
      })

      # highrisk display switcher ----------------------------------------------
      # initial highrisk_reactV = F
      highrisk_reactV <- reactiveValues(value=F)

      # use updatanewargument function to change edge color since we need to load another data
      observeEvent(input$highriskswitcher, {
          if(input$highriskswitcher == "TRUE"){
              highrisk_reactV$value <- T
          }
          else{
              highrisk_reactV$value <- F
          }
          updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)

          #, sliderfilter=filterrobust$value
          # , tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value
      })

      # edge hover to highlight a path --------------------------------------------
      myEdgehover <- reactiveValues(hovered = NULL)

      observeEvent(input$current_edge_hover, {

          # when mouse cursor move away edge, highlight invisabled
          if(input$current_edge_hover != 0){

              # store click input into myEdge$hovered
              # get path id for the edge clicked and fliter the edges from the same path
              path_id <- edges[which(edges$id %in% input$current_edge_hover),]$path_id
              myEdgehover$hovered <- edges[edges$path_id %in% path_id, , drop = FALSE]$id

              updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)

              # show correct title
              hover_edge <- edges[which(edges$id == input$current_edge_hover),]
              html(selector  = "#graphvisNetworkRobustness > div:nth-child(2)", html = hover_edge$title)

          } else{
              # 注意，reactivevalue一定要用<<-重置
              myEdgehover <<- reactiveValues(hovered = NULL)
              updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)

          }
      })

      # select edges ------------------------------------------------------

      myEdge <- reactiveValues(selected = NULL)
      filteredEdges <- reactiveValues(selected = NULL)
      path_id <- reactiveValues(selected = NULL)

      # do when click on edges
      observeEvent(input$current_edges_selection, ignoreNULL = FALSE, {
          if(!is.null(input$current_edges_selection)){
              # exclude edges that has been selected and filtered by sliders
              # But if click on nodes, it will return a vector, so we have to use unique
              if (all(input$current_edges_selection %in% filteredEdges$selected) == F) {

                  # store click input into myEdge$selected (vector)
                  myEdge$selected <- unique(c(myEdge$selected, input$current_edges_selection))

                  # get path id for the edge clicked and fliter the edges from the same path
                  path_id$selected <- unique(edges[which(edges$id %in% myEdge$selected),]$path_id)

                  filteredEdges$selected <- edges[(edges$path_id %in% path_id$selected) , , drop = FALSE]$id

                  updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)
                  #highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, sliderfilter=filterrobust$value,
                  # , tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value
              }
          }
      })

      # output data table for contrasts -----------------------------------------
      ## functions --------
      dt_visselect <- function(select, robusthidden, contrihidden){
          temp <- edges[ which( ( edges$path_id %in% select ) & (edges$id %in% robusthidden == F) & (edges$id %in% contrihidden == F)), ]

          out <- unique(data.frame(Stream=temp$path,
                            Robustness=round(temp$path_p, 3),
                            Contribution=round(temp$flow, 3),
                            Ratio=round(temp$path_p/temp$flow, 3),
                            Length=temp$length))

          row.names(out) <- unique(temp$path_id)
          # row.names(out) <- NULL
          return(out)
      }

      dt_visunselect <- function(select, robusthidden, contrihidden){
          temp <- edges[ which( ( edges$path_id %in% select == F) & (edges$id %in% robusthidden == F) & (edges$id %in% contrihidden == F)), ]
          out <- unique(data.frame(Stream=temp$path,
                            Robustness=round(temp$path_p, 3),
                            Contribution=round(temp$flow, 3),
                            Ratio=round(temp$path_p/temp$flow, 3),
                            Length=temp$length))
          row.names(out) <- unique(temp$path_id)

          # row.names(out) <- NULL
          return(out)
      }

      # dt_invisselect <- function(select, hidden){
      #     temp <- edges[ which( ( edges$id %in% select == T) & (edges$id %in% hidden == T) ), ]
      #     out <- data.frame(Stream=temp$path,
      #                       Robustness=round(temp$path_p, 3),
      #                       Contribution=round(temp$flow, 3),
      #                       Ratio=round(temp$path_p/temp$flow, 3),
      #                       Length=temp$length)
      #     row.names(out) <- temp$id
      #
      #     # row.names(out) <- NULL
      #     return(out)
      # }

      # dt_invisunselect <- function(select, hidden){
      #     temp <- edges[ which( ( edges$id %in% select == F) & (edges$id %in% hidden == T) ), ]
      #     out <- data.frame(Stream=temp$path,
      #                       Robustness=round(temp$path_p, 3),
      #                       Contribution=round(temp$flow, 3),
      #                       Ratio=round(temp$path_p/temp$flow, 3),
      #                       Length=temp$length)
      #     row.names(out) <- temp$id
      #
      #     # row.names(out) <- NULL
      #     return(out)
      # }

      # react
      df1 <- reactive({ dt_visselect(path_id$selected, filterrobust$value, filtercontri$value) })
      # df2 <- reactive({ dt_invisselect(filteredEdges$selected, filterrobust$value) })
      df3 <- reactive({ dt_visunselect(path_id$selected, filterrobust$value, filtercontri$value) })
      # df4 <- reactive({ dt_invisunselect(filteredEdges$selected, filterrobust$value) })


      output$visselected <- renderDataTable({
          # datatable(dt_visselect(myEdge$selected, filterrobust$value), caption="Selected contrasts")
          datatable(df1())
      })
      # output$invisselected <- renderDataTable({
      #     # datatable(dt_invisselect(myEdge$selected, filterrobust$value), caption="Selected contrasts")
      #     datatable(df2())
      # })
      output$visunselected <- renderDataTable({
          # datatable(dt_visunselect(myEdge$selected, filterrobust$value), caption="Unselected contrasts")
          datatable(df3())
      })
      # output$invisunselected <- renderDataTable({
      #     # datatable(dt_invisunselect(myEdge$selected, filterrobust$value), caption="Unselected contrasts")
      #     datatable(df4())
      # })


      # left datatable action --------------------------------------------------------
      # press action button and deselect
      observeEvent(input$deselect_vis,{

          if (!is.null(input$visselected_rows_selected)) {
              pathid <- unique(edges[which(edges$id %in% filterrobust$value == F & edges$id %in% filtercontri$value == F & edges$path_id %in% path_id$selected), ]$path_id)
              rmrows <- as.vector(input$visselected_rows_selected)

              path_id$selected <<- sort(pathid)[-rmrows]
              myEdge$selected <<- edges[which(edges$path_id %in% path_id$selected), ]$id
              filteredEdges$selected <<- myEdge$selected

              updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)
          }

      })

      # highlihgt the deselected edges on the plot
      tabledeselect <- reactiveValues(value=NULL)

      observeEvent(input$visselected_rows_selected,ignoreNULL = FALSE, {
          pathid <- unique(edges[which(edges$id %in% filterrobust$value == F & edges$id %in% filtercontri$value == F & edges$path_id %in% path_id$selected), ]$path_id)
          tabledeselect$value <- sort(pathid)[as.vector(input$visselected_rows_selected)]
          updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)

      })

      # deselect all
      observeEvent(input$deselect_vis_all, {

          path_id$selected <<- NULL
          myEdge$selected <<- NULL
          filteredEdges$selected <<- myEdge$selected

          updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)
      })

      # right datatable action ----------------------------------------------------
      # press action button and deselect
      observeEvent(input$select_vis,{

          if (!is.null(input$visunselected_rows_selected)) {
              pathid <- unique(edges[which(edges$id %in% filterrobust$value == F & edges$id %in% filtercontri$value == F & edges$path_id %in% path_id$selected == F), ]$path_id)
              addrows <- as.vector(input$visunselected_rows_selected)

              path_id$selected <<- c(path_id$selected, sort(pathid)[addrows])
              myEdge$selected <<- edges[which(edges$path_id %in% path_id$selected), ]$id
              filteredEdges$selected <<- myEdge$selected

              updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)

          }

      })
      # highlihgt the deselected edges on the plot
      tableselect <- reactiveValues(value=NULL)

      observeEvent(input$visunselected_rows_selected,ignoreNULL = FALSE, {

          pathid <- unique(edges[which(edges$id %in% filterrobust$value == F & edges$id %in% filtercontri$value == F & edges$path_id %in% path_id$selected == F), ]$path_id)
          tableselect$value <- sort(pathid)[as.vector(input$visunselected_rows_selected)]
          updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)

      })

      # select all
      observeEvent(input$select_vis_all, {
          myEdge$selected <<- edges$id
          filteredEdges$selected <<- myEdge$selected
          path_id$selected <<- unique(edges$path_id)
          updatanewargument(highrisk=highrisk_reactV$value, edgesizeby=edgewidth_reactV$value, hover.edge=myEdgehover$hovered, select.edge = filteredEdges$selected, robustslider=filterrobust$value, contrislider=filtercontri$value, tabledeselectededge = tabledeselect$value, tableselectededge=tableselect$value)
      })



      # reset plot when pressing the reset button ---------------------------------
      observeEvent(input$resetplot, {
          shinyjs::reset("highriskofvisStreamRobustness")
          shinyjs::reset("nodesize")
          shinyjs::reset("edgewidth")
          shinyjs::reset("streamfilterrobustness")
          shinyjs::reset("streamfiltercontribution")

          # 如果這邊highrisk=input$switcher的話會直接聯動，變成每開關一次都重畫一次圖
          output$visStreamRobustness <- render_get.interactive.robustnetworkplot()

          # reset input reactive values
          nodesize_reactV <<- reactiveValues(value=1)
          edgewidth_reactV <<- reactiveValues(value=1)
          highrisk_reactV <<- reactiveValues(value=F)
          filterrobust <<- reactiveValues(value=NULL)
          filtercontri <<- reactiveValues(value=NULL)

          # reset filter
          filteredEdges <<- reactiveValues(selected = NULL)
          # reset click input
          myEdge <<- reactiveValues(selected = NULL)
          # reset path
          path_id <<- reactiveValues(selected = NULL)


          # reset data table
          output$visselected <- renderDataTable({
              datatable(dt_visselect(path_id$selected, filterrobust$value, filtercontri$value), caption="Selected Streams")
          })
          output$visunselected <- renderDataTable({
              datatable(dt_visunselect(path_id$selected, filterrobust$value, filtercontri$value), caption="Unselected Streams")
          })
      })
    }
  )
}

# ui <- fluidPage(
#   streamrobustbessUI("streamrobustbess")
# )
# 
# server <- function(input, output, session) {
#   streamrobustbessServer("streamrobustbess", indata, hatmatrix, comparison)
# }
# 
# shinyApp(ui = ui, server = server)