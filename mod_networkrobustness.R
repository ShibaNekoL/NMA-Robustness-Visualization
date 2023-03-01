library(shiny)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(dplyr)
library(DT)
library(visNetwork)
library(tidygraph)


networkrobustbessUI <- function(id) {
  # `NS(id)` returns a namespace function, which was save as `ns` and will
  # invoke later.
  ns <- NS(id)
  
  tagList(
    # reset button for network robustness plot
    shinyjs::useShinyjs(),

    
    # App title ----
    titlePanel("Robustness of NMA Estimate"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
    
        # dataTableOutput(ns("table")),
        actionButton(ns("resetplot"), "Reset"),
        tags$hr(),
        
        # switch to highrisk
        switchInput(
          ns("highriskswitcher"),
          "Highrisk",
          onLabel = "ON",
          offLabel = "OFF"
        ),
        # node size selector
        selectInput(
          ns("nodesize"),
          "Node size by",
          choices = c("Sample size", "Number of studies", "Equal size"),
          selected = "Sample size"
        ),
        # edge width selector
        selectInput(
          ns("edgewidth"),
          "Edge width by",
          choices = c(
            "Robustness",
            "Robustness (inverse)",
            "Sample size",
            "Number of studies",
            "Equal size"
          ),
          selected = "Robustness"
        ),
        # slider filter robustness
        sliderInput(
          ns("filterrobustness"),
          "Fliter contrasts by robustness: ",
          min = 0,
          max = 1,
          value = c(0, 1)
        )
      )
      ,
      # Main panel for displaying outputs ----
      mainPanel(
        # stream robustness plot
        visNetworkOutput(ns("visNetworkRobustness"))
        ),
    ),
    # data table for contrast
    
    splitLayout(
      splitLayout(
        actionButton(ns("deselect_vis"), "Deselect"),
        actionButton(ns("deselect_vis_all"), "Deselect all"),
      ),
      splitLayout(actionButton(ns("select_vis"), "Select"),
                  actionButton(ns("select_vis_all"), "Select all"))
    ),
    splitLayout(dataTableOutput(ns("visselected")),
                dataTableOutput(ns("visunselected"))),
    # splitLayout(
    #     dataTableOutput("invisselected"),
    #     dataTableOutput("invisunselected")
    # )
    actionButton(ns("btn_contrasts"), "Analyze selected contrasts"),
    
    tags$hr(),

    titlePanel("Descriptive Analysis"),
    
    # Input: Slider for the number of bins ----
    sliderInput(
      inputId = ns("bins"),
      label = "Bins:",
      min = 2,
      max = 50,
      value = 5
    ),
    
    plotOutput(outputId = ns("distPlot"))
  )
}


networkrobustbessServer <- function(id, indata, hatmatrix, sm) {
  moduleServer(id,
               ## Below is the module function
               function(input, output, session) {
                 ns <- session$ns
                 
                 # output$table <- renderDataTable(indata, options = list(pageLength = 5))
                 # print(indata)
                 
                 # get data
                 plot <-
                   get.interactive.contrast_robust_plot(
                     indata,
                     hatmatrix,
                     sm,
                     highrisk = F,
                     nodesizeby = 1,
                     edgesizeby = 0
                   )
                 data <- plot$x
                 nodes <- data$nodes
                 edges <- data$edges

                 # render get.contrast_robust_plot -------------------------------------------

                 render_get.interactive.contrast_robust_plot <- function() {
                   return(renderVisNetwork({
                     plot %>%
                       visInteraction(hover = T, tooltipStay = 0) %>%  #, tooltipStyle="position: static;left: 10px; top: 10px"
                       visEdges(hoverWidth = 0,
                                shadow = list(
                                  x = 0,
                                  y = 0,
                                  size = 15
                                )) %>%
                       # click an edge
                       visEvents(
                         click = sprintf(
                           'function(properties) {
                              Shiny.onInputChange("%s", properties.edges);
                                ;}',
                           ns("current_edges_selection")
                         ),
                         hoverEdge = sprintf(
                           'function(edges) {
                              Shiny.onInputChange("%s", edges.edge);
                              ;}',
                           ns("current_edge_hover")
                         ),
                         blurEdge = sprintf(
                           'function(edges) {
                              Shiny.onInputChange("%s", 0);
                              ;}',
                           ns("current_edge_hover")
                         ),
                         doubleClick = sprintf(
                           'function(properties) {
                              Shiny.onInputChange("%s", properties.edges);
                              ;}',
                           ns("undoedgeid")
                         )
                       )
                   }))
                 }

                 output$visNetworkRobustness <-
                   render_get.interactive.contrast_robust_plot()

                 # node size selector -------------------------------------------------------
                 # node size by: Sample size, Number of studies, Equal size
                 nodesize_reactV <- reactiveValues(value = 1)

                 # store reactivevalues
                 observeEvent(input$nodesize, {
                   if (input$nodesize == "Sample size") {
                     nodesize_reactV$value <- 1
                   } else if (input$nodesize == "Number of studies") {
                     nodesize_reactV$value <- 2
                   } else if (input$nodesize == "Equal size") {
                     nodesize_reactV$value <- 0
                   }
                   updatenodes(size = nodesize_reactV$value)
                 })

                 # reacting function
                 updatenodes <- function(size) {
                   if (size == 1) {
                     nodes$size <- data$nodes$size_1
                   } else if (size == 2) {
                     nodes$size <- data$nodes$size_2
                   } else if (size == 0) {
                     nodes$size <- data$nodes$size_0
                   }
                   visNetworkProxy(ns("visNetworkRobustness")) %>% visUpdateNodes(nodes)
                 }


                 # Update Edges function --------------------------------------------------

                 updatanewargument <-
                   function(highrisk = F,
                            edgesizeby = 0,
                            sliderfilter = NULL,
                            hover.edge = NULL,
                            select.edge = NULL,
                            tabledeselectededge = NULL,
                            tableselectededge = NULL) {
                     # highrisk switcher
                     if (highrisk == T) {
                       edges$color <- edges$color_T
                     } else if (highrisk == F) {
                       edges$color <- edges$color_F
                     }

                     # width select
                     if (edgesizeby == 0) {
                       edges$width <- edges$width_0
                     } else if (edgesizeby == 1) {
                       edges$width <- edges$width_1
                     } else if (edgesizeby == 2) {
                       edges$width <- edges$width_2
                     } else if (edgesizeby == 3) {
                       edges$width <- edges$width_3
                     } else if (edgesizeby == 4) {
                       edges$width <- edges$width_4
                     }

                     # sliders filter
                     if (length(sliderfilter) > 0) {
                       edges[which(edges$id %in% sliderfilter),]$hidden <- T
                     }

                     # select edges
                     if (length(select.edge) > 0) {
                       if (highrisk == T) {
                         edges$color <-
                           ifelse(edges$id %in% select.edge,
                                  edges$color,
                                  "rgba(200,200,200,0)")
                       } else{
                         edges$color <-
                           ifelse(edges$id %in% select.edge,
                                  edges$color,
                                  "rgba(200,200,200,0.5)")
                       }
                     }

                     # hover edge
                     if (length(hover.edge) > 0) {
                       edges[which(edges$id == hover.edge),]$width <-
                         edges[which(edges$id == hover.edge),]$width + 3
                       if (all(hover.edge %in% select.edge) == F) {
                         if (highrisk == T) {
                           edges$color <- edges$color_T
                         } else{
                           edges$color <- edges$color_F
                         }
                       }
                     }

                     # table deselect edge
                     if (length(tabledeselectededge) > 0) {
                       edges[which(edges$id %in% tabledeselectededge),]$shadow <- T
                     }

                     # table select edge
                     if (length(tableselectededge) > 0) {
                       edges[which(edges$id %in% tableselectededge),]$color <- "#add8e6"
                     }
                     visNetworkProxy(ns("visNetworkRobustness")) %>% visUpdateEdges(edges)
                   }

                 # edge size selector ----------------------------------------------------

                 # edge size by: Robustness(0), Sample size(1), Number of studies(2), Equal size(3)
                 # initial width = Stream contribution
                 edgewidth_reactV <- reactiveValues(value = 0)

                 # use updatanewargument function to change edge color since we need to load another data
                 observeEvent(input$edgewidth, {
                   if (input$edgewidth == "Robustness") {
                     edgewidth_reactV$value <- 0
                   } else if (input$edgewidth == "Sample size") {
                     edgewidth_reactV$value <- 1
                   } else if (input$edgewidth == "Number of studies") {
                     edgewidth_reactV$value <- 2
                   } else if (input$edgewidth == "Equal size") {
                     edgewidth_reactV$value <- 3
                   } else if (input$edgewidth == "Robustness (inverse)") {
                     edgewidth_reactV$value <- 4
                   }
                   updatanewargument(
                     highrisk = highrisk_reactV$value,
                     edgesizeby = edgewidth_reactV$value,
                     sliderfilter = filterrobust$value,
                     hover.edge = myEdgehover$hovered,
                     select.edge = myEdge$selected,
                     tabledeselectededge = tabledeselect$value,
                     tableselectededge = tableselect$value
                   )
                 })

                 # highrisk display switcher ----------------------------------------------
                 # initial highrisk_reactV = F
                 highrisk_reactV <- reactiveValues(value = F)

                 # use updatanewargument function to change edge color since we need to load another data
                 observeEvent(input$highriskswitcher, {
                   if (input$highriskswitcher == "TRUE") {
                     highrisk_reactV$value <- T
                   }
                   else{
                     highrisk_reactV$value <- F
                   }
                   updatanewargument(
                     highrisk = highrisk_reactV$value,
                     edgesizeby = edgewidth_reactV$value,
                     sliderfilter = filterrobust$value,
                     hover.edge = myEdgehover$hovered,
                     select.edge = myEdge$selected,
                     tabledeselectededge = tabledeselect$value,
                     tableselectededge = tableselect$value
                   )
                 })


                 # robustness filter slider --------------------------------------------------
                 filterrobust <- reactiveValues(value = c(0, 1))

                 # fire to make other contrasts invisable
                 observeEvent(input$filterrobustness, {
                   # reset click input
                   # myEdge <<- reactiveValues(selected = NULL)

                   # # reset data table to show whole table
                   # output$table <<- renderDataTable({
                   # })
                   # output$resttable <<- renderDataTable({
                   #     datatable(setdiff(wdt(), dt()), caption="Unselected streams")
                   # })

                   filterrobust$value <-
                     which((edges$contrast_robust > input$filterrobustness[2]) |
                             (edges$contrast_robust < input$filterrobustness[1])
                     )

                   updatanewargument(
                     highrisk = highrisk_reactV$value,
                     edgesizeby = edgewidth_reactV$value,
                     sliderfilter = filterrobust$value,
                     hover.edge = myEdgehover$hovered,
                     select.edge = myEdge$selected,
                     tabledeselectededge = tabledeselect$value,
                     tableselectededge = tableselect$value
                   )

                 })


                 # hover width increase ----------------------------------------------------

                 myEdgehover <- reactiveValues(hovered = NULL)

                 observeEvent(input$current_edge_hover, {
                   # when mouse cursor move away edge, highlight invisabled
                   if (input$current_edge_hover != 0) {
                     # store hover input into myEdge$hovered
                     myEdgehover$hovered <- input$current_edge_hover

                     hover_edge <-
                       edges[which(edges$id == myEdgehover$hovered),]

                     updatanewargument(
                       highrisk = highrisk_reactV$value,
                       edgesizeby = edgewidth_reactV$value,
                       sliderfilter = filterrobust$value,
                       hover.edge = myEdgehover$hovered,
                       select.edge = myEdge$selected,
                       tabledeselectededge = tabledeselect$value,
                       tableselectededge = tableselect$value
                     )

                     html(selector  = "#graphvisNetworkRobustness > div:nth-child(2)", html = hover_edge$title)

                   } else{
                     # 注意，reactivevalue一定要用<<-重置
                     myEdgehover <<- reactiveValues(hovered = NULL)
                     updatanewargument(
                       highrisk = highrisk_reactV$value,
                       edgesizeby = edgewidth_reactV$value,
                       sliderfilter = filterrobust$value,
                       hover.edge = myEdgehover$hovered,
                       select.edge = myEdge$selected,
                       tabledeselectededge = tabledeselect$value,
                       tableselectededge = tableselect$value
                     )

                   }
                 })

                 # select edges ------------------------------------------------------

                 myEdge <- reactiveValues(selected = NULL)

                 # do when click on edges
                 observeEvent(input$current_edges_selection, ignoreNULL = FALSE, {
                   if (!is.null(input$current_edges_selection)) {
                     # exclude edges that has been selected and filtered by sliders
                     # But if click on nodes, it will return a vector, so we have to use unique
                     if (all(input$current_edges_selection %in% myEdge$selected) == F) {
                       # store click input into myEdge$selected (vector)
                       myEdge$selected <-
                         unique(c(myEdge$selected, input$current_edges_selection))

                       updatanewargument(
                         highrisk = highrisk_reactV$value,
                         edgesizeby = edgewidth_reactV$value,
                         sliderfilter = filterrobust$value,
                         hover.edge = myEdgehover$hovered,
                         select.edge = myEdge$selected,
                         tabledeselectededge = tabledeselect$value,
                         tableselectededge = tableselect$value
                       )
                     }
                   }
                 })


                 # output data table for contrasts -----------------------------------------
                 ## functions --------
                 dt_visselect <- function(select, hidden) {
                   temp <-
                     edges[which((edges$id %in% select == T) &
                                   (edges$id %in% hidden == F)),]

                   out <- data.frame(
                     Contrast = temp$contrast,
                     Robustness = round(temp$contrast_robust, 3),
                     Mean_stream_length = round(temp$meanStreamLen, 3),
                     Number_of_streams = temp$NumberStream,
                     Sample_size = temp$n,
                     Number_of_Studies = temp$trials
                   )
                   row.names(out) <- temp$id
                   # row.names(out) <- NULL

                   return(out)
                 }

                 dt_visunselect <- function(select, hidden) {
                   temp <-
                     edges[which((edges$id %in% select == F) &
                                   (edges$id %in% hidden == F)),]
                   out <- data.frame(
                     Contrast = temp$contrast,
                     Robustness = round(temp$contrast_robust, 3),
                     Mean_stream_length = round(temp$meanStreamLen, 3),
                     Number_of_streams = temp$NumberStream,
                     Sample_size = temp$n,
                     Number_of_Studies = temp$trials
                   )
                   row.names(out) <- temp$id

                   # row.names(out) <- NULL
                   return(out)
                 }

                 # dt_invisselect <- function(select, hidden){
                 #     temp <- edges[ which( ( edges$id %in% select == T) & (edges$id %in% hidden == T) ), ]
                 #     out <- data.frame(Contrast=temp$contrast,
                 #                       Robustness=round(temp$contrast_robust, 3),
                 #                       Mean_stream_length=round(temp$meanStreamLen, 3),
                 #                       Number_of_streams=temp$NumberStream,
                 #                       Sample_size=temp$n,
                 #                       Number_of_Studies=temp$trials)
                 #     row.names(out) <- temp$id
                 #
                 #     # row.names(out) <- NULL
                 #     return(out)
                 # }

                 # dt_invisunselect <- function(select, hidden){
                 #     temp <- edges[ which( ( edges$id %in% select == F) & (edges$id %in% hidden == T) ), ]
                 #     out <- data.frame(Contrast=temp$contrast,
                 #                       Robustness=round(temp$contrast_robust, 3),
                 #                       Mean_stream_length=round(temp$meanStreamLen, 3),
                 #                       Number_of_streams=temp$NumberStream,
                 #                       Sample_size=temp$n,
                 #                       Number_of_Studies=temp$trials)
                 #     row.names(out) <- temp$id
                 #
                 #     # row.names(out) <- NULL
                 #     return(out)
                 # }

                 # react
                 df1 <-
                   reactive({
                     dt_visselect(myEdge$selected, filterrobust$value)
                   })
                 # df2 <- reactive({ dt_invisselect(myEdge$selected, filterrobust$value) })
                 df3 <-
                   reactive({
                     dt_visunselect(myEdge$selected, filterrobust$value)
                   })
                 # df4 <- reactive({ dt_invisunselect(myEdge$selected, filterrobust$value) })


                 output$visselected <- renderDataTable({
                   # datatable(dt_visselect(myEdge$selected, filterrobust$value), caption="Selected contrasts")
                   datatable(df1(), caption = "Selected contrasts"
                             , options = list(pageLength = 5))
                 })
                 # output$invisselected <- renderDataTable({
                 #     # datatable(dt_invisselect(myEdge$selected, filterrobust$value), caption="Selected contrasts")
                 #     datatable(df2())
                 # })
                 output$visunselected <- renderDataTable({
                   # datatable(dt_visunselect(myEdge$selected, filterrobust$value), caption="Unselected contrasts")
                   datatable(df3(), caption = "Unselected contrasts"
                             , options = list(pageLength = 5))
                 })
                 # output$invisunselected <- renderDataTable({
                 #     # datatable(dt_invisunselect(myEdge$selected, filterrobust$value), caption="Unselected contrasts")
                 #     datatable(df4())
                 # })

                 # left datatable action --------------------------------------------------------
                 # press action button and deselect
                 observeEvent(input$deselect_vis, {
                   if (!is.null(input$visselected_rows_selected)) {
                     id <-
                       edges[which(edges$id %in% filterrobust$value == F &
                                     edges$id %in% myEdge$selected),]$id

                     rmrows <- as.vector(input$visselected_rows_selected)
                     myEdge$selected <<- sort(id)[-rmrows]

                     updatanewargument(
                       highrisk = highrisk_reactV$value,
                       edgesizeby = edgewidth_reactV$value,
                       sliderfilter = filterrobust$value,
                       hover.edge = myEdgehover$hovered,
                       select.edge = myEdge$selected,
                       tabledeselectededge = tabledeselect$value,
                       tableselectededge = tableselect$value
                     )

                   }

                 })
                 # highlihgt the deselected edges on the plot
                 tabledeselect <- reactiveValues(value = NULL)

                 observeEvent(input$visselected_rows_selected, ignoreNULL = FALSE, {
                   id <-
                     edges[which(edges$id %in% filterrobust$value == F  &
                                   edges$id %in% myEdge$selected),]$id
                   tabledeselect$value <-
                     sort(id)[as.vector(input$visselected_rows_selected)]
                   updatanewargument(
                     highrisk = highrisk_reactV$value,
                     edgesizeby = edgewidth_reactV$value,
                     sliderfilter = filterrobust$value,
                     hover.edge = myEdgehover$hovered,
                     select.edge = myEdge$selected,
                     tabledeselectededge = tabledeselect$value,
                     tableselectededge = tableselect$value
                   )

                 })

                 # deselect all
                 observeEvent(input$deselect_vis_all, {
                   myEdge$selected <<- NULL
                   updatanewargument(
                     highrisk = highrisk_reactV$value,
                     edgesizeby = edgewidth_reactV$value,
                     sliderfilter = filterrobust$value,
                     hover.edge = myEdgehover$hovered,
                     select.edge = myEdge$selected,
                     tabledeselectededge = tabledeselect$value,
                     tableselectededge = tableselect$value
                   )
                 })

                 # right datatable action ----------------------------------------------------
                 # press action button and deselect
                 observeEvent(input$select_vis, {
                   if (!is.null(input$visunselected_rows_selected)) {
                     id <-
                       edges[which(edges$id %in% filterrobust$value == F  &
                                     !(edges$id %in% myEdge$selected)),]$id
                     addrows <- as.vector(input$visunselected_rows_selected)
                     myEdge$selected <<-
                       c(myEdge$selected, sort(id)[addrows])

                     updatanewargument(
                       highrisk = highrisk_reactV$value,
                       edgesizeby = edgewidth_reactV$value,
                       sliderfilter = filterrobust$value,
                       hover.edge = myEdgehover$hovered,
                       select.edge = myEdge$selected,
                       tabledeselectededge = tabledeselect$value,
                       tableselectededge = tableselect$value
                     )
                   }

                 })
                 # highlihgt the deselected edges on the plot
                 tableselect <- reactiveValues(value = NULL)

                 observeEvent(input$visunselected_rows_selected, ignoreNULL = FALSE, {
                   id <-
                     edges[which(edges$id %in% filterrobust$value == F  &
                                   !(edges$id %in% myEdge$selected)),]$id
                   tableselect$value <-
                     sort(id)[as.vector(input$visunselected_rows_selected)]

                   updatanewargument(
                     highrisk = highrisk_reactV$value,
                     edgesizeby = edgewidth_reactV$value,
                     sliderfilter = filterrobust$value,
                     hover.edge = myEdgehover$hovered,
                     select.edge = myEdge$selected,
                     tabledeselectededge = tabledeselect$value,
                     tableselectededge = tableselect$value
                   )

                 })

                 # select all
                 observeEvent(input$select_vis_all, {
                   myEdge$selected <<- edges$id
                   updatanewargument(
                     highrisk = highrisk_reactV$value,
                     edgesizeby = edgewidth_reactV$value,
                     sliderfilter = filterrobust$value,
                     hover.edge = myEdgehover$hovered,
                     select.edge = myEdge$selected,
                     tabledeselectededge = tabledeselect$value,
                     tableselectededge = tableselect$value
                   )
                 })


                 # select nodes and highlight edges connected ------------------------------
                 # 現在只剩沒highlight，但點下去是對的


                 # reset plot when pressing the reset button ---------------------------------
                 observeEvent(input$resetplot, {
                   shinyjs::reset("highriskswitcher")
                   shinyjs::reset("nodesize")
                   shinyjs::reset("edgewidth")
                   shinyjs::reset("filterrobustness")

                   output$visNetworkRobustness <<-
                     render_get.interactive.contrast_robust_plot()

                   # reset input reactive values
                   nodesize_reactV <<- reactiveValues(value = 1)
                   edgewidth_reactV <<- reactiveValues(value = 0)
                   highrisk_reactV <<- reactiveValues(value = F)
                   filterrobust <<- reactiveValues(value = NULL)
                   # reset click input
                   myEdge <<- reactiveValues(selected = NULL)
                   myEdgehover <<- reactiveValues(hovered = NULL)

                   # reset data table to show whole table
                   output$visselected <- renderDataTable({
                     datatable(dt_visselect(myEdge$selected, filterrobust$value),
                               caption = "Selected contrasts"
                               , options = list(pageLength = 5))
                   })
                   # output$invisselected <- renderDataTable({
                   #     datatable(dt_invisselect(myEdge$selected, filterrobust$value), caption="Selected contrasts")
                   # })
                   output$visunselected <- renderDataTable({
                     datatable(dt_visunselect(myEdge$selected, filterrobust$value),
                               caption = "Unselected contrasts"
                               , options = list(pageLength = 5))
                   })
                   # output$invisunselected <- renderDataTable({
                   #     datatable(dt_invisunselect(myEdge$selected, filterrobust$value), caption="Unselected contrasts")
                   # })

                 })


                 # histogram
                 output$distPlot <- renderPlot({
                   x    <- edges$contrast_robust
                   bins <- seq(min(x), max(x), length.out = input$bins + 1)

                   hist(
                     x,
                     breaks = bins,
                     col = "#75AADB",
                     border = "white",
                     main = "Histogram of Contrast robustness"
                   )
                 })
                 
                 return(list(
                   btn_contrasts = reactive(input$btn_contrasts)
                   ,
                   edge.selected = reactive(df1()$Contrast)
                   )
                  )
               })
}

# ui <- fluidPage(
#   networkrobustbessUI("networkrobustbess")
# )
#
# server <- function(input, output, session) {
#   networkrobustbessServer("networkrobustbess")
# }
#
# shinyApp(ui = ui, server = server)