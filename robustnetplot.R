

get.robustnetworkplot <- function(indata, hatmatrix, comparison){
    # source("./robustness.R")
    library(dplyr)
    library(igraph)
    library(ggraph)
    library(tidygraph)
    
    ####### each edge
    netdata <- get.robustness(indata, hatmatrix, comparison)$allvariables
    
    ### create node data
    nodes <- aggregate(subset(indata, select=c("r", "n")), by=list(indata$t), FUN=sum)
    names(nodes)[1] <- "t"
    
    ### create edge data
    # flow
    # here we can also use flowperedge
    edges <- unique(subset(netdata, select=c("edgehead", "edgetail", "path", "flow", "edge_p", "path_p", "contrastinflow", "length")))
    edges$path <- unlist(edges$path)
    # convert to number as the order of nodes
    edges$from <- unlist(lapply(edges$edgehead, function(e){which(nodes$t == e)}))
    edges$to <- unlist(lapply(edges$edgetail, function(e){which(nodes$t == e)}))
    
    edges <- subset(edges, select=c("from", "to", "edgehead", "path", "flow", "edge_p", "path_p","contrastinflow", "length"))
    
    ### tidygraph
    graphdata <- tbl_graph(nodes = nodes, edges = edges, directed = T)
    
    ### plot
    # https://www.rdocumentation.org/packages/ggraph/versions/2.0.5/topics/geom_edge_fan
    robustnetworkplot <- ggraph(graphdata, layout = 'circle') + 
        
        theme(legend.position="right", panel.background = element_rect(fill = NA, colour = NA))+ #, legend.box = "horizontal"
        
        geom_edge_fan(aes(colour=path, 
                          edge_width=flow, 
                          edge_alpha=edge_p),
                      arrow = arrow(length = unit(3,'mm')), # ,type = 'closed'
                      #start_cap = circle(5, 'mm'),
                      end_cap = circle(7, 'mm'),
                      lineend = c('round'), #, 'butt', 'square'
                      linejoin = c('mitre') # 'round', , 'bevel'
        ) +
        
        geom_node_point(aes(size=n), colour="grey39") + 
        # highlight comparison
        geom_node_point(aes(filter=(t==strsplit(comparison, ":")[[1]][1] | t==strsplit(comparison, ":")[[1]][2]), size=n), colour = "darkred") +
        # scale_size_continuous(range = c(5,15)) +
        
        # hide legend
        guides(edge_width="none",size="none") +
        #scale_edge_width_continuous(guide = "none")+
        #scale_size_continuous(guide="none")+
        scale_edge_alpha(guide = "none", range=c(0.2,1))+
        
        # text
        ggtitle(paste0("Comparison ",netdata$comparison[1])) +
        #geom_node_label
        geom_node_text(aes(label = t,
                           x = x * 1.15, 
                           y = y * 1.15, 
                           #angle = angle, 
                           #hjust = hjust
        ),
        size = 4,  
        repel=F)
    
    
    return(robustnetworkplot)
}

get.interactive.robustnetworkplot <- function(indata, hatmatrix, comparison, highrisk=F, nodesizeby=1, edgesizeby=1){
    # source("./robustness.R")
    library(dplyr)
    library(igraph)
    library(ggraph)
    library(visNetwork) 
    library(scales)
    
    ####### each edge
    netdata <- get.robustness(indata, hatmatrix, comparison)$allvariables
    
    ### create node data
    indata$trials <- 1
    nodes <- aggregate(subset(indata, select=c("r", "n", "trials")), by=list(indata$t), FUN=sum)
    names(nodes)[1] <- "t"
    
    ### create edge data
    # flow
    # here we can also use flowperedge
    edges <- unique(subset(netdata, select=c("edgehead", "edgetail", "path", "flow", "edge_p", "path_p", "contrastinflow", "length")))
    edges$path <- unlist(edges$path)
    # convert to number as the order of nodes
    edges$from <- unlist(lapply(edges$edgehead, function(e){which(nodes$t == e)}))
    edges$to <- unlist(lapply(edges$edgetail, function(e){which(nodes$t == e)}))
    
    edges <- subset(edges, select=c("from", "to", "edgehead", "path", "flow", "edge_p", "path_p","contrastinflow", "length"))
    
    ### nodes attribution----------------------------------------------------
    
    vis.nodes <- nodes
    vis.nodes$id <- 1:nrow(vis.nodes)
    names(vis.nodes)[1] <- "label"
    
    # size ---------------------

    nodesmallest <- 3
    nodebiggest <- 15
    
    vis.nodes$size_2 <- nodesmallest + (vis.nodes$trials - min(vis.nodes$trials)) / (max(vis.nodes$trials) - (min(vis.nodes$trials)))  * (nodebiggest - nodesmallest)
    vis.nodes$size_1 <- nodesmallest + (vis.nodes$n - min(vis.nodes$n)) / (max(vis.nodes$n) - (min(vis.nodes$n)))  * (nodebiggest - nodesmallest)
    vis.nodes$size_0 <- 10
    
    if(nodesizeby == 2){
        vis.nodes$size <- vis.nodes$size_2
    }else if (nodesizeby == 1){
        vis.nodes$size <- vis.nodes$size_1
    }else if (nodesizeby == 0){
        vis.nodes$size <- vis.nodes$size_0
    }
    #vis.nodes$font.size <- vis.nodes$size
    
    # tooltip label of nodes ------
    vis.nodes$title <- paste0("Sample size: ", vis.nodes$n, 
                              "<br>Number of studies: ", vis.nodes$trials)
    
    
    # highlight the interested treatment comparison ------
    vis.nodes$color.background <- "grey"
    vis.nodes$color.border <- "grey"
    vis.nodes[which(vis.nodes$label==strsplit(comparison, ":")[[1]][1] | vis.nodes$label==strsplit(comparison, ":")[[1]][2]), ]$color.background <- "darkred"
    vis.nodes[which(vis.nodes$label==strsplit(comparison, ":")[[1]][1] | vis.nodes$label==strsplit(comparison, ":")[[1]][2]), ]$color.border <- "darkred"
    

    
    ### edges attribution---------------------------------------------------
    vis.links <- merge(edges, 
                       data.frame(contrastinflow=names(table(edges$contrastinflow)), 
                                  smooth=table(edges$contrastinflow) > 1), by=c("contrastinflow"),
                       all.x=T)
    
    # width of edges ------
    edgesmallest <- 1
    edgebiggest <- 15
    
    vis.links$width_1 <- vis.links$flow
    vis.links$width_1 <- edgesmallest + (vis.links$width_1 - min(vis.links$width_1)) / (max(vis.links$width_1) - (min(vis.links$width_1)))  * (edgebiggest - edgesmallest)
    
    vis.links$width_0 <- 7
    
    if(edgesizeby==1){
        vis.links$width <- vis.links$width_1
    }else if(edgesizeby==0){
        vis.links$width <- vis.links$width_0
    }

    # color of edges ------
    # F
    color_list_F <- unlist(lapply(hue_pal()(length(unique(vis.links$path))), function(x){paste(as.vector(col2rgb(x)), collapse=",")}))
    
    vis.links$c_ref_F <- as.integer(factor(vis.links$path))
    vis.links$color_Xaplha_F <- color_list_F[vis.links$c_ref_F]
    
    vis.links$color_F <- unlist( mapply(function(x, y){ paste0("rgba(", x, ",", y, ")" ) }, vis.links$color_Xaplha_F, vis.links$edge_p ) )
    # T
    colors_T <- c("darkred", "#FFFFFF")
    pa_T <- colorRampPalette(colors_T)(100)
    alpha <- seq(from=1,to=0,length.out=100)
    pal_T <- mapply(
        function(RGB, a){ paste0("rgba(", RGB,",", a, ")") }, 
        unlist( lapply( 
            lapply(pa_T, col2rgb), 
            function(c) { paste0(c, collapse = ",") } 
        )
        ), 
        alpha)
    
    # data rob range now c(0.1,0.9) converts to alpha c(0,1)
    vis.links$c_ref_T <- findInterval(vis.links$edge_p, seq(from = 0.1, to = 0.9, length.out = 100))
    vis.links$color_T <- pal_T[vis.links$c_ref_T] 
    
    ### if else
    if (highrisk==F) {
        vis.links$color <- vis.links$color_F
        vis.links <- arrange(vis.links, path, contrastinflow)

    ### legend --------------------------------
        ledges <- data.frame(
            id = 1:length(unlist(unique(vis.links$path))), 
            label = unlist(unique(vis.links$path)), 
            color = unlist(lapply(unlist(unique(vis.links$color_Xaplha)), function(c){paste0("rgba(",c,",1)")})),
            font.align = "bottom" # or top
        ) 
        
    } else if (highrisk==T) {
        vis.links$color <- vis.links$color_T
    }    
    
    # label of edges ------
    # arrange order
    # reorder studies_df to plot in the right order
    reorder_df_edge <- data.frame()
    
    for(i in unlist(unique(vis.links$path))){
        # get a subset of data for one path
        temp_df_edge <- subset(vis.links, path==i)
        temp_df_edge$edgehead <- factor(temp_df_edge$edgehead, levels=unlist(strsplit(i, ":")))
        
        # reorder w/ edgehead
        temp_df_edge <- temp_df_edge[order(temp_df_edge$edgehead),]
        reorder_df_edge <- rbind(reorder_df_edge, temp_df_edge)
    }
    
    vis.links <- reorder_df_edge
    
    vis.links$path_id <- as.integer(factor(vis.links$path))
    vis.links$idd <- 0
    
    for(i in 1:length(unique(vis.links$path_id))){
        vis.links[which(vis.links$path_id==i), ]$idd <- 1:nrow(vis.links[which(vis.links$path_id==i), ] )
    }
    
    # make sure the data are arraned by path and its inner edge for both highrisk=T & F
    vis.links <- arrange(vis.links, path_id, idd)
    
    # add id
    vis.links$id <- 1:nrow(vis.links)
    
    #vis.links$label <- paste0(vis.links$path_id, "-", vis.links$idd)
    
    # tooltip title of edges ------
    vis.links$title <- paste0("Robustness: ", 
                              "<br>", vis.links$contrastinflow, "  [",round(vis.links$edge_p,2), "]", 
                              "<br>", vis.links$path, "  [",round(vis.links$path_p, 2), "]",
                              "<br>Contribution: ", round(vis.links$flow, 2), 
                              "<br>Order: ", vis.links$idd, " / ", vis.links$length)
    ### igraph to get position
    graphdata <- tbl_graph(nodes = vis.nodes, edges = vis.links, directed = T)
    
    igraph_plot <- ggraph(graphdata, layout = 'circle') + geom_node_point()
    vis.nodes$x <- igraph_plot$data$x * 250
    # to make the html object axis same as ggplot
    vis.nodes$y <- - igraph_plot$data$y * 250
    
    ### edges default to hidden=F
    vis.links$hidden <- F
    
    ### edges default to shadow=F
    vis.links$shadow <- F
    
    ### plot ----------------------------------
    
    vnet <- visNetwork(nodes=vis.nodes, edges=vis.links, 
                       width="800px", height="600px",
                       main = "Decomposition in Streams and Robustness for Single Contrast Estimate", 
                       submain = list(text = paste0("Contrast estimate: [", comparison, "]", 
                                                    "<br>Robustness: ", paste0(round(get.robustness(indata, hatmatrix, comparison)$network_robustness.contrast[1],3))),
                                      style = "font-size:20px;text-align:center;") #font-family:Comic Sans MS;color:#ff0000;
                       #footer = "Fig.1 minimal example"
                       ) %>% # edges=vis.links[,-ncol(vis.links)] for corret highlight
        visEdges(arrows = "to", selectionWidth=0) %>% #, hoverWidth=1 , color = list(highlight = "blue", hover = "blue")
        visNodes(physics = F) %>% #, shape='circle'
        # visIgraphLayout(layout="layout_in_circle") %>%
        # visPhysics(
        #     # solver="repulsion",
        #     # repulsion=list(nodeDistance=100),
        #     barnesHut=list(gravitationalConstant=-100),
        # ) %>%
        # bug: I only want the nearest points and edges, not including the links between them
        visInteraction(keyboard = TRUE,
                       dragNodes = T, 
                       dragView = T, 
                       zoomView = T,
                       tooltipDelay=0,
                       tooltipStay=0,
                       navigationButtons = F
                       # multiselect = TRUE,
                       ) 
    # if(highrisk==F){
    #     vnet <- vnet %>%
    #                 # visOptions(highlightNearest = list(enabled = T, hover = T, degree=list(from = 1, to = 1), algorithm="hierarchical"), nodesIdSelection = TRUE) %>%
    #                 visLegend(main="Stream" , useGroups = FALSE, addEdges = ledges, position="right")
    # }    
    
        
    return(vnet)
}

