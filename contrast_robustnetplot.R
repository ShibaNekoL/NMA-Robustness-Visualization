

get.contrast_robust_plot <- function(indata, hatmatrix){
    source("./robustness.R")
    library(dplyr)
    library(igraph)
    library(ggraph)
    library(tidygraph)

    ### create node data
    nodes <- aggregate(subset(indata, select=c("r", "n")), by=list(indata$t), FUN=sum)
    names(nodes)[1] <- "t"
    
    
    ### create edge data
    # convert to number as the order of nodes
    contrast_robust_df <- get.contrast_robust(indata, hatmatrix)
    
    contrast_robust_df$edgehead <- unlist(lapply(contrast_robust_df$contrast, function(e){strsplit(e, ":")[[1]][1]}))
    contrast_robust_df$edgetail <- unlist(lapply(contrast_robust_df$contrast, function(e){strsplit(e, ":")[[1]][2]}))
    
    contrast_robust_df$from <- unlist(lapply(contrast_robust_df$edgehead, function(e){which(nodes$t == e)}))
    contrast_robust_df$to <- unlist(lapply(contrast_robust_df$edgetail, function(e){which(nodes$t == e)}))
    
    edges <- subset(contrast_robust_df, select=c("from", "to", "contrast", "contrast_robust"))
    
    ### tidygraph
    graphdata <- tbl_graph(nodes = nodes, edges = edges, directed = F)
    
    ### plot
    # https://www.rdocumentation.org/packages/ggraph/versions/2.0.5/topics/geom_edge_fan
    contrast_robust_plot <- ggraph(graphdata, layout = 'circle') + 
        
        theme(legend.position="right", panel.background = element_rect(fill = NA, colour = NA))+ #, legend.box = "horizontal"
        
        geom_edge_link(aes(colour=contrast_robust,
                           edge_width=contrast_robust)
                       #start_cap = circle(5, 'mm'),
                       #end_cap = circle(7, 'mm'),
                       #lineend = c('round'), #, 'butt', 'square'
                       #linejoin = c('mitre') # 'round', , 'bevel'
        ) +
        
        geom_node_point(aes(size=n), colour="grey39") + 
        # scale_size_continuous(range = c(5,15)) +
        # hide legend
        guides(edge_width="none",size="none") +
        #scale_edge_width_continuous(guide = "none")+
        #scale_size_continuous(guide="none")+
        #scale_edge_alpha(guide = "none", range=c(0.2,1))+
        
        scale_edge_color_gradient2(low = "red", 
                                   mid = "gold", 
                                   high = "darkgreen", 
                                   midpoint = 0.5,
                                   name = "Contrast Robustness"
        ) +
        
        # text
        #ggtitle() +
        #geom_node_label
        geom_node_text(aes(label = t,
                           x = x * 1.15, 
                           y = y * 1.15 
                           #angle = angle, 
                           #hjust = hjust
        ),
        size = 4,  
        repel=F)    
    
    #geom_edge_text
    # geom_edge_text(aes(label = contrast_robust,
    #                    # x = x * 1.15, 
    #                    # y = y * 1.15, 
    #                    #angle = angle, 
    #                    #hjust = hjust
    # ),
    # size = 4,  
    # repel=T)
    
    return(contrast_robust_plot)
}


##### interactive
get.interactive.contrast_robust_plot <- function(indata, hatmatrix, sm, highrisk=F, nodesizeby=1, edgesizeby=0){
    source("./robustness.R")
    library(dplyr)
    library(igraph)
    library(ggraph)
    library(visNetwork) 
    library(scales)

    ### create node data
    indata$trials <- 1
    nodes <- aggregate(subset(indata, select=c("r", "n", "trials")), by=list(indata$t), FUN=sum)
    names(nodes)[1] <- "t"
    
    ### create edge data

    # convert to number as the order of nodes
    contrast_robust_df <- get.contrast_robust(indata, hatmatrix)
    
    contrast_robust_df$edgehead <- unlist(lapply(contrast_robust_df$contrast, function(e){strsplit(e, ":")[[1]][1]}))
    contrast_robust_df$edgetail <- unlist(lapply(contrast_robust_df$contrast, function(e){strsplit(e, ":")[[1]][2]}))
    
    contrast_robust_df$from <- unlist(lapply(contrast_robust_df$edgehead, function(e){which(nodes$t == e)}))
    contrast_robust_df$to <- unlist(lapply(contrast_robust_df$edgetail, function(e){which(nodes$t == e)}))
    
    edges <- subset(contrast_robust_df, select=c("from", "to", "contrast", "contrast_robust", "meanStreamLen", "NumberStream"))
    
    ### nodes attribution----------------------------------------------------
    
    vis.nodes <- nodes
    vis.nodes$id <- 1:nrow(vis.nodes)
    names(vis.nodes)[1] <- "label"
    
    # size ------
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
    # tooltip label of nodes ------
    vis.nodes$title <- paste0("Sample size: ", vis.nodes$n,
                              "<br>Number of studies: ", vis.nodes$trials)
    
    
    #vis.nodes$font.size <- vis.nodes$size
    
    # highlight the interested treatment comparison ------
    vis.nodes$color.background <- "grey"
    vis.nodes$color.border <- "grey"

    ### edges attribution---------------------------------------------------
    vis.links <- edges
    
    ### count edge trials for width ----
    indata$t<-as.factor(indata$t)
    # 注意這邊sm="OR"到時候要改成輸入式參數
    indata_wide <- pairwise(treat=t, event=r, n=n, studlab=id, data=indata, sm=sm)
    
    indata_wide$pair <- paste0(indata_wide$t1, ":", indata_wide$t2)
    
    # add sample size, number of studies
    vis.links$n <- vis.nodes[vis.links$from, ]$n + vis.nodes[vis.links$to, ]$n

    treatment_trials <- as.data.frame(with(indata_wide, table(treat1,treat2)))
    treatment_trials$treat1 <- as.character(treatment_trials$treat1)
    treatment_trials$treat2 <- as.character(treatment_trials$treat2)

    treatment_trials <- treatment_trials[treatment_trials$treat1 != treatment_trials$treat2, ]
    treatment_trials <- treatment_trials[treatment_trials$Freq != 0,]

    treatment_trials$tt1 <- paste0(treatment_trials$treat1, ":", treatment_trials$treat2)
    treatment_trials$tt2 <- paste0(treatment_trials$treat2, ":", treatment_trials$treat1)
    treatment_trials_for_merge_1 <- treatment_trials[,c("tt1", "Freq")]
    treatment_trials_for_merge_2 <- treatment_trials[,c("tt2", "Freq")]

    vis.links <- merge(vis.links, treatment_trials_for_merge_1,
          by.x = "contrast", by.y = "tt1", all.x = TRUE
    )
    vis.links <- merge(vis.links, treatment_trials_for_merge_2,
         by.x = "contrast", by.y = "tt2", all.x = TRUE
    )
    
    vis.links$Freq.x[which(is.na(vis.links$Freq.x))] <- 0
    vis.links$Freq.y[which(is.na(vis.links$Freq.y))] <- 0
    
    vis.links$trials <- vis.links$Freq.x + vis.links$Freq.y

    vis.links$trials[which(is.na(vis.links$trials))] <- 0
    
    # vis.links$trials <- as.vector(table(indata_wide$pair)[vis.links$contrast])
    # vis.links$trials[which(is.na(vis.links$trials))] <- 0
    
    # width of edges ------
    # edge size by: Robustness(0), Sample size(1), Number of studies(2), Equal size(3)
    edgesmallest <- 1
    edgebiggest <- 15
    
    vis.links$width_0 <- vis.links$contrast_robust
    vis.links$width_0 <- edgesmallest + (vis.links$width_0 - min(vis.links$width_0)) / (max(vis.links$width_0) - (min(vis.links$width_0)))  * (edgebiggest - edgesmallest)
    
    vis.links$width_1 <- vis.links$n
    vis.links$width_1 <- edgesmallest + (vis.links$width_1 - min(vis.links$width_1)) / (max(vis.links$width_1) - (min(vis.links$width_1)))  * (edgebiggest - edgesmallest)
    
    vis.links$width_2 <- vis.links$trials
    vis.links$width_2 <- edgesmallest + (vis.links$width_2 - min(vis.links$width_2)) / (max(vis.links$width_2) - (min(vis.links$width_2)))  * (edgebiggest - edgesmallest)
    
    vis.links$width_3 <- 7
    
    # robustness inverse
    vis.links$width_4 <- vis.links$contrast_robust
    vis.links$width_4 <- edgesmallest + (max(vis.links$width_0) - vis.links$width_0) / (max(vis.links$width_0) - (min(vis.links$width_0)))  * (edgebiggest - edgesmallest)
    
    if(edgesizeby==0){
        vis.links$width <- vis.links$width_0
    }else if(edgesizeby==1){
        vis.links$width <- vis.links$width_1
    }else if(edgesizeby==2){
        vis.links$width <- vis.links$width_2
    }else if(edgesizeby==3){
        vis.links$width <- vis.links$width_3
    }else if(edgesizeby==4){
        vis.links$width <- vis.links$width_4
    }

    # highrisk display -----
    
    # F
    colors_F <- c("darkred", "gold", "darkgreen")
    pal_F <- colorRampPalette(colors_F)(100)

    vis.links$c_ref_F <- findInterval(vis.links$contrast_robust, seq(from = 0.1, to = 0.9, length.out = 100), all.inside=T)
    vis.links$color_F <- pal_F[vis.links$c_ref_F]
    
    # T
    colors_T <- c("darkred", "#FFFFFF")
    pa <- colorRampPalette(colors_T)(100)
    alpha <- seq(from=1,to=0,length.out=100)
    pal_T <- mapply(
        function(RGB, a){ paste0("rgba(", RGB,",", a, ")") }, 
        unlist( lapply( 
            lapply(pa, col2rgb), 
            function(c) { paste0(c, collapse = ",") } 
        )
        ), 
        alpha)
    
    # data rob range now c(0.1,0.9) converts to alpha c(0,1)
    vis.links$c_ref_T <- findInterval(vis.links$contrast_robust, seq(from = 0.1, to = 0.9, length.out = 100), all.inside=T)
    vis.links$color_T <- pal_T[vis.links$c_ref_T]
    
    if (highrisk==F) {
        vis.links$color <- vis.links$color_F
    } else if (highrisk==T) {
        vis.links$color <- vis.links$color_T
    }
    

    # tooltip title of edges ------
    vis.links$title <- paste0("Robustness: ", round(vis.links$contrast_robust, 2), 
                              "<br>Mean length: ", vis.links$meanStreamLen,
                              "<br>Number of Streams: ", vis.links$NumberStream
                              )
    
    # dashes if it's indirect evidence -----------------
    vis.links$dashes <- !vis.links$contrast %in% hatmatrix$colNames
    
    # # corret highlight degree to 0: create fake edge ---------
    # vis.links.fake <- vis.links %>%
    #     select(to, everything())
    # 
    # names(vis.links.fake)[1:2] <- c("from", "to")
    # 
    # vis.links <- rbind(vis.links, vis.links.fake)
    
    ### igraph to get position
    graphdata <- tbl_graph(nodes = vis.nodes, edges = vis.links, directed = F)

    igraph_plot <- ggraph(graphdata, layout = 'circle') + geom_node_point()
    vis.nodes$x <- igraph_plot$data$x * 178
    # to make the html object axis same as ggplot
    vis.nodes$y <- - igraph_plot$data$y * 178
    
    # vis.links add id
    vis.links$id <- 1:nrow(vis.links)
    
    ### hidden default to F
    vis.links$hidden <- F
    
    ### shadows default to F
    vis.links$shadow <- F
    
    ### plot ----------------------------------
    
    vnet <- visNetwork(nodes=vis.nodes, edges=vis.links, 
                       width="800px", height="800px",
                       main = "Robustness of NMA Estimate", 
                       submain = list(text = paste0("Overall NMA Robustness: ", round(get.network_robust(indata, hatmatrix),3)), 
                                      style = "font-size:20px;text-align:center;") #font-family:Comic Sans MS;color:#ff0000;
                       #footer = "Fig.1 minimal example"
                       ) %>% # edges=vis.links[,-ncol(vis.links)] for corret highlight
        visEdges(physics = F, selectionWidth=0, smooth = FALSE) %>% #, hoverWidth=1 , color = list(highlight = "blue", hover = "blue")
        visNodes(physics = F) %>% #, shape='circle'
        # visIgraphLayout(layout="layout_in_circle") %>% 
        # visPhysics(
        #     # solver="repulsion",
        #     # repulsion=list(nodeDistance=100),
        #     barnesHut=list(gravitationalConstant=-100),
        # ) %>%
        # bug: I only want the nearest points and edges, not including the links between them
        # visOptions(highlightNearest = list(enabled = T, hover = T, degree=0, algorithm="all", hideColor="rgba(200,200,200,0)" ), nodesIdSelection = T, manipulation = F) %>%
        visInteraction(keyboard = TRUE,
                       dragNodes = T, 
                       dragView = T, 
                       zoomView = T,
                       tooltipDelay=0,
                       tooltipStay=0,
                       navigationButtons = F
                       # multiselect = T
                       )
    return(vnet)
}

