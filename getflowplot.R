
getflowplot <- function(indata, hatmatrix, comparison){
    # source("./getflow.R")
    # source("./robustness.R")
    library(ggplot2)
    library(plotly)
    
    # get studies_df with rob
    studies_df <- get.studies_rob(indata, hatmatrix, comparison)
    
    # reorder studies_df to plot in the right order
    reorder_df <- data.frame()
    
    for(i in unlist(unique(studies_df$path))){
        # get a subset of data for one path
        temp_df <- subset(studies_df, path==i)
        temp_df$edgehead <- factor(temp_df$edgehead, levels=unlist(strsplit(i, ":")))
        
        # reorder w/ edgehead & rob (or study.cont is aslo available)
        temp_df <- temp_df[order(temp_df$edgehead, -temp_df$rob, -temp_df$study.cont, temp_df$study),]
        reorder_df <- rbind(reorder_df, temp_df)
    }
    
    
    ##### plot
    
    ### treatment contrast position df
    t_df <- subset(reorder_df, select=c(path, contrastinflow, flowperedge, flow))
    t_df <- t_df[!duplicated(t_df),]
    t_df_pos <- data.frame()
    for(i in t_df$path){
        t_df_subpath <- subset(t_df, path==i)
        t_df_subpath$cumulativeflow <- 0
        t_df_subpath$treatment_name <- ""
        for(j in 1:nrow(t_df_subpath)){
            t_df_subpath$cumulativeflow[j] <- t_df_subpath$flowperedge[1] * j
            t_df_subpath$treatment_name[j] <- strsplit(i, ":")[[1]][j+1]
        }
        t_df_subpath <- rbind(data.frame(path=i, 
                                         contrastinflow="sv", 
                                         flowperedge=t_df_subpath$flowperedge[1], 
                                         flow=t_df_subpath$flow[1], 
                                         cumulativeflow=0, 
                                         treatment_name=strsplit(comparison,":")[[1]][1]), 
                              t_df_subpath)
        
        t_df_pos <- rbind(t_df_pos, t_df_subpath)
    }

    ### ggplot
    # create a SharedData object for use in the ggplot below, group by 'groups' 
    # important: the variable has to be continuous instead of discrete
    # since in ggplot2 version 2.2.1 the order of the stack is no longer determined by the row order in the data.frame. 
    # Instead, it matches the order of the legend as determined by the order of levels in the factor.
    # So, if the fill or group in aes is discrete, the order would follow it's levels
    reorder_df$study <- as.integer(reorder_df$study)
    d_shareddata <- highlight_key(reorder_df, ~study)
    #t_df_pos_shareddata <- highlight_key(t_df_pos, ~treatment_name)
    
    ## 1
    if( (1 %in% reorder_df$rob) & 
        (!2 %in% reorder_df$rob) &
        (!3 %in% reorder_df$rob))
    {
      # here we reorder x
      plot <- ggplot() + 
        
        # text
        ggtitle(paste0("Contrast Estimate ",reorder_df$comparison[1])) +
        xlab("Stream") + ylab("Proportional Contribution (%)") +
        # hide legend
        guides(fill="none") +
        
        # studydata
        geom_bar(data=d_shareddata, 
                 stat = "identity",
                 #position='dodge',
                 colour = "white", 
                 aes(x=reorder(path, flow), y=study.cont, fill = "darkgreen"
                     
                     ### 2 ways to customize tooltip
                     ## 1. use label, label2, label3...
                     # label1=study, label2=study.cont, label3=rob)
                 )) + #, alpha=study.cont
        geom_point(data=t_df_pos, 
                   aes(x=reorder(path, flow), y=cumulativeflow),
                   shape=18) +
        
        ### this one works at non-interactive plot
        # # treatment contrast data to become a frame
        # geom_bar(data=t_df,
        #          stat= "identity",
        #          aes(x=reorder(path, flow), y=flowperedge),
        #          fill = NA,
        #          colour="grey2",
        #          size=1)+
        
        # flip the plot
      coord_flip()
    }
    ## 3
    else if ( (!1 %in% reorder_df$rob) & 
        (!2 %in% reorder_df$rob) &
        (3 %in% reorder_df$rob))
    {
      # here we reorder x
      plot <- ggplot() + 
        
        # text
        ggtitle(paste0("Contrast Estimate ",reorder_df$comparison[1])) +
        xlab("Stream") + ylab("Proportional Contribution (%)") +
        # hide legend
        guides(fill="none") +
        
        # studydata
        geom_bar(data=d_shareddata, 
                 stat = "identity",
                 #position='dodge',
                 colour = "white", 
                 aes(x=reorder(path, flow), y=study.cont, fill = "red"
                     
                     ### 2 ways to customize tooltip
                     ## 1. use label, label2, label3...
                     # label1=study, label2=study.cont, label3=rob)
                 )) + #, alpha=study.cont
        geom_point(data=t_df_pos, 
                   aes(x=reorder(path, flow), y=cumulativeflow),
                   shape=18) +
        
        ### this one works at non-interactive plot
        # # treatment contrast data to become a frame
        # geom_bar(data=t_df,
        #          stat= "identity",
        #          aes(x=reorder(path, flow), y=flowperedge),
        #          fill = NA,
        #          colour="grey2",
        #          size=1)+
        
        # flip the plot
      coord_flip()
    }
    ## 1, 2, 3
    else {
      # here we reorder x
      plot <- ggplot() + 
        
        # text
        ggtitle(paste0("Contrast Estimate ",reorder_df$comparison[1])) +
        xlab("Stream") + ylab("Proportional Contribution (%)") +
        # hide legend
        guides(fill="none") +
        
        # studydata
        geom_bar(data=d_shareddata, 
                 stat = "identity",
                 #position='dodge',
                 colour = "white", 
                 aes(x=reorder(path, flow), y=study.cont, fill = rob
                     
                     ### 2 ways to customize tooltip
                     ## 1. use label, label2, label3...
                     # label1=study, label2=study.cont, label3=rob)
                 )) + #, alpha=study.cont
        geom_point(data=t_df_pos, 
                   aes(x=reorder(path, flow), y=cumulativeflow),
                   shape=18) +
        
        ### this one works at non-interactive plot
        # # treatment contrast data to become a frame
        # geom_bar(data=t_df,
        #          stat= "identity",
        #          aes(x=reorder(path, flow), y=flowperedge),
        #          fill = NA,
        #          colour="grey2",
        #          size=1)+
        
        # flip the plot
      coord_flip() +
        
        # color of rob
        # 如果只有一個rob會變黃色
        scale_fill_gradient2(
          low = "darkgreen", 
          mid = "gold", 
          high = "red", 
          midpoint = 2,
          na.value = "grey50"
        )
    }
    


    interactive.flowplot <- ggplotly(plot, tooltip = c("text"))
    
    ### 2 ways to customize tooltip hoverinfo
    ## Better: 2. directly modify the text data
    # since they are divided into 3 rob groups
    n <- 0
    
    for(i in sort(unique(reorder_df$rob)) ){
      reorder_df_groupbyrob <- reorder_df[which(reorder_df$rob == i),]
      
      if ( length(unique(reorder_df$rob)) == 3 ) {
        if(i == 1){
          n <- 1
        }
        else if (i == 2){
          n <- 3
        }
        else if (i == 3){
          n <- 2
        }
      }
        
      if ( length(unique(reorder_df$rob)) == 2) {
        if ( (1 %in% unique(reorder_df$rob)) &  (2 %in% unique(reorder_df$rob)) ) {
          if (i == 2){
            n <- 2
          }
          else if (i == 3){
            n <- 1
          }
        }
        n <- n + 1
      }
      
      if ( length(unique(reorder_df$rob)) == 1) {
        n <- n + 1
      }

      interactive.flowplot$x$data[[n]]$text <- paste0("Study ID: ", reorder_df_groupbyrob$study,
                                                      "<br>", reorder_df_groupbyrob$contrastinflow,
                                                      "<br>Contribution: ", round(reorder_df_groupbyrob$study.cont, 2),
                                                      "<br>ROB: ", reorder_df_groupbyrob$rob
                                                      )
    }
    
    interactive.flowplot$x$data[[ length(unique(reorder_df$rob)) + 1 ]]$text <- paste0(t_df_pos$treatment_name)
    
    interactive.flowplot <- highlight(interactive.flowplot, on = "plotly_click", off = "plotly_doubleclick")
    
    return(interactive.flowplot)
}

