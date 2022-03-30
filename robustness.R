###### merge the statistics with rob

get.studies_rob <- function(indata, hatmatrix, comparison){
    
    
    ### study_rob: new data.frame of each study's rob
    study_rob <- subset(indata, select=c("id","rob"))
    # duplicated()
    study_rob <- study_rob[!duplicated(study_rob),]
    
    ### studies_df: dataframe from merge studies_df & rob
    studies_df <- merge(get.studies_df(hatmatrix, comparison)[[1]], study_rob, by.x="study", by.y="id", all.x=T)
    
    # reorder by path
    studies_df <- arrange(studies_df, path)
    return(studies_df)
}

###### an robustness index of each contrast in flow

get.robustness <- function(indata, hatmatrix, comparison){
    source("./getflow.R")
    studies_rob <- get.studies_rob(indata, hatmatrix, comparison)
    
    ### set risk for each rob 0.2, 0.5, 0.8
    # rob_q is the probability of failed
    studies_rob$rob_q <- ifelse(studies_rob$rob==1, 0.1, ifelse(studies_rob$rob==2, 0.5, ifelse(studies_rob$rob==3, 0.9, NA)))
    studies_rob$rob_p <- 1 - studies_rob$rob_q
    
    ### robustness of an edge in a flow
    ### edge_p: calculate the worked probability of a contrast in flow
    studies_rob$path.contrastinflow <- interaction(studies_rob$path, studies_rob$contrastinflow)
    
    studies_rob$edge_p <- NA
    for(i in unique(studies_rob$path.contrastinflow)){
        p <- sum(studies_rob[ which( studies_rob$path.contrastinflow == i ), ]$rob_p * studies_rob[ which( studies_rob$path.contrastinflow == i ), ]$study.cont / studies_rob[ which( studies_rob$path.contrastinflow == i ), ]$flowperedge)
        studies_rob$edge_p <- ifelse(studies_rob$path.contrastinflow==i, p, studies_rob$edge_p)
    }

    ### robustness of a path
    ### path_p: calculate the worked probability of a path
    studies_rob$path_p <- NA
    
    for(i in unique(studies_rob$path)){
        temp <- subset(studies_rob[ which( studies_rob$path == i ), ], select=c("path","path.contrastinflow","edge_p"))
        temp <- temp[!duplicated(temp), ]
        
        pp <- prod(temp$edge_p)
        studies_rob$path_p <- ifelse(studies_rob$path==i, pp, studies_rob$path_p)
        
    }
    
    
    ### output
    # edge drobustness for a single contrast
    robustness_df <- subset(studies_rob, select=c("path", "contrastinflow", "edge_p", "path_p"))
    robustness_df <- robustness_df[!duplicated(robustness_df), ]
    
    # path robustness for a single contrast
    pathrobust <- subset(studies_rob, select=c("path", "path_p", "flow"))
    pathrobust <- pathrobust[!duplicated(pathrobust), ]
    
    # network rubustness for a single contrast
    network_robustness.contrast <- as.matrix(sum(pathrobust$path_p * pathrobust$flow))
    colnames(network_robustness.contrast)[1] <- comparison
    rownames(network_robustness.contrast)[1] <- "network_robustness.contrast"
    
    return(list(allvariables=studies_rob, 
                robustness_df=robustness_df, 
                pathrobust=pathrobust, 
                network_robustness.contrast=network_robustness.contrast
                )
           )

}


### get the robustness for each contrast estimate

get.contrast_robust <- function(indata, hatmatrix){
    
    ####### each edge
    comparisons <- hatmatrix$rowNames
    
    contrast_robust_df <- data.frame()
    for(c in comparisons){
        robust_df <- get.robustness(indata, hatmatrix, c)
        temp <- t(robust_df$network_robustness.contrast)
        # mean stream length of a contrast estimate
        temp <- cbind(temp, nrow(robust_df$robustness_df) / nrow(robust_df$pathrobust))
        # numer of streams in a contrast estimate
        temp <- cbind(temp, nrow(robust_df$pathrobust))

        contrast_robust_df <- rbind(contrast_robust_df, temp)
    }
    output_df <- data.frame(contrast=rownames(contrast_robust_df), 
                            contrast_robust=contrast_robust_df$network_robustness.contrast, 
                            meanStreamLen=contrast_robust_df$V2,
                            NumberStream=contrast_robust_df$V3)
    
    return(output_df)
}

### get the overall robustness for the network
get.network_robust <- function(indata,hatmatrix){mean(get.contrast_robust(indata, hatmatrix)$contrast_robust)}
