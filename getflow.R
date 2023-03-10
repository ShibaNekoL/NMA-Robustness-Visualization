##### get.streams_df

get.streams_df <- function(indata, hatmatrix, comparison){
    
    # source("./flow_contribution/R/hatmatrix.R")
    # source("./flow_contribution/R/streamStatistics2.R")
    # source("./flow_contribution/R/studycontribution.R")
    # source("./flow_contribution/R/contributionrow.R")
    
    ### Get the return list modified by Man-fang Liang in "streamStatistics2.R"
    
    # The list contains: 
    # "comp" (the interested comparison),
    # "streams" (streams of the interested comparison), 
    # "contribution" (contribution of direct evidences for the interested comparison)
    streamsstat <- comparisonStreams(hatmatrix, comparison)
    
    ### streamsstat_df is a dataframe that contains the contents (columns) of each stream (rows)
    streamsstat_df <- data.frame()
    
    
    ### convert the list streamsstat to dataframe streamsstat_df
    
    # Read the contents from comparisonStreams() in "streamStatistics2.R" to 
    for(i in 1:length(streamsstat$streams)){
        stream_temp_df <- 0
        stream_temp_df <- data.frame(comparison=streamsstat$comp,
                                     path=paste(streamsstat$streams[[i]]$path, collapse = ":"),
                                     flow=streamsstat$streams[[i]]$stream$flow,
                                     length=streamsstat$streams[[i]]$stream$length,
                                     flowperedge=streamsstat$streams[[i]]$stream$flowperedge*100,
                                     edgehead=streamsstat$streams[[i]]$edge$head,
                                     edgetail=streamsstat$streams[[i]]$edge$tail
        )
        streamsstat_df <- rbind(streamsstat_df, stream_temp_df)
    }
    # rename path
    streamsstat_df$path <- unlist(lapply(streamsstat_df$path, 
                                         function(i){ paste0( unlist(unique( strsplit(i, ":")[[1]] )), 
                                                              collapse = ":" 
                                                              ) } ))
    
    ### contrastinflow: a new column with combind edgehead & edgetail 
    
    # assign the order of the combinations of each t (treatment) C(number of treatments, 2)
    # eg. A:B instead of B:A
    order <- paste0(t(combn(levels(factor(indata$t)),2))[,1],":",t(combn(levels(factor(indata$t)),2))[,2])
    
    # delete the combinations in wrong order
    streamsstat_df$headtail <- paste0(streamsstat_df$edgehead,":",streamsstat_df$edgetail)
    streamsstat_df$tailhead <- paste0(streamsstat_df$edgetail,":",streamsstat_df$edgehead)
    
    streamsstat_df$headtail[streamsstat_df$tailhead %in% order] <- ""
    streamsstat_df$tailhead[streamsstat_df$headtail %in% order] <- ""
    
    # combine 2 column into one new column "contrastinflow"
    streamsstat_df$contrastinflow <- paste0(streamsstat_df$headtail,streamsstat_df$tailhead)
    
    # delete the temp columns "headtail" & "tailhead"
    streamsstat_df <- streamsstat_df[,c(-8,-9)]
    
    
    ### direct.cont: a new column of contribution of a contrast (pooled all of the flows)
    cont <- data.frame(contrastinflow=names(streamsstat$contribution), direct.cont=streamsstat$contribution)
    streamsstat_df <- merge(streamsstat_df, cont, by='contrastinflow', all.x=T)
    
    return(streamsstat_df)
}


##### get.studies_df

get.studies_df <- function(indata, hatmatrix, comparison){
    
    library(igraph)
    # source("./flow_contribution/R/hatmatrix.R")
    # source("./flow_contribution/R/streamStatistics2.R")
    # source("./flow_contribution/R/studycontribution.R")
    # source("./flow_contribution/R/contributionrow.R")
    
    ### read contents
    studiesstat <- getStudyContribution(hatmatrix, comparison)
    streams_df <- get.streams_df(indata, hatmatrix, comparison)
    
    
    ### StudyContributionPerFlow: a new dataframe merge with streams_df and studiesstat
    
    # all.x=T since the number of evidences of studies is larger than the contrastinflow in streams_df
    StudyContributionPerFlow <- data.frame(merge(streams_df,studiesstat$studyRow, by.x = 'contrastinflow', by.y = 'comparison',all.x=T))
    # change column names of the variables
    names(StudyContributionPerFlow)[c(1,11)] <- c("contrastinflow","aggr.contribution")
    # arrange the data by path & contrastinflow
    StudyContributionPerFlow <- arrange(StudyContributionPerFlow,path,contrastinflow)
    
    
    ### study.cont: a new column with the contribution of a direct evidence in a study in a flow
    
    # calculate: contribution of a direct evidence in a study * flow of an edge in a stream / total flow of a direct evidence in all streams
    StudyContributionPerFlow$study.cont <- StudyContributionPerFlow$aggr.contribution * StudyContributionPerFlow$flowperedge / StudyContributionPerFlow$direct.cont
    
    
    ### StudyReport: a new dataframe which is easier to read with necessary variable 
    StudyReport <- subset(StudyContributionPerFlow, select = c('path','edgehead','edgetail','study','study.cont'))
    
    
    output <- list(StudyContributionPerFlow=StudyContributionPerFlow,
                   StudyReport=StudyReport
    )

    return(output)
}




