setwd('D:/OneDrive - 國立台灣大學/110 科技部大專生/Open resources/')

source("./flow_contribution/R/streamstatistics2.R")
source("./flow_contribution/R/hatmatrix.R")

source("./getflow.R")
source("./getflowplot.R")
source("./robustness.R")
source("./robustnetplot.R")
source("./contrast_robustnetplot.R")

source("./contrast_robustnetplot.R")



######

## data exmaple
indata <- read.csv("./example data/diabetes.csv", fileEncoding="UTF-8-BOM")
comparison <- 'ACE:Placebo'
comparison <- 'ACE:ARB'
hatmatrix <- getHatMatrix(indata, type="long_binary", model="random", sm='OR')

##
indata <- read.csv("./example data/hypocalcemia.csv", fileEncoding="UTF-8-BOM")
comparison <- 'Cinacalcet:Evocalcet'
hatmatrix <- getHatMatrix(indata, type="long_binary", model="random", sm='OR')
##

View(get.streams_df(hatmatrix, comparison))
View(get.studies_df(hatmatrix, comparison)$StudyContributionPerFlow)
View(get.studies_df(hatmatrix, comparison)$StudyReport)

getflowplot(indata, hatmatrix,comparison)

View(get.robustness(indata, hatmatrix,comparison)$allvariables)
View(get.robustness(indata, hatmatrix,comparison)$robustness_df)
View(get.robustness(indata, hatmatrix,comparison)$pathrobust)
get.robustness(indata, hatmatrix,comparison)$network_robustness.contrast

get.robustnetworkplot(indata, hatmatrix,comparison)
get.interactive.robustnetworkplot(indata, hatmatrix, comparison, highrisk = F)
get.interactive.robustnetworkplot(indata, hatmatrix, comparison, highrisk = T)

get.contrast_robust(indata, hatmatrix)
get.network_robust(indata, hatmatrix)

get.contrast_robust_plot(indata, hatmatrix)
get.interactive.contrast_robust_plot(indata, hatmatrix, highrisk = F)
get.interactive.contrast_robust_plot(indata, hatmatrix, highrisk = T)


##################### 

### to see how the results my function calculates are different from the proportion of rob
proportion.ROB <- get.studies_rob(indata, hatmatrix, comparison)
proportion.ROB$rob_q <- ifelse(proportion.ROB$rob==1, 0.1, ifelse(proportion.ROB$rob==2, 0.5, ifelse(proportion.ROB$rob==3, 0.9, NA)))
proportion.ROB$rob_p <- 1 - proportion.ROB$rob_q

# calculate the proportion of rob for each path
path_flow.df <- unique(subset(proportion.ROB, select=c("path","flow")))

proportion.ROB$pro.rob <- (proportion.ROB$rob_p / 100) * (proportion.ROB$study.cont / proportion.ROB$flow)
proportion.ROB <- aggregate( subset( proportion.ROB, select=c("pro.rob") ), by=list(proportion.ROB$path), FUN=sum)
names(proportion.ROB)[1] <- "path"

# use our function
r <- get.robustness(indata, hatmatrix ,comparison)$pathrobust
compare.robustness <- merge(r, proportion.ROB, by=c("path"))

# visualize the difference
View(compare.robustness)
ggplot(compare.robustness, aes(x=pro.rob, y=path_p)) + geom_point(size=3) +
    geom_smooth(method=lm, lwd=2) + 
    geom_abline(color="red", lwd=2) +
    xlim(c(0,1)) + ylim(c(0,1)) +
    xlab("Proportion of ROB of a stream") + ylab("Robustness index of a stream")


###
### calculate the proportion of rob for the comparison
proportion.ROB <- merge(proportion.ROB, path_flow.df, by=c("path"), all.X=T)
proportion.ROB_contrast_network <- sum(proportion.ROB$pro.rob * proportion.ROB$flow)
proportion.ROB_contrast_network


###