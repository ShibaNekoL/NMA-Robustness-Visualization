Sys.setenv(LANG = "en")
rm(list=ls())

setwd("D:/OneDrive - 國立台灣大學/110 科技部大專生/R/flow_contribution/")


library(parallel)
library(devtools)
# install_github("esm-ispm-unibe-ch/dataformatter")
# install_github("esm-ispm-unibe-ch/nmadata",force = TRUE)
# install_github("esm-ispm-unibe-ch/flow_contribution")
source("D:/OneDrive - 國立台灣大學/110 科技部大專生/R/flow_contribution/R/streamstatistics.R")
source("D:/OneDrive - 國立台灣大學/110 科技部大專生/R/flow_contribution/R/hatmatrix.R")
library(dataformatter)
library(contribution)
library(nmadb)

##### 原本的資料
# #indata = read.csv("./tests/ms.csv")
# indata = read.csv2("./tests/griselda/griselda.csv")
# hatmat = getHatMatrix(indata,type="long_binary",model="random",sm="OR")
# 
# ###############
# dim(hatmat$H)
# c(levels(factor(indata$t)))
# 
# 
# indata$txt <- as.factor(indata$t)
# 
# data_wide <- pairwise(treat=t, event=r, n=n, studlab=id, data=indata, sm="OR")
# 
# data_wide$pair <- paste(data_wide$txt1, "-", data_wide$txt2)
# 
# 
# # Define order of treatments in printouts and forest plots
# trts <- c(levels(factor(indata$t)))
# 
# # Conduct network meta-analysis
# net1 <- netmeta(data_wide,reference.group = "Agomelatine",
#                 sm = "OR", comb.fixed = FALSE, comb.random = TRUE,
#                 backtransf=TRUE,seq = trts,nchar.trts = 38)
# 
# netgraph(net1)
# ############### 


#### 我的測試資料
data_long <- read.csv("D:/OneDrive - 國立台灣大學/110 科技部大專生/Notes/Sample/Data for day 2 - copy.csv")

# Transform data from long arm-based format to contrast-based format
# Argument 'sm' has to be used for odds ratio as summary measure; by
# default the risk ratio is used in the metabin function called
# internally.

data_long$txt <- as.factor(data_long$treatment)

names(data_long)[1] <- 'id'

data_wide <- pairwise(treat=txt, event=r, n=n, studlab=id, data=data_long, sm="OR")

data_wide$pair <- paste(data_wide$txt1, "-", data_wide$txt2)

# Define order of treatments in printouts and forest plots
trts <- c("A","B","C","D","E")

# Conduct network meta-analysis
net1 <- netmeta(data_wide,reference.group = "A",
                sm = "OR", comb.fixed = FALSE, comb.random = TRUE,
                backtransf=TRUE,seq = trts,nchar.trts = 38)

# network plot
netgraph(net1)

hatmat = getHatMatrix(data_long,type="long_binary",model="random",sm="OR")

hatmat$H

##################
stats = streamStatistics(hatmat)

stats$lengthfrequency2

hist=data.frame(contribution = 
    cbind(mapply(function(c){return(toString(c$comparison))},(unlist(stats$lengthfrequency2,recursive=F)))), 
  len = 
    cbind(mapply(function(c){return(c$length)},(unlist(stats$lengthfrequency2,recursive=F)))) 
  )

#get first order loops
fol = hist[hist$len==2,]
hs = table(fol)
hist(hs)

hist(hs, breaks=seq(min(hs)-0.5, max(hs)+0.5, by=1)  )

plot(stats$cummulativeContributionPerStream)
