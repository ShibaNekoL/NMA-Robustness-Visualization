rm(list=ls())
source("./R/hatmatrix.R")
source("./R/nmadbhatmatrix.R")
source("./R/forestplot.R")
library(devtools)
install_github("esm-ispm-unibe-ch/dataformatter")

#install.packages("../nmadata_1.0.tar.gz",repos=NULL)
install_github("esm-ispm-unibe-ch/nmadata")
library(nmadata)

#install_github("esm-ispm-unibe-ch/flow_contribution")
#library(contribution)

listVerified()

#testhm = getHatmatrixFromDB(482734, model="random",sm="OR")
indata = read.csv("tests/Senn2013.csv")
testhm = getHatMatrix(indata=indata,model="random",type="iv",sm="SMD")

forestcinema(testhm$NMAresults,0.24,"imprecision",testhm$sm)
forestcinema(testhm$NMAresults,0.24,"heterogeneity",testhm$sm)

