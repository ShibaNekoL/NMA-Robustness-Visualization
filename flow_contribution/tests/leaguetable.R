rm(list=ls())
source("./R/hatmatrix.R")
source("./R/nmadbhatmatrix.R")
source("./R/leaguetable.R")
library(readr)
library(devtools)
install_github("esm-ispm-unibe-ch/dataformatter")

install_github("esm-ispm-unibe-ch/nmadata")
#install.packages("../nmadata_1.0.tar.gz",repos=NULL)
library(nmadata)
#listVerified()

#install_github("esm-ispm-unibe-ch/flow_contribution")
#library(contribution)

#testhm = getHatmatrixFromDB(482734, model="random",sm="OR")

#indata = read.csv("tests/Senn2013.csv")
#testhm = getHatMatrix(indata=indata,model="fixed",type="iv",sm="MD")
indata = read.csv2("tests/diabetes_indr.csv")
testhm = getHatMatrix(indata=wide2long(indata),model="fixed",type="long_binary",sm="OR")

res = leaguetable(testhm$forleaguetable,testhm$model,testhm$sm)
print(res)

