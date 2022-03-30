rm(list=ls())
source("./R/hatmatrix.R")
source("./R/nmadbhatmatrix.R")
source("./R/contributionrow.R")
source("./R/studycontribution.R")
library(readr)
library(devtools)
install_github("esm-ispm-unibe-ch/dataformatter")
install_github("esm-ispm-unibe-ch/nmadata")
#install.packages("../nmadata_1.0.tar.gz",repos=NULL)
#install_github("esm-ispm-unibe-ch/flow_contribution")
library(nmadata)
#library(contribution)

listVerified()

#indata = read.csv("tests/Senn2013.csv")
#testhm = getHatMatrix(indata=indata,model="random",type="iv",sm="OR")
#comparison="metf:plac"

testhm = getHatmatrixFromDB(482734, model="random",sm="OR")$hm
comparison = "2:3"

result = getStudyContribution(testhm, comparison)
print(result)

print("contributions sum to 100")
print(all.equal(sum(result$contribution),100))
