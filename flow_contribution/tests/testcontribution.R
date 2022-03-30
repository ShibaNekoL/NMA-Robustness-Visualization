Sys.setenv(LANG = "en")
library("devtools")
install_version("netmeta",version="0.9-6")
#install_github("esm-ispm-unibe-ch/flow_contribution")
source("../R/hatmatrix.R")
#library(contribution)
rm(list=ls())
#indata = read.csv2("binary.csv",header=TRUE)
 indata = read.csv2("diabetes_new.csv",header=TRUE,sep=";")
 
#indata = read.csv("Macfayeden.csv",header=TRUE,sep=";")
#indata = read.csv("diabetes_indr.csv",header=TRUE,sep=";")

C = getContributionMatrix(indata,type="netwide_binary",model="random",sm="OR")
# Direct effects
md = -C$hatMatrix$Pairwise[,1]
newnmd = lapply(names(md), function(n){gsub(" vs ",":",n)})
names(md) = newnmd
md = md[order(names(md))]

#network effects
mn = C$hatMatrix$NMA[,1]
#hatmatrix
HM = C$hatMatrix$H2bu
nmn = HM %*% md

#Check if Contribution matrix * direct effectes is 100
CM = C$contributionMatrix
ch2 = (CM * sign(HM)) %*% md
test2 = all(lapply((ch2), function(n){n == 100}))
print("Check if Contribution matrix * direct effectes is 100")
print(test2)

#Check if hatmatrix and contribution matrix give the same values for the direct
#comparisons
print("Check if Contribution matrix and Hat matrix have the same directs (diagonals)")
colnames(HM) = C$hatMatrix$colNames
CMdiag = unlist(lapply(colnames(CM), function(c){ CM[c,c]}))
HMdiag = unlist(lapply(colnames(HM), function(c){ HM[c,c]}))
print(all.equal(HMdiag,(CMdiag / 100)))

