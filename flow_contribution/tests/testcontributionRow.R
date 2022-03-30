Sys.setenv(LANG = "en")
rm(list=ls())
library("devtools")
#install_version("netmeta",version="0.9-6")
install.packages("netmeta")
#install_github("esm-ispm-unibe-ch/flow_contribution")
#library(contribution)
source("../R/hatmatrix.R")
source("../R/contributionrow.R")
#indata = read.csv2("binary.csv",header=TRUE)
 #indata = read.csv2("long_conti.csv",header=TRUE,sep=";")
 indata = read.csv2("long_bin_pain.csv",header=TRUE,sep=";")
 
#indata = read.csv("Macfayeden.csv",header=TRUE,sep=";")
#indata = read.csv("diabetes_indr.csv",header=TRUE,sep=";")

C = getHatMatrix(indata,type="long_binary",model="random",sm="OR")
# Direct effects
rownames = C$rowNames
print(rownames)
fstcomp = rownames[1]
cm = getComparisonContribution(C,fstcomp)
sum(cm$contribution)
#md = -C$hatMatrix$Pairwise[,1]
#newnmd = lapply(names(md), function(n){gsub(" vs ",":",n)})
#names(md) = newnmd
#md = md[order(names(md))]

##network effects
#mn = C$hatMatrix$NMA[,1]
##hatmatrix
#HM = C$hatMatrix$H2bu
#nmn = HM %*% md

##Check if Contribution matrix * direct effectes is 100
#CM = C$contributionMatrix
#ch2 = (CM * sign(HM)) %*% md
#test2 = all(lapply((ch2), function(n){n == 100}))
#print("Check if Contribution matrix * direct effectes is 100")
#print(test2)

##Check if hatmatrix and contribution matrix give the same values for the direct
##comparisons
#print("Check if Contribution matrix and Hat matrix have the same directs (diagonals)")
#colnames(HM) = C$hatMatrix$colNames
#CMdiag = unlist(lapply(colnames(CM), function(c){ CM[c,c]}))
#HMdiag = unlist(lapply(colnames(HM), function(c){ HM[c,c]}))
#print(all.equal(HMdiag,(CMdiag / 100)))

