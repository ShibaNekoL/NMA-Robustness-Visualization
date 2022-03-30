Sys.setenv(LANG = "en")
rm(list=ls())
library("devtools")
#install_github("esm-ispm-unibe-ch/flow_contribution")
#install_github("esm-ispm-unibe-ch/nmadata")
#library(nmadata)
source("R/hatmatrix.R");
source("R/contributionrow.R");
library(contribution)
library(reshape2)
library(jsonlite)

hatmatrixToJSON = function (filename="diabetes_indr") {
  indata = readnma(filename)

  Res = getHatMatrix(indata$data,type=longType(indata),model="random",sm="OR")

  hm = melt(Res$H, varnames = c("row","comparison"))

  jhm = toJSON(hm)
  outname = paste(filename,"hat.json",sep="")
  write(jhm,outname)
#write.csv(Res$H,"diabhat.csv")
  print(jhm)
}

#hatmatrixToJSON()
indata = read.csv("tests/NMA_Max_Effic_indirect_problems.csv",header=TRUE,sep=",")
#indata = read.csv("tests/NMA_Max_Effic_indirect.csv",header=TRUE,sep=",")

C = getHatMatrix(indata,type="long_continuous",model="random",sm="SMD")

print("the hat matrix is:")
print(C)


# Direct effects
#md = C$hatMatrix$Pairwise[,1]
#mdsigns = lapply(names(md), function(n){if(strsplit(n," vs ")[[1]][1]<strsplit(n," vs ")[[1]][2]){+1}else{-1}})
#mdsigns = -1 * unlist(mdsigns)
#newnmd = lapply(names(md), function(n){paste(sort(unlist(strsplit(n," vs "))),collapse=":")})
#names(md) = newnmd
#md = md * mdsigns
#md = md[order(names(md))]

##network effects
##hatmatrix
#HM = C$hatMatrix$H2bu
#nmn = HM %*% md
#nmn = unlist(nmn[,1])

#mn = C$hatMatrix$NMA[,1]

 ##test that hmatrix works
#print("hatmatrix gives correct network measures")
#print(all.equal(mn,nmn))

















