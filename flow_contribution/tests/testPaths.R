Sys.setenv(LANG = "en")
rm(list=ls())

library(parallel)
library(devtools)
library(jsonlite)
#install_github("esm-ispm-unibe-ch/flow_contribution",repos="simulations")
install_github("esm-ispm-unibe-ch/nmadata")
#install.packages("../../nmadata_1.0.tar.gz",repos=NULL)
#install.packages("../../../contribution_0.1.0.tar.gz",repos=NULL)

source("~/Documents/flow_contribution/R/hatmatrix.R")
source("~/Documents/flow_contribution/R/contributionmatrix.R")
#source("~/Documents/flow_contribution/R/contributionrow.R")
source("~/Documents/flow_contribution/R/streamstatistics2.R")
source("~/Documents/flow_contribution/R/randomcontribution.R")
#source("~/Documents/flow_contribution/R/streamstatistics.R")
#library(contribution)
library(nmadata)
library(netmeta)

hatmatrix = function(filename="diabetes_indr",model="random",sm="OR"){
  indata = readnma(filename)
  C = getHatMatrix (indata$data,type=longType(indata),model,sm,tau="NA")
  return(C)
}

netplot = function(filename="diabetes_indr"){
  C = hatmatrix(filename,model="random",sm="OR")
}

shortestRandomDiffs = function(filename="diabetes_indr"){
  indata = readnma(filename)
  C = getContributionMatrix(indata$data,type=longType(indata),model="random",sm="OR")
  print(c("calculated matrix"))
# Direct effects
#md = -C$hatMatrix$Pairwise[,1]
  md = C$hatMatrix$colNames
#md = md[order(names(md))]
#hatmatrix
  HM = C$hatMatrix$H
  allComparisons = rownames(HM)
  CM = C$contributionMatrix

  CS = lapply(allComparisons, function(comp){
                comparisonStreams(C$hatMatrix,comparison=comp)
  })

  CR = lapply(allComparisons, function(comp){
                getRandomComparisonContribution(C$hatMatrix,comparison=comp)
  })

  rsums = lapply(CR, function(comp){
                sum(comp$contribution)
  })

  ssums = lapply(CS, function(comp){
                sum(comp$contribution)
  })

#diffs = abs(as.data.frame(CS)-as.data.frame(CR))
  diffs = lapply(1:length(CR),function(row){CR[[eval(row)]]$contribution-CS[[eval(row)]]$contribution})
#diffs = lapply(CR, function(cont){print(cont$rcontr);return(cont$contribution-cont$rcontr)})
  filterDiffs = function (diffs, thres) {
    lapply(diffs, function(diff){diff[diff>thres]})
  }
  fods =
    function(diffs, thres) {
    allDiffs = length(md) * length(CR)
    return (sum(
      lapply(filterDiffs(diffs, thres), function(d){length(d)}) %>% unlist()
    )*100/allDiffs)
  }

  biggestDiff = function(diffs){
    xs = seq(0.01,100,0.01)
    return(Reduce(function(out, x){
                    o = 0
                    if(fods(diffs,x)>0){
                      o=x
                    }else{
                      o=out
                    }
                    return(o)
    },xs,0))
  }
  print("equivelance threshold")
  print(biggestDiff(diffs))
  xs = seq(0.01,1.5,0.01)
  ys = lapply(xs,function(x){fods(diffs,x)})
  print("plotting diffs")
  svg(filename=paste(filename,"diffs.svg",sep="-"), 
      width=5, 
      height=4, 
      pointsize=12)
  plot(xs,ys)
  dev.off()

#show streams in random algorithm
  writeLines(toJSON(CR[[1]]$streams),paste(filename,"randomstreams.json",sep="-"))
  writeLines(toJSON(CS[[1]]$streams),paste(filename,"shorteststreams.json",sep="-"))
}

