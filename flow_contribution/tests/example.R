library(readr)
#TODO: switch to nmadata
diabetes <- read_delim("./diabetes.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)

NMA_Max <- read_csv("./NMA_Max.csv")

#example binary
gh1<-getHatMatrix(indata <- diabetes,type="long_binary",model="fixed",tau=NA, sm="OR")

forestcinema(gethatresult=gh1$NMAresults,civ=1.05,domain="imprecision",sm="OR")
forestcinema(gethatresult=gh1$NMAresults,civ=0.9,domain="heterogeneity",sm="OR")

leaguetable(gethatresult=gh1$netmetaObject, model="fixed",sm="OR")
leaguetable(gethatresult=gh1$netmetaObject, model="random",sm="OR")

#example continuous
NMA_Max=as.data.frame(NMA_Max)
gh2<-getHatMatrix(indata <- NMA_Max,type="long_continuous",model="fixed",tau=NA, sm="SMD")

forestcinema(gethatresult=gh3$NMAresults,civ=0.3,domain="imprecision",sm="SMD")
forestcinema(gethatresult=gh3$NMAresults,civ=-0.1,domain="heterogeneity",sm="SMD")

leaguetable(gethatresult=gh3$netmetaObject, model="fixed",sm="OR")
leaguetable(gethatresult=gh3$netmetaObject, model="random",sm="OR")
