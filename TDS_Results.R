#### TDS #######
## I will use the normalized data for the parameter extraction
## norm.ready has the normalized 
setwd("~/Dropbox/Frost_Research/Extended Macer Wines/EM_Analysis/")
save(list=c("norm.ready","tdsExtNORM"), file="tds.Rdata")
load("tds.Rdata")

emExt <- lapply(norm.ready, tdsExtNORM, att="Hot", sigLine=0.297)
do.call(rbind, lapply(1:27, function(i) emExt[[i]]$ExtParm))







