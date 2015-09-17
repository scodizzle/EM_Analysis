#### TDS #######
## I will use the normalized data for the parameter extraction
## norm.ready has the normalized 
setwd("~/Dropbox/Frost_Research/Extended Macer Wines/EM_Analysis/")
save(list=c("norm.ready","tdsExtNORM"), file="tds.Rdata")
load("tds.Rdata")


emExt.Bitter<- lapply(norm.ready, tdsExtNORM, att="Bitter", sigLine=0.297)
Bitter <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Bitter[[i]]$ExtParm))
Bitter$wine <- names(emExt.Bitter)
row.names(Bitter) = 1:27
Bitter$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Bitter$wine, "_")[[i]][1]))
Bitter$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Bitter$wine, "_")[[i]][2]))
Hot.lm <- lm(as.matrix(Bitter[,c(1:5)]) ~ trt + fermRep, data = Bitter)
summary.aov(Hot.lm)
Hot.LSD <- LSD.test(Hot.lm, trt="trt", group=TRUE)






emExt.Hot<- lapply(norm.ready, tdsExtNORM, att="Hot", sigLine=0.297)
Hot<- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Hot[[i]]$ExtParm))
Hot$wine <- names(emExt.Hot)
row.names(Hot) = 1:27
Hot$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Hot$wine, "_")[[i]][1]))
Hot$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Hot$wine, "_")[[i]][2]))

emExt.Sweet<- lapply(norm.ready, tdsExtNORM, att="Sweet", sigLine=0.297)
Sweet <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Sweet[[i]]$ExtParm))
Sweet$wine <- names(emExt.Sweet)
row.names(Sweet) = 1:27
Sweet$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Sweet$wine, "_")[[i]][1]))
Sweet$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Sweet$wine, "_")[[i]][2]))

emExt.Sour<- lapply(norm.ready, tdsExtNORM, att="Sour", sigLine=0.297)
Sour <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Sour[[i]]$ExtParm))
Sour$wine <- names(emExt.Sour)
row.names(Sour) = 1:27
Sour$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Sour$wine, "_")[[i]][1]))
Sour$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Sour$wine, "_")[[i]][2]))

emExt.Ast<- lapply(norm.ready, tdsExtNORM, att="Astringent", sigLine=0.297)
Ast <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Ast[[i]]$ExtParm))
Ast$wine <- names(emExt.Ast)
row.names(Ast) = 1:27
Ast$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Ast$wine, "_")[[i]][1]))
Ast$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Ast$wine, "_")[[i]][2]))



