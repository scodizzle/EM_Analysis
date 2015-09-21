#### TDS #######
## I will use the normalized data for the parameter extraction
## norm.ready has the normalized 


library(agricolae)
library(candisc)
library(ggplot2)
library(grid)
library(splines)

setwd("~/Dropbox/Frost_Research/Extended Macer Wines/EM_Analysis/")
save(list=c("norm.ready", "normFrame.wine", "tdsNORMall", "tdsExtNORM"), file="tds.Rdata")
# norm.ready is by fermRep or bottle, there are 27 all normalized 
# norFrame.wine is by treatment there are 9 all normalized
# created from the code Greg wrote, under emNORMcode.R
# the function tdsEXTNORM -- extractes curve parameters for ANOVA and other evalation....  
load("tds.Rdata")

# extract curve parameters using the tdsExtNORM f(X)  
emExt.Bitter<- lapply(norm.ready, tdsExtNORM, att="Bitter", sigLine=0.297, df=18)
Bitter <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Bitter[[i]]$ExtParm))
Bitter$wine <- names(emExt.Bitter)
row.names(Bitter) = 1:27
Bitter$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Bitter$wine, "_")[[i]][1]))
Bitter$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Bitter$wine, "_")[[i]][2]))
Bitter.lm <- lm(as.matrix(Bitter[,c(1:5)]) ~ trt + fermRep, data = Bitter)
summary.aov(Bitter.lm) 
#area, maxDom, dur
Bitter.area <- LSD.test(lm(area~trt+fermRep, data=Bitter), trt="trt", group=TRUE)
Bitter.maxDom <- LSD.test(lm(maxDom~trt+fermRep, data=Bitter), trt="trt", group=TRUE)
Bitter.dur <- LSD.test(lm(dur~trt+fermRep, data=Bitter), trt="trt", group=TRUE)
#LSD table
Bitter.LSD <- cbind.data.frame(
Bitter.area$groups[order(Bitter.area$groups$trt),],
Bitter.maxDom$groups[order(Bitter.maxDom$groups$trt),],
Bitter.dur$groups[order(Bitter.dur$groups$trt),])
colnames(Bitter.LSD) = c("trt1", "area", "gp1", "trt2", "MaxDom","gp2", "trt3", "dur", "gp3")
Bitter.LSD

emExt.Hot<- lapply(norm.ready, tdsExtNORM, att="Hot", sigLine=0.297, df=18)
Hot<- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Hot[[i]]$ExtParm))
Hot$wine <- names(emExt.Hot)
row.names(Hot) = 1:27
Hot$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Hot$wine, "_")[[i]][1]))
Hot$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Hot$wine, "_")[[i]][2]))
Hot.lm <- lm(as.matrix(Hot[,c(1:5)]) ~ trt + fermRep, data = Hot)
summary.aov(Hot.lm) 
# No Sig for Hot

emExt.Sweet<- lapply(norm.ready, tdsExtNORM, att="Sweet", sigLine=0.297, df=18)
Sweet <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Sweet[[i]]$ExtParm))
Sweet$wine <- names(emExt.Sweet)
row.names(Sweet) = 1:27
Sweet$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Sweet$wine, "_")[[i]][1]))
Sweet$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Sweet$wine, "_")[[i]][2]))
Sweet.lm <- lm(as.matrix(Sweet[,c(1:5)]) ~ trt + fermRep, data = Sweet)
summary.aov(Sweet.lm) 
# No Sig for Sweet


emExt.Sour<- lapply(norm.ready, tdsExtNORM, att="Sour", sigLine=0.297, df=18)
Sour <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Sour[[i]]$ExtParm))
Sour$wine <- names(emExt.Sour)
row.names(Sour) = 1:27
Sour$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Sour$wine, "_")[[i]][1]))
Sour$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Sour$wine, "_")[[i]][2]))
Sour.lm <- lm(as.matrix(Sour[,c(1:5)]) ~ trt + fermRep, data = Sour)
summary.aov(Sour.lm) 
# No significant difference for Sour


emExt.Ast<- lapply(norm.ready, tdsExtNORM, att="Astringent", sigLine=0.297, df=18)
Ast <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Ast[[i]]$ExtParm))
Ast$wine <- names(emExt.Ast)
row.names(Ast) = 1:27
Ast$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Ast$wine, "_")[[i]][1]))
Ast$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Ast$wine, "_")[[i]][2]))
Ast.lm <- lm(as.matrix(Ast[,c(1:5)]) ~ trt + fermRep, data = Ast)
summary.aov(Ast.lm) 


##### 
##some plots

ggplot(tdsNORMall[tdsNORMall$sensation == "Bitter",], aes(x=time, y=domRate)) +
  geom_smooth(aes(color=wine), method="lm", formula= y~ns(x,18), se=FALSE, guide=TRUE)
# the plot is busy...

PD <- preplotTDS(rbind.data.frame(normFrame.wine$PD_A, normFrame.wine$PD_B, normFrame.wine$PD_C))
PD$wine <- rep("PD", 600)
PD$bottle <- rep()
row.names(PD) = 1:600

POC <- preplotTDS(rbind.data.frame(normFrame.wine$POC_A, normFrame.wine$POC_B, normFrame.wine$POC_C))
POC$wine <- rep("POC", 600) 
row.names(POC) = 1:600

SUB <- preplotTDS(rbind.data.frame(normFrame.wine$SUB_A, normFrame.wine$SUB_B, normFrame.wine$SUB_C))
SUB$wine <- rep("SUB", 600) 
row.names(SUB) = 1:600

PO1 <- preplotTDS(rbind.data.frame(normFrame.wine$PO1_A, normFrame.wine$PO1_B, normFrame.wine$PO1_C))
PO1$wine <- rep("PO1", 600) 
row.names(PO1) = 1:600

PO2 <- preplotTDS(rbind.data.frame(normFrame.wine$PO2_A, normFrame.wine$PO2_B, normFrame.wine$PO2_C))
PO2$wine <- rep("PO2", 600) 
row.names(PO2) = 1:600

PO4 <- preplotTDS(rbind.data.frame(normFrame.wine$PO4_A, normFrame.wine$PO4_B, normFrame.wine$PO4_C))
PO4$wine <- rep("PO4", 600) 
row.names(PO4) = 1:600

PO6 <- preplotTDS(rbind.data.frame(normFrame.wine$PO6_A, normFrame.wine$PO6_B, normFrame.wine$PO6_C))
PO6$wine <- rep("PO6", 600) 
row.names(PO6) = 1:600

PO8 <- preplotTDS(rbind.data.frame(normFrame.wine$PO8_A, normFrame.wine$PO8_B, normFrame.wine$PO8_C))
PO8$wine <- rep("PO8", 600) 
row.names(PO8) = 1:600

S8 <- preplotTDS(rbind.data.frame(normFrame.wine$S8_A, normFrame.wine$S8_B, normFrame.wine$S8_C))
S8$wine <- rep("S8", 600) 
row.names(S8) = 1:600
tdsNORMall <- rbind.data.frame(PD, POC, SUB, PO1, PO2, PO4, PO6, PO8, S8)
rm(PD, POC, SUB, PO1, PO2, PO4, PO6, PO8, S8)
