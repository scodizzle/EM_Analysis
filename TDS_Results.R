#### TDS #######
## I will use the normalized data for the parameter extraction
## norm.ready has the normalized 


library(agricolae)
library(candisc)
library(ggplot2)
library(grid)
library(splines)

setwd("~/Dropbox/Frost_Research/Extended Macer Wines/EM_Analysis/")
save(list=c("norm.ready", "normFrame.wine", "tdsNORMall", "tdsExtNORM", "preplotTDS", "tdsAll", "tdsAll.t"), file="tds.Rdata")
# norm.ready is by fermRep or bottle, there are 27 all normalized 
# norFrame.wine is by treatment there are 9 all normalized
# created from the code Greg wrote, under emNORMcode.R
# the function tdsEXTNORM -- extractes curve parameters for ANOVA and other evalation....  
load("tds.Rdata")

# extract curve parameters using the tdsExtNORM f(X) 
## Bitter
emExt.Bitter<- lapply(norm.ready, tdsExtNORM, att="Bitter", sigLine=0.297, df=5)
Bitter <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Bitter[[i]]$ExtParm))
Bitter$wine <- names(emExt.Bitter)
row.names(Bitter) = 1:27
Bitter$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Bitter$wine, "_")[[i]][1]))
Bitter$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Bitter$wine, "_")[[i]][2]))
Bitter$sensation <- rep("Bitter", nrow(Bitter))
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

cbind.data.frame(
  Bitter.area$statistics[4],
  Bitter.maxDom$statistics[4],
  Bitter.dur$statistics[4])

## HOT
emExt.Hot<- lapply(norm.ready, tdsExtNORM, att="Hot", sigLine=0.297, df=5)
Hot<- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Hot[[i]]$ExtParm))
Hot$wine <- names(emExt.Hot)
row.names(Hot) = 1:27
Hot$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Hot$wine, "_")[[i]][1]))
Hot$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Hot$wine, "_")[[i]][2]))
Hot$sensation <- rep("Hot", nrow(Hot))
Hot.lm <- lm(as.matrix(Hot[,c(1:5)]) ~ trt + fermRep, data = Hot)
summary.aov(Hot.lm) 
# Hot duration
Hot.dur <- LSD.test(lm(dur~trt+fermRep, data=Hot), trt="trt", group=TRUE)
Hot.dur
Hot.dur$statistics[4]

## Sweet
emExt.Sweet<- lapply(norm.ready, tdsExtNORM, att="Sweet", sigLine=0.297, df=5)
Sweet <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Sweet[[i]]$ExtParm))
Sweet$wine <- names(emExt.Sweet)
row.names(Sweet) = 1:27
Sweet$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Sweet$wine, "_")[[i]][1]))
Sweet$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Sweet$wine, "_")[[i]][2]))
Sweet$sensation <- rep("Sweet", nrow(Sweet))
Sweet.lm <- lm(as.matrix(Sweet[,c(1:5)]) ~ trt + fermRep, data = Sweet)
summary.aov(Sweet.lm) 
# No Sig for Sweet

## Sour
emExt.Sour<- lapply(norm.ready, tdsExtNORM, att="Sour", sigLine=0.297, df=5)
Sour <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Sour[[i]]$ExtParm))
Sour$wine <- names(emExt.Sour)
row.names(Sour) = 1:27
Sour$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Sour$wine, "_")[[i]][1]))
Sour$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Sour$wine, "_")[[i]][2]))
Sour$sensation <- rep("Sour", nrow(Sour))
Sour.lm <- lm(as.matrix(Sour[,c(1:5)]) ~ trt + fermRep, data = Sour)
summary.aov(Sour.lm) 
# No significant difference for Sour

## Astringent
emExt.Ast<- lapply(norm.ready, tdsExtNORM, att="Astringent", sigLine=0.297, df=5)
Ast <- do.call(rbind.data.frame, lapply(1:27, function(i) emExt.Ast[[i]]$ExtParm))
Ast$wine <- names(emExt.Ast)
row.names(Ast) = 1:27
Ast$trt <- unlist(lapply(1:27, FUN = function(i) strsplit(Ast$wine, "_")[[i]][1]))
Ast$fermRep <- unlist(lapply(1:27, FUN = function(i) strsplit(Ast$wine, "_")[[i]][2]))
Ast$sensation <- rep("Astringent", nrow(Ast))
Ast.lm <- lm(as.matrix(Ast[,c(1:5)]) ~ trt + fermRep, data = Ast)
summary.aov(Ast.lm) 
# No significant difference for Astringent

## bind up a tds ALL extracted parameter dataframe ##
tdsAll <- rbind(Sweet, Sour, Hot, Bitter, Ast)
tdsAll$wine <- factor(tdsAll$wine)
tdsAll$trt <- factor(tdsAll$trt)
tdsAll$sensation <- factor(tdsAll$sensation)
tdsAll$fermRep <- factor(tdsAll$fermRep)

## this "tips" the tdsAll over to reorganize it, as tdsAll is a df of the extracted paramters from the
## 
    tdsAll.t <- cbind(tdsAll[tdsAll$sensation == "Bitter",], tdsAll[tdsAll$sensation == "Astringent",], 
                  tdsAll[tdsAll$sensation == "Sweet",], tdsAll[tdsAll$sensation == "Sour",], 
                  tdsAll[tdsAll$sensation == "Hot",])
    colnames(tdsAll.t) <- c(
      "B.area","B.maxDom","B.tAmax","B.tmax","B.dur","wine","trt","fermRep", "sensation",
      "A.area","A.maxDom","A.tAmax","A.tmax","A.dur","wine","trt","fermRep", "sensation",
      "Sw.area","Sw.maxDom","Sw.tAmax","Sw.tmax","Sw.dur","wine","trt","fermRep", "sensation",
      "So.area","So.maxDom","So.tAmax","So.tmax","So.dur","wine","trt","fermRep", "sensation",
      "H.area","H.maxDom","H.tAmax","H.tmax","H.dur","wine","trt","fermRep", "sensation")
    tdsAll.t <- tdsAll.t[,-c(6:9,15:18,24:27,33:36, 45)]
    tdsAll.t <- cbind(tdsAll.t[,c(26:28)], tdsAll.t[,c(1:25)])
    
    #Note....  this gives the same results as the indivdual lm analysis above
    summary.aov(lm(as.matrix(tdsAll.t[,-c(1:3)]) ~ trt + fermRep, data = tdsAll.t))  

## this approach of nesting the sensation w/in the fermRep... 
manova(as.matrix(tdsAll.t[,-c(1:3)]) ~ trt + fermRep, data = tdsAll.t)#FAILS
summary.aov(lm(as.matrix(tdsAll[,c(1:5)]) ~ trt + fermRep + fermRep/sensation, data=tdsAll))


######################################################################################################
###### MANOVA of the TDS dataset
## start with the extracted dataset..  the tdsAll.t 
head(tdsAll.t)
# use the 4 extracted significant parameters for CVA
tdsSig <- manova(as.matrix(tdsAll.t[,c("B.area","B.maxDom","B.dur","H.dur")]) ~ trt, data=tdsAll.t)
    summary(tdsSig, test="Wilks") # SIGNIFICANT
tdsSigCVA <- candisc(tdsSig)
    plot(tdsSigCVA)
# using the tdsAll df for the manova
# all sensations
tds.m <- manova(as.matrix(tdsAll[,c(1:5)]) ~ trt +fermRep + fermRep/sensation, data = tdsAll)
  summary(tds.m, test="Wilks") # not significant
# just bitter and Astringent
tds.BA <- manova(as.matrix(
  tdsAll[tdsAll$sensation %in% c("Bitter","Astringent"),][,c(1:5)]) ~ trt + fermRep + fermRep/sensation, 
  data = tdsAll[tdsAll$sensation %in% c("Bitter","Astringent"),])
    summary(tds.BA, test="Wilks") # significant
tdsBA.CVA <- candisc(tds.BA)
  plot(tdsBA.CVA)

##### CVA plots
## tdsSig
ggplot(tdsSigCVA$means, aes(x=Can1, y=Can2, label=row.names(tdsSigCVA$means))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) +
  geom_segment(data=as.data.frame(tdsSigCVA$structure), aes(x=0, y=0, xend=Can1*2, yend=Can2*2, label=row.names(tdsSigCVA$structure)), 
               arrow=arrow(length=unit(0.3,"cm")), color="grey", size=1) +
  geom_text(data=as.data.frame(tdsSigCVA$structure), 
            aes(x=Can1*2, y=Can2*2, label=row.names(tdsSigCVA$structure)), family = "Times New Roman", fontface = "italic") +
  scale_x_continuous(paste("Can 1 ", "(", round(tdsSigCVA$pct[1],1), "%", ")", sep="")) +
  scale_y_continuous(paste("Can 2 ", "(", round(tdsSigCVA$pct[2],1), "%", ")", sep="")) +
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))
## tdsBA.CVA
ggplot(tdsBA.CVA$means, aes(x=Can1, y=Can2, label=row.names(tdsBA.CVA$means))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) +
  geom_segment(data=as.data.frame(tdsBA.CVA$structure), aes(x=0, y=0, xend=Can1*3, yend=Can2*3, label=row.names(tdsBA.CVA$structure)), 
               arrow=arrow(length=unit(0.3,"cm")), color="grey", size=1) +
  geom_text(data=as.data.frame(tdsBA.CVA$structure), 
            aes(x=Can1*3, y=Can2*3, label=row.names(tdsBA.CVA$structure)), family = "Times New Roman", fontface = "italic") +
  scale_x_continuous(paste("Can 1 ", "(", round(tdsBA.CVA$pct[1],1), "%", ")", sep="")) +
  scale_y_continuous(paste("Can 2 ", "(", round(tdsBA.CVA$pct[2],1), "%", ")", sep="")) +
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))

######################################################################################################
######################################################################################################
##### 
## some aditional plots
## create data for plotting...
######################################################################################################
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

## TDS but facet the plot by wine
## facet grid
ggplot(tdsNORMall[tdsNORMall$sensation %in% c("Bitter", "Astringent", "Sweet", "Sour", "Hot"),], aes(x=time, y=domRate)) +
         geom_smooth(method="lm", formula= y~ns(x,5), se=FALSE, guide=TRUE, color="black") +
         geom_polygon(data = data.frame(x=c(0,100,100,0), y=c(0,0,0.297,0.297)), aes(x=x, y=y), alpha=0.1) +
         facet_grid(wine~sensation) +
         scale_x_continuous(breaks=c(50), name = "Normalized Time") +
         scale_y_continuous(breaks=c(0.50), name = "Dominance Rate (%)") +
         theme(axis.text = element_text(size=14), 
               axis.title=element_text(size=16),
               panel.background = element_rect(fill = "transparent"),
               #panel.grid.major = element_line(colour = "black", size=0.75),
               #panel.grid.minor = element_line(colour = "black", size=0.75),
               panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
               strip.text.y = element_text(colour = "black", size = 14),
               legend.text = element_text(size = 13, color = "black"),
               legend.title = element_text(size=13),
               legend.key = element_rect(fill = "transparent"))

## TDS by the wines 
ggplot(tdsNORMall[tdsNORMall$sensation %in% c("Bitter", "Astringent", "Sweet", "Sour", "Hot"),], 
       aes(x=time, y=domRate)) +
  geom_smooth(aes(color=sensation), method="lm", formula= y~ns(x,4), se=FALSE, guide=TRUE, size=2) +
  geom_polygon(data = data.frame(x=c(0,100,100,0), y=c(0,0,0.297,0.297)), aes(x=x, y=y), alpha=0.2) +
  facet_grid(~wine) +
  scale_x_continuous(breaks=c(50), name = "Normalized Time") +
  scale_y_continuous(breaks=c(0.50), name = "Dominance Rate (%)") +
  scale_color_brewer(name="Sensation", labels=c("Astringent", "Bitter", "Hot", "Sweet", "Sour"), palette="Set1") +
  #scale_linetype_manual(name="Sensation", breaks=c("Astringent", "Bitter", "Hot", "Sweet", "Sour"),
   #                     values = c(1,2,3,4,5)) +
  theme(axis.text = element_text(size=14, color="black", family = "Times New Roman"), 
        axis.title=element_text(size=16, color="black", family = "Times New Roman"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", colour = "black", fill=NA),
        strip.text.x = element_text(size = 14, color="black", face = "bold.italic", family = "Times New Roman"),
        legend.text = element_text(size = 14, color = "black", face="italic", family = "Times New Roman"),
        legend.title = element_text(size=14, color = "black", face = "bold", family = "Times New Roman"),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom")
  




