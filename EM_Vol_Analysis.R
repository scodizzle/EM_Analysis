
######################################################################################
#### VOL Analysis
######################################################################################

library(agricolae)
library(plyr)
library(ggplot2)
library(xlsx)
library(candisc)
library(grid)
library(reshape2)

save(list = c("EMvol","volCVA", "volPlot"), file = "EMvol.Rdata")
load("EMvol.Rdata")
# Boxplots of the various scaling methods
boxplot(EMvol[,-c(1:9)])
#boxplot(scale(EMvol[,-c(1:9)]))
#boxplot(log(EMvol[,-c(1:9)]))
#boxplot(scale(log10(EMvol[,-c(1:9)])))

#Scale the data no log tho...
ScaleVol <- cbind(EMvol[,1:9], scale(EMvol[,-c(1:9)], center = TRUE)) #mean centerscale the vol data

#####  ANOVA of the scaled DATA  #####
vol.lm <- lm(as.matrix(ScaleVol[,-c(1:9)]) ~ wine + wine/fermRep, data = ScaleVol)
summary.aov(vol.lm)
# All significant but ethyl.decanoate  vitispirane.I.and.II
#LSD out...
bb <- lapply(10:38, FUN = function(i) {
  LSD.test(lm(ScaleVol[,colnames(ScaleVol)[i]] ~ (wine + wine/fermRep), data = ScaleVol), trt = "wine", group=TRUE) })
names(bb) = colnames(ScaleVol)[10:38]

bb.lsd <- lapply(1:29, FUN = function(i) {bb[[i]]$groups[order(bb[[i]]$groups$trt),]})
names(bb.lsd) = colnames(ScaleVol)[10:38]

cc <- do.call(cbind, lapply(1:29, FUN= function(i) bb.lsd[[i]]))
#write.csv(cc, file="Vol_LSD.csv")

cc.lsd <- do.call(cbind, lapply(1:29, FUN=function(i) bb[[i]]$statistics[4]))
#write.csv(cc.lsd, file="lsd.csv")

# drop ethyl.decanoate & vitispirane.I.and.II
volSig <- ScaleVol[,-which(names(ScaleVol) %in% c("ethyl.decanoate", "vitispirane.I.and.II"))]

# CVA of the significant measurments
volMan <- manova(as.matrix(volSig[,-c(1:9)]) ~ wine, data = volSig)
summary(volMan, test = "Wilks")
volCVA <- candisc(volMan, scale=FALSE)
plot(volCVA)

# CVA Plot
volPlot <- ggplot(volCVA$means, aes(x=Can1, y=Can2, label=row.names(volCVA$means))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) +
  geom_segment(data=as.data.frame(volCVA$structure), aes(x=0, y=0, xend=Can1*15.5, yend=Can2*15.5, label=row.names(volCVA$structure)), 
               arrow=arrow(length=unit(0.3,"cm")), color="grey", size=1) +
  geom_text(data=as.data.frame(volCVA$structure), 
            aes(x=Can1*15.5, y=Can2*15.5, label=row.names(volCVA$structure)), family = "Times New Roman", fontface = "italic") +
  scale_x_continuous(paste("Can 1 ", "(", round(volCVA$pct[1],1), "%", ")", sep=""), limits = c(-18,18)) +
  scale_y_continuous(paste("Can 2 ", "(", round(volCVA$pct[2],1), "%", ")", sep="")) +
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))
volPlot











