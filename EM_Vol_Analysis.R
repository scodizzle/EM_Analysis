
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

save(list = c("EMvol"), file = "EMvol.Rdata")

# log of all and center scale
boxplot(EMvol[,-c(1:9)])
#boxplot(scale(EMvol[,-c(1:9)]))
#boxplot(log(EMvol[,-c(1:9)]))
#boxplot(scale(log10(EMvol[,-c(1:9)])))

logScaleVol <- cbind(EMvol[,1:9], (EMvol[,-c(1:9)]))
#logScaleVol <- cbind(EMvol[,1:9], scale(EMvol[,-c(1:9)]))
#logScaleVol <- cbind(EMvol[,1:9], log(EMvol[,-c(1:9)]))
#logScaleVol <- cbind(EMvol[,1:9], scale(log(EMvol[,-c(1:9)])))

volMan <- manova(as.matrix(logScaleVol[,-c(1:9)]) ~ wine, data = logScaleVol)
summary(volMan, test = "Wilks")
volCVA <- candisc(volMan, scale=FALSE)
plot(volCVA)

# CVA Plot
volPlot <- ggplot(volCVA$means, aes(x=Can1, y=Can2, label=row.names(volCVA$means))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) +
  geom_segment(data=as.data.frame(volCVA$coeffs.std), aes(x=0, y=0, xend=Can1*5, yend=Can2*5, label=row.names(volCVA$coeffs.std)), 
               arrow=arrow(length=unit(0.3,"cm")), color="grey", size=1) +
  geom_text(data=as.data.frame(volCVA$coeffs.std), 
            aes(x=Can1*5, y=Can2*5, label=row.names(volCVA$coeffs.std)), family = "Times New Roman", fontface = "italic") +
  scale_x_continuous(paste("Can 1 ", "(", round(volCVA$pct[1],1), "%", ")", sep=""), limits = c(-27,27)) +
  scale_y_continuous(paste("Can 2 ", "(", round(volCVA$pct[2],1), "%", ")", sep=""), limits = c(-27,27))+
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))

