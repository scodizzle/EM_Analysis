
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

# drop ethyl.decanoate & vitispirane.I.and.II
volSig <- ScaleVol[,-which(names(ScaleVol) %in% c("ethyl.decanoate", "vitispirane.I.and.II"))]

# CVA of the significant measurments
volMan <- manova(as.matrix(volSig[,-c(1:9)]) ~ wine, data = volSig)
summary(volMan, test = "Wilks")
volCVA <- candisc(volMan, scale=FALSE)
plot(volCVA)

# CVA Plot
ggplot(volCVA$means, aes(x=Can1, y=Can2, label=row.names(volCVA$means))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) +
  geom_segment(data=as.data.frame(volCVA$coeffs.std), aes(x=0, y=0, xend=Can1*3.5, yend=Can2*3.5, label=row.names(volCVA$coeffs.std)), 
               arrow=arrow(length=unit(0.3,"cm")), color="grey", size=1) +
  geom_text(data=as.data.frame(volCVA$coeffs.std), 
            aes(x=Can1*3.5, y=Can2*3.5, label=row.names(volCVA$coeffs.std)), family = "Times New Roman", fontface = "italic") +
  scale_x_continuous(paste("Can 1 ", "(", round(volCVA$pct[1],1), "%", ")", sep=""), limits = c(-27,20)) +
  scale_y_continuous(paste("Can 2 ", "(", round(volCVA$pct[2],1), "%", ")", sep="")) +
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))












