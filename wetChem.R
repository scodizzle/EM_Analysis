#####  wet chemistry  

save(list = c("wetChem","wcCVA", "pptPlot"), file = "wetChem.Rdata")
# import wet chem data
wetChem <- read.csv("WetChem.csv", header = TRUE)
str(wetChem)

load("wetChem.Rdata")
# ANOVA
wc.lm <- lm(as.matrix(wetChem[,-c(1:2)]) ~ wine + fermRep, data=wetChem)
summary.aov(wc.lm)

pH <- LSD.test(lm(pH ~ wine + fermRep, data = wetChem), trt="wine", group=TRUE)
TA <- LSD.test(lm(TA ~ wine + fermRep, data = wetChem), trt="wine", group=TRUE)
MA <- LSD.test(lm(MA ~ wine + fermRep, data = wetChem), trt="wine", group=TRUE)
AA <- LSD.test(lm(AA ~ wine + fermRep, data = wetChem), trt="wine", group=TRUE)
EtOH <- LSD.test(lm(EtOH ~ wine + fermRep, data = wetChem), trt="wine", group=TRUE)

cbind(
  pH$groups[order(pH$groups$trt),],
  TA$groups[order(TA$groups$trt),],
  MA$groups[order(MA$groups$trt),],
  AA$groups[order(AA$groups$trt),],
  EtOH$groups[order(EtOH$groups$trt),]
)
cbind(
  pH$statistics[4],
  TA$statistics[4],
  MA$statistics[4],
  AA$statistics[4],
  EtOH$statistics[4]
)

## manova on the scaled dataset
wetChemScale <- cbind(wetChem[,c(1,2)], scale(wetChem[,c(3:8)], center = TRUE))
wc.man <- manova(as.matrix(wetChemScale[,-c(1,2,5)]) ~ wine, data=wetChemScale)
summary(wc.man, test="Wilks")

## CVA 
wcCVA <- candisc(wc.man, scale=FALSE)
plot(wcCVA)
## CVA plot
wcPlot <- 
  ggplot(wcCVA$means, aes(x=Can1, y=Can2, label=row.names(wcCVA$means))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) +
  geom_segment(data=as.data.frame(wcCVA$structure), aes(x=0, y=0, xend=Can1*5, yend=Can2*5, label=row.names(wcCVA$structure)), 
               arrow=arrow(length=unit(0.3,"cm")), color="grey", size=1) +
  geom_text(data=as.data.frame(wcCVA$structure), 
            aes(x=Can1*5, y=Can2*5, label=row.names(wcCVA$structure)), family = "Times New Roman", fontface = "italic") +
  scale_x_continuous(paste("Can 1 ", "(", round(wcCVA$pct[1],1), "%", ")", sep=""), limits = c(-6.5, 6)) +
  scale_y_continuous(paste("Can 2 ", "(", round(wcCVA$pct[2],1), "%", ")", sep="")) +
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))
wcPlot
