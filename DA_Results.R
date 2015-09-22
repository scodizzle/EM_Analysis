#### Macration Wine dataset ####
##############################


library(agricolae)
library(candisc)
library(ggplot2)
library(grid)


setwd("~/Dropbox/Frost_Research/Extended Macer Wines/EM_Analysis")
load("EMdata.Rdata")
save(list = c("em.da", "ppt", "stat_sum_single"), file = "EMdata.Rdata")

############################################################################################
## ANOVA of the DA ##
aov.em = aov(as.matrix(em.da[,-c(1:8)]) ~ (judge + wine + wine/ferm.rep + sen.rep)^2, data=em.da)
sum.aov.em = summary(aov.em)
# results of the ANOVA
# test the mixed model if needed
####### Red Fruit ######
sum.aov.em[[1]]
# pMM JxW 
sum.aov.em[[1]][2,3]/sum.aov.em[[1]][4,3]
qf(0.95, 8,88)
# F > Fcrit, reject Ho still significant
# LSD
RedFruit.lm = lm(RedFruit ~ (judge + wine + wine/ferm.rep + sen.rep)^2, data = em.da)
RedFruit.LSD = LSD.test(RedFruit.lm, trt="wine", group=TRUE)

####### CitrusFloral #######
sum.aov.em[[4]]
# pMM JxW 
sum.aov.em[[4]][2,3]/sum.aov.em[[4]][4,3]
qf(0.95, 8,88)
# F < Fcrit, accept Ho NOT SIGNIFICANT

####### PepperSpice #######
sum.aov.em[[7]]
# pMM JxW 
sum.aov.em[[7]][2,3]/sum.aov.em[[7]][4,3]
qf(0.95, 8,88)
# F > Fcrit, reject Ho still significant
# LSD
PepperSpice.lm = lm(PepperSpice ~ (judge + wine + wine/ferm.rep + sen.rep)^2, data = em.da)
PepperSpice.LSD = LSD.test(PepperSpice.lm, trt="wine", group=TRUE)

####### Earthy #####
sum.aov.em[[8]]
# pMM JxW 
sum.aov.em[[8]][2,3]/sum.aov.em[[8]][4,3]
qf(0.95, 8,88)
# F > Fcrit reject Ho
sum.aov.em[[8]][2,3]/sum.aov.em[[8]][7,3]
qf(0.95, 8,16)
# F < Fcrit accepct Ho, NOT SIGNIFICANT

####### Aldehydic #####
sum.aov.em[[9]]
# pMM JxW 
sum.aov.em[[9]][2,3]/sum.aov.em[[9]][4,3]
qf(0.95, 8,88)
# F> Fcrit reject Ho still significant
# LSD
Aldehydic.lm = lm(Aldehydic ~ (judge + wine + wine/ferm.rep + sen.rep)^2, data = em.da)
Aldehydic.LSD = LSD.test(Aldehydic.lm, trt="wine", group=TRUE)

###### Alcohol ########
sum.aov.em[[10]]
# pMM JxW 
sum.aov.em[[10]][2,3]/sum.aov.em[[10]][4,3]
qf(0.95, 8,88)
# F > Fcrit reject Ho still Significant
# LSD
Alcohol.lm = lm(Alcohol ~ (judge + wine + wine/ferm.rep + sen.rep)^2, data = em.da)
Alcohol.LSD = LSD.test(Alcohol.lm, trt="wine", group=TRUE)

###### VA ########
sum.aov.em[[11]]
# pMM JxW 
sum.aov.em[[11]][2,3]/sum.aov.em[[11]][4,3]
qf(0.95, 8,88)
# F < Fcrit accepct Ho NOT Significant

###### Sour ########
sum.aov.em[[13]]
# pMM JxW 
sum.aov.em[[13]][2,3]/sum.aov.em[[13]][4,3]
qf(0.95, 8,88)
# F > Fcrit accepct Ho NOT Significant

##### Bitter #########
sum.aov.em[[14]]
# Significant
Bitter.lm = lm(Bitter ~ (judge + wine + wine/ferm.rep + sen.rep)^2, data = em.da)
Bitter.LSD = LSD.test(Bitter.lm, trt="wine", group=TRUE)

####### Hot #####
sum.aov.em[[15]]
# pMM JxW 
sum.aov.em[[15]][2,3]/sum.aov.em[[15]][4,3]
qf(0.95, 8,88)
# F < Fcrit reject Ho Still Significant
# LSD
Hot.lm = lm(Hot ~ (judge + wine + wine/ferm.rep + sen.rep)^2, data = em.da)
Hot.LSD = LSD.test(Hot.lm, trt="wine", group=TRUE)

####### Astringent #####
sum.aov.em[[16]]
# pMM JxW 
sum.aov.em[[16]][2,3]/sum.aov.em[[16]][4,3]
qf(0.95, 8,88)
# F > Fcrit -- check WxSR
sum.aov.em[[16]][2,3]/sum.aov.em[[16]][7,3]
qf(0.95, 8, 16)
# F < Fcrit accepct Ho NOT significant

####### Drying #########
sum.aov.em[[17]]
# pMM JxW 
sum.aov.em[[17]][2,3]/sum.aov.em[[17]][7,3]
qf(0.95, 8,16)
# F < Fcrit accept Ho NOT significant

####### Astringent Texture #####
sum.aov.em[[18]]
AstTex.lm = lm(AstTexture ~ (judge + wine + wine/ferm.rep + sen.rep)^2, data = em.da)
AstTex.LSD = LSD.test(AstTex.lm, trt="wine", group=TRUE)

####
# Bind the LSD together into a single dataFrame
means.table <- cbind(
RedFruit.LSD$groups[order(RedFruit.LSD$groups$trt),],  
PepperSpice.LSD$groups[order(PepperSpice.LSD$groups$trt),],
Aldehydic.LSD$groups[order(Aldehydic.LSD$groups$trt),],
Alcohol.LSD$groups[order(Alcohol.LSD$groups$trt),],
Bitter.LSD$groups[order(Bitter.LSD$groups$trt),],
Hot.LSD$groups[order(Hot.LSD$groups$trt),],
AstTex.LSD$groups[order(AstTex.LSD$groups$trt),]
)
means.table <- means.table[,-c(4,7,10,13,16,19)]
colnames(means.table) = c("Wine","Red Fruit", "g1",
                                "Pepper Spice", "g2",
                                "Aldehydic", "g3",
                                "Alcohol", "g4",
                                "Bitter", "g5",
                                "Hot", "g6",
                                "Ast. Texture", "g7")

cbind(
RedFruit.LSD$statistics[4],
PepperSpice.LSD$statistics[4],
Aldehydic.LSD$statistics[4],
Alcohol.LSD$statistics[4],
Bitter.LSD$statistics[4],
Hot.LSD$statistics[4],
AstTex.LSD$statistics[4]
)

############################################################################################

### MANOVA and CVA analysis...    #####
# significant DA terms
sigMan <- manova(as.matrix(em.da[,c("RedFruit","PepperSpice","Aldehydic","Alcohol","Bitter","Hot","AstTexture")]) 
                 ~ wine, data=em.da)
summary(sigMan, test="Wilks")
# CVA and its plot
sigCVA = candisc(sigMan)
DAplot <- ggplot(sigCVA$means, aes(x=Can1, y=Can2, label=row.names(sigCVA$means))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) +
  geom_segment(data=as.data.frame(sigCVA$coeffs.std), aes(x=0, y=0, xend=Can1, yend=Can2, label=row.names(sigCVA$coeffs.std)), 
               arrow=arrow(length=unit(0.3,"cm")), color="grey", size=1) +
  geom_text(data=as.data.frame(sigCVA$coeffs.std), 
            aes(x=Can1, y=Can2, label=row.names(sigCVA$coeffs.std)), family = "Times New Roman", fontface = "italic") +
  scale_x_continuous(paste("Can 1 ", "(", round(sigCVA$pct[1],1), "%", ")", sep=""), limits = c(-1,1)) +
  scale_y_continuous(paste("Can 2 ", "(", round(sigCVA$pct[2],1), "%", ")", sep="")) +
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))



## using only the significant terms, the six terms can account for 81.4% of the varability with the
## first two dinensions.  
sigCVA$pct
sum(sigCVA$pct[1:2])
sum(sigCVA$pct[1:3])
# 90% of the varaibilty accounted for in the first 3 dimensions
# Scree Plot
ggplot(data.frame(x=1:7, EigValue=sigCVA$eigenvalues), aes(x,EigValue)) + geom_point() + geom_line()
# we can see that it breaks at 3
# Bartlets test?  How to do this?





######################################################################################
## plot of the seven significant discriptors for a presentation
em.mean = aggregate(em.da[,-c(1:8)], by = list(em.da$wine), mean)
meltMean = melt(em.mean)

meltMean[meltMean$variable %in% c("RedFruit","Alcohol","Aldehydic","Bitter","Hot","PepperSpice","AstTexture"),]

ggplot(meltMean[meltMean$variable %in% c("RedFruit","Alcohol","Aldehydic","Bitter","Hot","PepperSpice","AstTexture"),], aes(x=variable, y=value)) +
  geom_text(aes(label=Group.1, color=Group.1), fontface = "bold", size = 7) +
  #geom_vline(data=mean7, aes(xintercept=which(mean7$variable == "ethyl.acetate"))) +
  #geom_vline(xintercept=which(df$x == 'm')) +
  scale_color_manual(values=c("grey","grey","grey","grey","grey","red","grey", "black", "grey")) + 
  ylab("Mean Intensity") +
  #scale_colour_brewer(palette="Set1") +
  theme(axis.text = element_text(size=16, color="black"),
        axis.text.x  = element_text(angle=90, vjust=0.5),
        axis.title = element_text(size=16),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major.x = element_line(color="grey", linetype="dashed"),
        panel.grid.major.y = element_blank(),
        legend.position = "none")





##############################################################################################################################
######## GRAVE YARD ##########################################################################################################
##############################################################################################################################
##############################################################################################################################
######## GRAVE YARD ##########################################################################################################
##############################################################################################################################
##############################################################################################################################
######## GRAVE YARD ##########################################################################################################
##############################################################################################################################
ggplot(data.frame(pca1.mf$x), aes(x=PC1, y=PC2)) +
  geom_text(label=rownames(pca1.mf$x), fontface = "bold", size = 7) +
  geom_segment(data=as.data.frame(pca1.mf$rotation), aes(x=0, y=0, xend=PC1*3, yend=PC2*3, label=row.names(pca1.mf$rotation)), 
               arrow=arrow(length=unit(0.3,"cm")), color="red", size=1) +
  geom_text(data=as.data.frame(pca1.mf$rotation), aes(x=PC1*3, y=PC2*3, label=row.names(pca1.mf$rotation))) +
  scale_x_continuous(paste("PC 1 ", round(summary(pca1.mf)$importance[2,1]*100,1), "%", sep="")) +
  scale_y_continuous(paste("PC 2 ", round(summary(pca1.mf)$importance[2,2]*100,1), "%", sep="")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))

### PCA exp 2 ######
# POC PD SUB S8
pca2 = prcomp(em.mean[c("POC", "PD", "SUB", "S8"),])
pca2.mf = prcomp(em.mean[c("POC", "PD", "SUB", "S8"),
                         c("Sweet", "Sour", "Bitter", "Hot", "Astringency", "Drying", "AstTexture")])
#  PCA plot  #
ggplot(data.frame(pca2$x), aes(x=PC1, y=PC2)) +
  geom_text(label=rownames(pca2$x), fontface = "bold", size = 7) +
  geom_segment(data=as.data.frame(pca2$rotation), aes(x=0, y=0, xend=PC1*3, yend=PC2*3, label=row.names(pca2$rotation)), 
               arrow=arrow(length=unit(0.3,"cm")), color="red", size=1) +
  geom_text(data=as.data.frame(pca2$rotation), aes(x=PC1*3, y=PC2*3, label=row.names(pca2$rotation))) +
  scale_x_continuous(paste("PC 1 ", round(summary(pca2)$importance[2,1]*100,1), "%", sep="")) +
  scale_y_continuous(paste("PC 2 ", round(summary(pca2)$importance[2,2]*100,1), "%", sep="")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))

ggplot(data.frame(pca2.mf$x), aes(x=PC1, y=PC2)) +
  geom_text(label=rownames(pca2.mf$x), fontface = "bold", size = 7) +
  geom_segment(data=as.data.frame(pca2.mf$rotation), aes(x=0, y=0, xend=PC1*3, yend=PC2*3, label=row.names(pca2.mf$rotation)), 
               arrow=arrow(length=unit(0.3,"cm")), color="red", size=1) +
  geom_text(data=as.data.frame(pca2.mf$rotation), aes(x=PC1*3, y=PC2*3, label=row.names(pca2.mf$rotation))) +
  scale_x_continuous(paste("PC 1 ", round(summary(pca2.mf)$importance[2,1]*100,1), "%", sep="")) +
  scale_y_continuous(paste("PC 2 ", round(summary(pca2.mf)$importance[2,2]*100,1), "%", sep="")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))

# all wines all terms
em.man = manova(as.matrix(em.da[,-c(1:8)]) ~ (wine), data = em.da)
summary(em.man, test="Wilks") ## note sig Wilks
## CVA
em.cva = candisc(em.man)

### ggplot2 CVA plot #######
ggplot(em.cva$means, aes(x=Can1, y=Can2, label=row.names(em.cva$means))) +
  geom_text(fontface="bold", size=7) +
  geom_segment(data=as.data.frame(em.cva$coeffs.std), aes(x=0, y=0, xend=Can1, yend=Can2, label=row.names(em.cva$coeffs.std)), 
               arrow=arrow(length=unit(0.3,"cm")), color="red", size=1) +
  geom_text(data=as.data.frame(em.cva$coeffs.std), aes(x=Can1, y=Can2, label=row.names(em.cva$coeffs.std))) +
  scale_x_continuous(paste("Can 1 ", round(em.cva$pct[1],1), "%", sep=""), limits=c(-0.75, 0.65)) +
  scale_y_continuous(paste("Can 2 ", round(em.cva$pct[2],1), "%", sep="")) +
  theme(axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=16, color="black"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))

# All wines, MF terms... they all fail for signifi. with the MANOVA
summary(manova(as.matrix(
  em.da[,c("Sweet", "Sour", "Bitter", "Hot", "Astringency", "Drying", "AstTexture")]) ~ wine, data = em.da))
# POC, PO1, PO2, PO4, PO6, PO8
summary(manova(as.matrix(
  em.da[,c("Sweet", "Sour", "Bitter", "Hot", "Astringency", "Drying", "AstTexture")]
  [em.da$wine == c("POC", "PO1", "PO2", "PO4", "PO6", "PO8"),]) ~ (wine), 
  data = em.da[em.da$wine == c("POC", "PO1", "PO2", "PO4", "PO6", "PO8"),]))
# POC, PD, SUB, S8
summary(manova(as.matrix(
  em.da[,c("Sweet", "Sour", "Bitter", "Hot", "Astringency", "Drying", "AstTexture")]
  [em.da$wine == c("POC", "PD", "SUB", "S8"),]) ~ (wine), 
  data = em.da[em.da$wine == c("POC", "PD", "SUB", "S8"),]))

## all MANOVA of the MF terms only fail significance...  on to PCAs...

############################################################################################
####  PCA All Treatments  ####
em.mean = aggregate(em.da[,-c(1:8)], by=list(em.da$wine), mean)
rownames(em.mean) = em.mean$Group.1
em.mean = em.mean[,-1]
em.pca = prcomp(em.mean)

#  PCA plot #
pdf("~/Dropbox/Frost_Research/Extended Macer Wines/Descriptive/PCAall.pdf", width=11, height=8.5) 
ggplot(as.data.frame(em.pca$x), aes(x=PC1, y=PC2)) +
  geom_text(label=rownames(em.pca$x), fontface = "bold", size = 7) +
  geom_segment(data=as.data.frame(em.pca$rotation), aes(x=0, y=0, xend=PC1*3, yend=PC2*3, label=row.names(em.pca$rotation)), 
               arrow=arrow(length=unit(0.3,"cm")), color="red", size=1) +
  geom_text(data=as.data.frame(em.pca$rotation[-c(7,8,13), ]), aes(x=PC1*3, y=PC2*3, 
                                                                   label=row.names(em.pca$rotation[-c(7,8,13), ])), hjust=0, vjust=0 ) +
  geom_text(data=as.data.frame(em.pca$rotation[c(7,8,13), ]), aes(x=PC1*3, y=PC2*3, 
                                                                  label=row.names(em.pca$rotation[c(7,8,13), ])), hjust=1, vjust=0 ) +
  xlab(paste("PC 1 ", round(summary(em.pca)$importance[2,1]*100,1), "%", sep="")) +
  ylab(paste("PC 2 ", round(summary(em.pca)$importance[2,2]*100,1), "%", sep="")) +
  xlim(-1.1, 1.5) +
  ylim(-1, 1.5) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))
dev.off()

# Just MF #
pca.mf = prcomp(em.mean[,c("Sweet", "Sour", "Bitter", "Hot", "Astringency", "Drying", "AstTexture")])
#  PCA plot  #
pdf("~/Dropbox/Frost_Research/Extended Macer Wines/Descriptive/PCA_mf.pdf", width=11, height=8.5) 
ggplot(data.frame(pca.mf$x), aes(x=PC1, y=PC2)) +
  geom_text(label=rownames(pca.mf$x), fontface = "bold", size = 7) +
  geom_segment(data=as.data.frame(pca.mf$rotation), aes(x=0, y=0, xend=PC1*3, yend=PC2*3, label=row.names(pca.mf$rotation)), 
               arrow=arrow(length=unit(0.3,"cm")), color="red", size=1) +
  geom_text(data=as.data.frame(pca.mf$rotation), aes(x=PC1*3, y=PC2*3, label=row.names(pca.mf$rotation)), hjust=0, vjust=0) +
  scale_x_continuous(paste("PC 1 ", round(summary(pca.mf)$importance[2,1]*100,1), "%", sep="")) +
  scale_y_continuous(paste("PC 2 ", round(summary(pca.mf)$importance[2,2]*100,1), "%", sep="")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"))
dev.off()      
