#######################################################################################
## MFA ##
#######################################################################################

library(agricolae)
library(candisc)
library(ggplot2)
library(grid)
library(pls)
library(SensoMineR)
library(plyr)

save(list = c("da.mean", "ppt.mean", "vol.mean", "wet.mean", "tds.mean"), file = "GPA_MFA.RData")
load("GPA_MFA.RData")

da.mean <- aggregate(em.da[,-c(1:8)], by = list(em.da$wine), mean)
da.mean$Group.1 <- factor(da.mean$Group.1, levels = c("PD","PO1","PO2","PO4","PO6","PO8","POC","S8","SUB"))
da.mean <- da.mean[order(da.mean$Group.1),]
ppt.mean <- aggregate(ppt[,c("TotalPhenol","Anthocyanin","Tannin","LPP","SPP")], by = list(ppt$wine), mean)
vol.mean <- aggregate(EMvol[,-c(1:9)], by = list(EMvol$wine), mean)
wet.mean <- aggregate(wetChem[,-c(1:2)], by = list(wetChem$wine), mean)


bitter.mean <- aggregate(Bitter[,-c(6:8)], by = list(Bitter$trt), mean)





#######################################################################################
## MFA 
mfaAll = MFA(allMean, group=c(18,4,29), name.group=c("Desc", "Tannin", "Vol"))
plot.MFA(mfaAll, choix = "ind", habillage="group", partial="all", title="Multiple Factor Analysis")


## GPA
gpaAll = GPA(allMean, group=c(18,4,29), name.group=c("Desc", "Tannin", "Vol"))
plot.GPA(gpaAll, choix = "ind", habillage="group", partial="all", title="Generalized Procrustes Analysis")