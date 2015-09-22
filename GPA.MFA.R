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

da.mean <- aggregate(em.da[,-c(1:8)], by = list(em.da$wine), mean)
ppt.mean <- aggregate(ppt[,c("TotalPhenol","Anthocyanin","Tannin","LPP","SPP")], by = list(ppt$wine), mean)





#######################################################################################
## MFA 
mfaAll = MFA(allMean, group=c(18,4,29), name.group=c("Desc", "Tannin", "Vol"))
plot.MFA(mfaAll, choix = "ind", habillage="group", partial="all", title="Multiple Factor Analysis")


## GPA
gpaAll = GPA(allMean, group=c(18,4,29), name.group=c("Desc", "Tannin", "Vol"))
plot.GPA(gpaAll, choix = "ind", habillage="group", partial="all", title="Generalized Procrustes Analysis")