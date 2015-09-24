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


save(list = c("means"), file = "GPA_MFA.RData")
load("GPA_MFA.RData")

##############################################################################################################

## MFA 
mfaAll <- MFA(cbind(means$da, means$ppt, means$tds, means$vol, means$wet), 
              group = c(18,5,25,29,6), name.group = c("da","ppt","tds","vol","wet"))
plot.MFA(mfaAll, choix = "ind", habillage="group", partial="all", title="Multiple Factor Analysis")

# sig terms
mfaSig <- MFA(cbind(means$da[,c("Sweet", "Sour", "Bitter", "Hot", "Astringency", "Drying", "AstTexture")], means$ppt,
                    means$tds[,c("B.area","B.maxDom","B.dur")], 
                    means$vol[,-which(names(means$vol) %in% c("ethyl.decanoate", "vitispirane.I.and.II"))],
                    means$wet[,c("pH","TA","MA","AA","EtOH")]), group = c(7,5,3,27,5), 
                    name.group = c("da","ppt","tds","vol","wet")) 
plot.MFA(mfaSig, choix = "ind", habillage="group", partial="all", title="Multiple Factor Analysis")





## GPA
gpaAll <- GPA(cbind(means$da, means$ppt, means$tds, means$vol, means$wet), 
              group = c(18,5,25,29,6), name.group = c("da","ppt","tds","vol","wet"))
plot.GPA(gpaAll, choix = "ind", habillage="group", partial="all", title="Generalized Procrustes Analysis")






##############################################################################################################
##############################################################################################################

## mean table creation
## mean of the da
da.mean <- aggregate(em.da[,-c(1:8)], by = list(em.da$wine), mean)
# the LPP and SPP is log scaled, and the entire dataset is mean center scaled
ppt.mean <- aggregate(pptScale[,c("TotalPhenol","Anthocyanin","Tannin","LPP","SPP")], by = list(pptScale$wine), mean)
# mean centered scaled was used
vol.mean <- aggregate(ScaleVol[,-c(1:9)], by = list(ScaleVol$wine), mean)
wet.mean <- aggregate(wetChemScale[,-c(1:2)], by = list(wetChemScale$wine), mean)
# tds mean table
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
tds.mean <- aggregate(tdsAll.t[,-c(1:3)], by = list(tdsAll.t$trt), mean)

rownames(da.mean) = da.mean$Group.1
da.mean <- da.mean[,-1]

rownames(ppt.mean) = ppt.mean$Group.1
ppt.mean <- ppt.mean[,-1]

rownames(tds.mean) = tds.mean$Group.1
tds.mean <- tds.mean[,-1]

rownames(vol.mean) = vol.mean$Group.1
vol.mean <- vol.mean[,-1]

rownames(wet.mean) = wet.mean$Group.1
wet.mean <- wet.mean[,-1]

means <- list(da.mean, ppt.mean, tds.mean, vol.mean, wet.mean)
names(means) = c("da", "ppt", "tds", "vol", "wet")