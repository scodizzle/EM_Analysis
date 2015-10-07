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


save(list = c("sigCVA", "pptCVA", "volCVA", "wcCVA", "tdsBA.CVA", "tdsSigCVA", "tdsBA.CVA", "means", "caTRT", "mfaDat"), file = "GPA_MFA.RData")
load("GPA_MFA.RData")

##############################################################################################################
#### MFA 
mfaAll <- GPA(cbind(mfaDat$da, mfaDat$ppt, mfaDat$vol, mfaDat$wet, mfaDat$tds), 
              group = c(7,5,8,5,8), name.group = c("da","ppt","vol","wet","tds"))
plot.MFA(mfaAll, choix = "ind", habillage="group", partial="all", title="Multiple Factor Analysis")

mfaSig <- MFA(cbind(mfaDat$da[,1:2], mfaDat$ppt[,1:5], mfaDat$vol[,1:8], mfaDat$wet[,1:2], mfaDat$tds[,1:3]), 
              group = c(2,5,8,2,3), name.group = c("da","ppt","vol","wet","tds"))
plot.MFA(mfaSig, choix = "ind", habillage="group", partial="all", title="Multiple Factor Analysis")

mfa3 <- MFA(cbind(mfaDat$da[,1:3], mfaDat$ppt[,1:3], mfaDat$vol[,1:3], mfaDat$wet[,1:3], mfaDat$tds[,1:3]), 
              group = c(3,3,3,3,3), name.group = c("da","ppt","vol","wet","tds"))
plot.MFA(mfa3, choix = "ind", habillage="group", partial="all", title="Multiple Factor Analysis")


#########
## GPA
gpaAll <- GPA(cbind(mfaDat$da, mfaDat$ppt, mfaDat$vol, mfaDat$wet, mfaDat$tds), 
              group = c(7,5,8,5,8), name.group = c("da","ppt","vol","wet","tds"))
plot.GPA(gpaAll, choix = "ind", habillage="group", partial="all", title="")

gpaSig <- GPA(cbind(mfaDat$da[,1:2], mfaDat$ppt[,1:5], mfaDat$vol[,1:8], mfaDat$wet[,1:2], mfaDat$tds[,1:3]), 
              group = c(2,5,8,2,3), name.group = c("da","ppt","vol","wet","tds"))
plot.GPA(mfaSig, choix = "ind", habillage="group", partial="all", title="")

gpa3 <- GPA(cbind(mfaDat$da[,1:3], mfaDat$ppt[,1:3], mfaDat$vol[,1:3], mfaDat$wet[,1:3], mfaDat$tds[,1:3]), 
            group = c(3,3,3,3,3), name.group = c("da","ppt","vol","wet","tds"))
plot.GPA(mfa3, choix = "ind", habillage="group", partial="all", title="")

##############################################################################################################
##############################################################################################################
##############################################################################################################
##############################################################################################################
## mean table creation
###### DA MEAN
da.mean <- aggregate(em.da[,-c(1:8)], by = list(em.da$wine), mean)
###### ppt MEAN
# the LPP and SPP is log scaled, and the entire dataset is mean center scaled
ppt.mean <- aggregate(pptScale[,c("TotalPhenol","Anthocyanin","Tannin","LPP","SPP")], by = list(pptScale$wine), mean)
###### VOl MEAN
# mean centered scaled was used
vol.mean <- aggregate(ScaleVol[,-c(1:9)], by = list(ScaleVol$wine), mean)
wet.mean <- aggregate(wetChemScale[,-c(1:2)], by = list(wetChemScale$wine), mean)
###### tds MEAN
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

###### adjust row nams and stich it together
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

##### CVA means ....  ############
cbind(  
sigCVA$means,
pptCVA$means,
volCVA$means,
wcCVA$means,
tdsBA.CVA$means)

### 
mfaDat <- list(da = sigCVA$means, ppt = pptCVA$means, vol = volCVA$means, wet = wcCVA$means, tds = caTRT$rowcoord)



