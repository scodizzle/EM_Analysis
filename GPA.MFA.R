#######################################################################################
## MFA ##
#######################################################################################

library(agricolae)
library(candisc)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(SensoMineR)
library(plyr)


save(list = c("sigCVA", "pptCVA", "volCVA", "wcCVA", "tdsBA.CVA", "tdsSigCVA", "tdsBA.CVA", "means", "caTRT", "mfaDat"), file = "GPA_MFA.RData")
load("GPA_MFA.RData")

##############################################################################################################
#### MFA on the CVs from the CVAs
mfaAll <- MFA(cbind(mfaDat$da, mfaDat$ppt, mfaDat$vol, mfaDat$wet, mfaDat$tds), type = rep("c",5), 
              group = c(7,5,8,5,8), ncp = 3, name.group = c("da","ply","vol","wet","tds"))
plot.MFA(mfaAll, choix = "ind", habillage="group", partial="all", title="MFA All CVs")

mfaSig <- MFA(cbind(mfaDat$da[,1:2], mfaDat$ppt[,1:5], mfaDat$vol[,1:8], mfaDat$wet[,1:2], mfaDat$tds[,1:3]), 
              type = rep("c",5), group = c(2,5,8,2,3), name.group = c("da","ply","vol","wet","tds"))
plot.MFA(mfaSig, choix = "ind", habillage="group", partial="all", title="MFA Sig CVs")

mfa3 <- 
  MFA(cbind(mfaDat$da[,1:3], mfaDat$ppt[,1:3], mfaDat$vol[,1:3], mfaDat$wet[,1:3], mfaDat$tds[,1:3]), 
            type = rep("c",5), group = c(3,3,3,3,3), name.group = c("da","ply","vol","wet","tds"))
m3 <- plot.MFA(mfa3, choix = "ind", habillage="group", partial="all", title="MFA 3 CVs")

### MFA on the means of the measurmetns
mfaMean <- MFA(cbind(means$da, means$ppt, means$vol, means$wet, TRT), type=c("c","c","c","c","f"),
    group = c(18,5,29,6,500), name.group = c("da","ply","vol","wet","tds"))
plot.MFA(mfaMean, choix = "ind", habillage="group", partial="all", title="MFA on Means")

mfaMeanSig <- MFA(cbind(means$da[,c("RedFruit","PepperSpice","Aldehydic","Alcohol","Bitter","Hot","AstTexture")], 
                    means$ppt[,c("TotalPhenol","Anthocyanin", "SPP", "LPP", "Tannin")],
                    means$vol[,-which(names(means$vol) %in% c("ethyl.decanoate", "vitispirane.I.and.II"))],
                    means$wet[,-3], TRT), type=c("c","c","c","c","f"),
              group = c(7,5,27,5,500), name.group = c("da","ply","vol","wet","tds"))
plot.MFA(mfaMeanSig, choix = "ind", habillage="group", partial="all")

noTDS <- MFA(cbind(means$da[,c("RedFruit","PepperSpice","Aldehydic","Alcohol","Bitter","Hot","AstTexture")], 
          means$ppt[,c("TotalPhenol","Anthocyanin", "SPP", "LPP", "Tannin")],
          means$vol[,-which(names(means$vol) %in% c("ethyl.decanoate", "vitispirane.I.and.II"))],
          means$wet[,-3]), type=c("c","c","c","c"),
    group = c(7,5,27,5), name.group = c("da","ply","vol","wet"))

              
mfaMeanSig$group$RV
mfaMeanSig$group$contrib
mfaMeanSig$group$cos2
mfaMeanSig$ind$contrib
mfaMeanSig$ind$cos2

plot(mfaMeanSig, choix = "var", habillage = "group", select="contrib 10")
mfaMeanSig$quanti.var$contrib

mfaMeanSig$quanti.var$contrib[order(rowSums(mfaMeanSig$quanti.var$contrib[,1:2]), decreasing = TRUE),]

dimdesc(mfaMeanSig)





#########


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

#######
#######
# MFA with the partial points

## dataset for plot
part <- as.data.frame(mfaMeanSig$ind$coord.partiel)
  colnames(part) <- unlist(lapply(1:length(part), FUN = function(i) paste("parti",(i), sep="")))
  part$part.trt <- do.call(rbind, strsplit(rownames(part), split="[.]"))[,1]
  part$meas <- do.call(rbind, strsplit(rownames(part), split="[.]"))[,2]

ind <- as.data.frame(mfaMeanSig$ind$coord)
  colnames(ind) <- unlist(lapply(1:length(ind), FUN = function(i) paste("indi",(i), sep="")))
  ind <- ind[rep(seq_len(nrow(ind)), each =5),]
  ind$ind.trt <- do.call(rbind, strsplit(rownames(ind), split="[.]"))[,1]

identical(part$part.trt, ind$ind.trt)

dat <- cbind(part, ind)

mP <- ggplot(dat, aes(x=parti1, y=parti2)) +
  geom_point(aes(shape=meas, color=meas), size = 3) +
  geom_segment(aes(x=indi1, y=indi2, xend=parti1, yend=parti2, linetype=meas, color=meas)) +
  geom_text(aes(x=indi1, y=indi2, label=ind.trt), family = "Times New Roman", fontface="bold", size=7) +
  
  scale_x_continuous(paste("Dim 1 ", "(", round(mfaMeanSig$eig[1,2],1), "%", ")", sep="")) +
  scale_y_continuous(paste("Dim 2 ", "(", round(mfaMeanSig$eig[2,2],1), "%", ")", sep="")) +
  
  scale_color_brewer(name="", labels=c("DA","PolyPh","TDS","Vol","Wet"), palette="Set1") + #, guide=guide_legend(override.aes=list(size=1))) +
  
  scale_shape_manual(name = "", labels=c("DA","PolyPh","TDS","Vol","Wet") ,values = c(17,1,15,8,7)) +
  scale_linetype_manual(name = "", labels=c("DA","PolyPh","TDS","Vol","Wet") ,values = c(1,3,2,4,5)) +
  
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.text = element_text(size = 13, color = "black", face="italic", family = "Times New Roman"),
        legend.title = element_text(size = 13, color = "black", face = "bold", family = "Times New Roman"),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom")

head(dat)  

## partial Axes circle
plot(mfaMeanSig, choix="axes", habillage = "group", cex=0.7, shadowtext=TRUE, axes=c(1,2))

# dataset for plot
tt <- as.data.frame(mfaMeanSig$partial.axes$coord)
tt$group <- factor(c(rep("DA",5), rep("PolyPh",5), rep("Vol",5), rep("Wet", 5), rep("TDS",5)) )
rownames(tt) <- sub("da","DA",row.names(tt))
rownames(tt) <- sub("ply","Ph",row.names(tt))
rownames(tt) <- sub("vol","Vol",row.names(tt))
rownames(tt) <- sub("wet","Wet",row.names(tt))
rownames(tt) <- sub("tds","TDS",row.names(tt))

pax <- ggplot(tt, aes(x=Dim.1, y=Dim.2, label=row.names(tt))) +
  geom_text(aes(x=Dim.1*1.1, y=Dim.2*1.1, color=group, show_guide=FALSE)) +
  geom_segment(aes(x=0, y=0, xend=Dim.1, yend=Dim.2, color=group),  arrow=arrow(length=unit(0.3,"cm")), size=0.5) +
  annotate("path", x=0+1*cos(seq(0,2*pi,length.out=100)),y=0+1*sin(seq(0,2*pi,length.out=100))) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(yintercept=0, linetype = "dashed") +
  scale_color_brewer(name="", palette="Set1", guide=guide_legend(override.aes=list(size=1))) +
  scale_x_continuous(paste("Dim 1 ", "(", round(mfaMeanSig$eig[1,2],1), "%", ")", sep=""), limits=c(-1.15,1.15)) +
  scale_y_continuous(paste("Dim 2 ", "(", round(mfaMeanSig$eig[2,2],1), "%", ")", sep=""), limits=c(-1.15,1.15)) +
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.text = element_text(size = 13, color = "black", face="italic", family = "Times New Roman"),
        legend.title = element_text(size = 13, color = "black", face = "bold", family = "Times New Roman"),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom")
pax

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
leg <- get_legend(mP)


grid.arrange(mP+theme(legend.position="none"), pax+theme(legend.position="none"), leg, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))



