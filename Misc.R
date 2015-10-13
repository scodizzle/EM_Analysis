##  Contribution of an Observation to a Dimension
## look for ones that are lorger than the average contribution
mfaMean$group$contrib

## conrtribution of the group variables for the da group to each dim
mfaMean$separate.analyses$da$var$contrib

plot.PCA(mfaMean$separate.analyses$da, title = "DA PCA")
plot.PCA(mfaMean$separate.analyses$vol, title = "Vol PCA")
plot.PCA(mfaMean$separate.analyses$ply, title = "Ply PCA")
plot.PCA(mfaMean$separate.analyses$wet, title = "wet PCA")

plot.PCA(mfaMeanSig$separate.analyses$da, title = "DA PCA Sig")
plot.PCA(mfaMeanSig$separate.analyses$vol, title = "Vol PCA Sig")
plot.PCA(mfaMeanSig$separate.analyses$ply, title = "Ply PCA Sig")
plot.PCA(mfaMeanSig$separate.analyses$wet, title = "wet PCA Sig")

levels(em.da$wine) <- c("PuD","Em1","Em2","Em4","Em6","Em8","Em0","Su8","Su0")

plot(res, choix = "var", habillage ="group", cex=0.7, shadowtext = TRUE, select="contrib 6")
plot(res, choix = "var", habillage ="group", cex=0.7, shadowtext = TRUE, select="contrib 6", axes=1:3)
plot(res, choix = "var", habillage ="group", cex=0.7, shadowtext = TRUE, select="contrib 6", axes=3:4)
plot(res, choix = "var", habillage ="group", cex=0.7, shadowtext = TRUE, select="contrib 6", axes=c(1,3))
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, partial = c("1VAU","1BOI"), select="cos2 0.4")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="cos2 0.4")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, partial = c("1VAU","1BOI"), select="cos2 0.4")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="cos2 0.4")
plot(res, invisible = "quali", habillage = 1, cex=0.8, select="cos2 0.4")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="cos2 0.4")
plot(res, invisible = "quali", habillage = 1, cex=0.8, select="cos2 0.4")
plot(res, invisible = "quali", habillage = 1, cex=0.8, select="contrib 8")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="contrib 8")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="contrib 8", unselect=0)
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="contrib 8", unselect=1)
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="contrib 8")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="contrib 8", unselect="grey")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="contrib 8", unselect="grey78")
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="contrib 8", unselect="grey78", axes=c(3,4))
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select="contrib 8", unselect="grey78", axes=3:4)
plot(res, invisible = "quali", habillage = "Soil", cex=0.8, select=c("1VAU","1BOI"), unselect="grey78", axes=3:4)
plot(res, choix = "var")
plot(res, choix = "var", habillage ="group")
plot(res, choix = "var", habillage ="group", cex=0.7)
plot(res, choix = "var", habillage ="group", cex=0.7, shadowtext = TRUE)
plot(res, choix = "var", habillage ="group", cex=0.7, shadowtext = TRUE, select="contrib 6")
plot(res, choix = "var", habillage ="group", cex=0.7, shadowtext = TRUE, select="contrib 6", aexs=1:3)

plot(mfaMeanSig, choix="var", habillage ="group", cex=0.7, shadowtext = TRUE)

## partial Axes
plot(mfaMeanSig, choix="axes", habillage = "group", cex=0.7, shadowtext=TRUE, axes=c(1,2))

tt <- as.data.frame(mfaMeanSig$partial.axes$coord)
ggplot(tt, aes(x=Dim.1, y=Dim.2, label=row.names(tt))) +
  geom_text() +
  geom_segment(aes(x=0, y=0, xend=Dim.1, yend=Dim.2),  arrow=arrow(length=unit(0.3,"cm")), color="grey", size=1)




# cor circle
aa <- mfaMeanSig$quanti.var$contrib
plot(mfaMeanSig, choix = "var", habillage = "group", select="contrib 10")

















