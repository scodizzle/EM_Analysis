####  
####

library(tidyr)
library(dplyr)
library(ca)
library(candisc)
library(ggplot2)
library(gridExtra)
library(grid)
# start with the normalized TDS data.....  

save(list=c("tdsNORMall","norm.ready"), file="CAtds.RData")
load("CAtds.Rdata")

## from the normalized EM dataset..

## list of all the frequecy of each att. by normalized time
normFreq <- lapply(norm.ready, FUN = function(dat, ...) {
  aa <- spread(dat[,-4], key=time, value=freq)
  rownames(aa) <- aa[,1]
  aa <- aa[,-1]
})

## CA on all the wine note the drop of the "all off option"
EM.ca <- lapply(normFreq, FUN = function(dat){
  ca(dat[-1,])
  })

## plot em.. ##  kinda usless...  but vectorized...  
lapply(EM.ca, plot)

## pull out for ANOVA..  
EM.out <- lapply(EM.ca, FUN = function(dat) {
  data.frame(sensation=rownames(dat$rowcoord), dat$rowcoord, Mass=dat$rowmass, ChiDist=dat$rowdist, Inertia=dat$rowinertia, row.names = 1:5)
})
EM.out <- do.call(rbind.data.frame, EM.out)
EM.out$trt <- do.call(rbind, strsplit(rownames(EM.out), split = "[_.]"))[,1]
EM.out$fermRep <- do.call(rbind, strsplit(rownames(EM.out), split = "[_.]"))[,2]
row.names(EM.out) <- 1:nrow(EM.out)
EM.out$trt <- factor(EM.out$trt) 
EM.out$fermRep <- factor(EM.out$fermRep)
EM.out <- EM.out[,c("trt","fermRep","sensation","Dim1","Dim2","Dim3","Dim4","Mass","ChiDist","Inertia")]


## ANOVA
summary.aov(lm(as.matrix(EM.out[,-c(1:3)]) ~ trt + sensation, data=EM.out))
summary(manova(as.matrix(EM.out[,-c(1:3)]) ~ trt + fermRep + fermRep/sensation, data=EM.out), test="Wilks")

##  Ugh....  nothing.....
##  try just the trt...
TRT <- spread(tdsNORMall[,-4], key=time, value=freq)
TRT <- cbind(TRT[TRT$sensation == "Sweet",][,-c(1)], TRT[TRT$sensation == "Sour",][,-c(1:2)], 
      TRT[TRT$sensation == "Bitter",][,-c(1:2)], TRT[TRT$sensation == "Hot",][,-c(1:2)], 
      TRT[TRT$sensation == "Astringent",][,-c(1:2)])
rr <- c(paste("Sw",seq(1:100), sep=""), paste("So",seq(1:100), sep=""), paste("B",seq(1:100), sep=""), paste("H",seq(1:100), sep=""), 
      paste("A",seq(1:100), sep=""))
row.names(TRT) <- TRT[,1]
TRT <- TRT[,-1]
colnames(TRT) <- rr
caTRT <- ca(TRT)

## make a dataframe for plotting...
caTime <- data.frame(code = row.names(caTRT$colcoord), caTRT$colcoord, row.names = 1:500)
caTime$sensation <- gsub("[0-9]","", caTime$code)
caTime$sensation <- factor(caTime$sensation)
caTime$time <- gsub("[^0-9]", "", caTime$code)

##  plot Dim1 vs Dim2
p1 <- 
  ggplot(caTime, aes(x=Dim1, y=Dim2)) +
  geom_point(aes(color=sensation), size=0) +
  geom_text(aes(color=sensation, label=time, show_guide=FALSE), family = "Times New Roman", fontface="bold", size=5) +
  
  #geom_text(data=as.data.frame(caTRT$rowcoord), aes(x=Dim1, y=Dim2, label=row.names(caTRT$rowcoord)),
   #         family = "Times New Roman", fontface="bold", size=7) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(yintercept=0, linetype = "dashed") +
  
  scale_x_continuous(paste("Dim 1 ", "(", round(summary(caTRT)$scree[1,3],1), "%", ")", sep=""), limits = c(-5,5)) +
  scale_y_continuous(paste("Dim 2 ", "(", round(summary(caTRT)$scree[2,3],1), "%", ")", sep=""), limits = c(-4,4)) +
  
  scale_color_brewer(name="Sensation", labels=c("Astringent", "Bitter", "Hot", "Sour", "Sweet"), palette="Set1",
                     guide=guide_legend(override.aes=list(size=4))) +
  
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.text = element_text(size = 13, color = "black", face="italic", family = "Times New Roman"),
        legend.title = element_text(size = 13, color = "black", face = "bold", family = "Times New Roman"),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom")
  
## Dim1 vs Dim3
p2 <- ggplot(caTime, aes(x=Dim1, y=Dim3)) +
  geom_point(aes(color=sensation), size=0) +
  geom_text(aes(color=sensation, label=time, show_guide=FALSE), family = "Times New Roman", fontface="bold", size=5) +
  
  #geom_text(data=as.data.frame(caTRT$rowcoord), aes(x=Dim1, y=Dim3, label=row.names(caTRT$rowcoord)),
   #         family = "Times New Roman", fontface="bold", size=7) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(yintercept=0, linetype = "dashed") +
  
  scale_x_continuous(paste("Dim 1 ", "(", round(summary(caTRT)$scree[1,3],1), "%", ")", sep=""), limits = c(-5,5)) +
  scale_y_continuous(paste("Dim 3 ", "(", round(summary(caTRT)$scree[3,3],1), "%", ")", sep=""), limits = c(-4,4)) +
  
  scale_color_brewer(name="Sensation", labels=c("Astringent", "Bitter", "Hot", "Sour", "Sweet"), palette="Set1",
                     guide=guide_legend(override.aes=list(size=4))) +
  
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.text = element_text(size = 13, color = "black", face="italic", family = "Times New Roman"),
        legend.title = element_text(size = 13, color = "black", face = "bold", family = "Times New Roman"),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom")


p3 <- ggplot(as.data.frame(caTRT$rowcoord), aes(x=Dim1, y=Dim2, label=row.names(caTRT$rowcoord))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) + 
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(yintercept=0, linetype = "dashed") +
  
  scale_x_continuous(paste("Dim 1 ", "(", round(summary(caTRT)$scree[1,3],1), "%", ")", sep=""), limits = c(-5,5)) +
  scale_y_continuous(paste("Dim 2 ", "(", round(summary(caTRT)$scree[2,3],1), "%", ")", sep=""), limits = c(-4,4)) +
  
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.text = element_text(size = 13, color = "black", face="italic", family = "Times New Roman"),
        legend.title = element_text(size = 13, color = "black", face = "bold", family = "Times New Roman"),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom")

p4 <- ggplot(as.data.frame(caTRT$rowcoord), aes(x=Dim1, y=Dim3, label=row.names(caTRT$rowcoord))) +
  geom_text(family = "Times New Roman", fontface="bold", size=7) +
  geom_hline(yintercept=0, linetype = "dashed") +
  geom_vline(yintercept=0, linetype = "dashed") +
  
  scale_x_continuous(paste("Dim 1 ", "(", round(summary(caTRT)$scree[1,3],1), "%", ")", sep=""), limits = c(-5,5)) +
  scale_y_continuous(paste("Dim 3 ", "(", round(summary(caTRT)$scree[3,3],1), "%", ")", sep=""), limits = c(-4,4)) +
  theme(axis.text = element_text(size=16, color="black", family = "Times New Roman"),
        axis.title = element_text(size=16, color="black", family = "Times New Roman", face = "bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.text = element_text(size = 13, color = "black", face="italic", family = "Times New Roman"),
        legend.title = element_text(size = 13, color = "black", face = "bold", family = "Times New Roman"),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom")

grid_arrange_shared_legend(p3, p4, p1, p2)

## two plots one legend
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[3]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend(p1, p2, p3, p4)


dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
p1 <- qplot(carat, price, data=dsamp, colour=clarity)
p2 <- qplot(cut, price, data=dsamp, colour=clarity)
p3 <- qplot(color, price, data=dsamp, colour=clarity)
p4 <- qplot(depth, price, data=dsamp, colour=clarity)
grid_arrange_shared_legend(p1, p2, p3, p4)


