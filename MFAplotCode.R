m3 <- plot.MFA(mfa3, choix = "ind", habillage="group", partial="all", title="MFA 3 CVs")

part <- as.data.frame(mfa3$ind$coord.partiel)
colnames(part) <- unlist(lapply(1:length(part), FUN = function(i) paste("parti",(i), sep="")))
part$part.trt <- do.call(rbind, strsplit(rownames(part), split="[.]"))[,1]
part$meas <- do.call(rbind, strsplit(rownames(part), split="[.]"))[,2]

ind <- as.data.frame(mfa3$ind$coord)
colnames(ind) <- unlist(lapply(1:length(ind), FUN = function(i) paste("indi",(i), sep="")))
ind <- ind[rep(seq_len(nrow(ind)), each =5),]
ind$ind.trt <- do.call(rbind, strsplit(rownames(ind), split="[.]"))[,1]

identical(part$part.trt, ind$ind.trt)

dat <- cbind(part, ind)


ggplot(dat, aes(x=parti1, y=parti2)) +
  geom_point(aes(shape=meas), size = 3) +
  geom_segment(aes(x=indi1, y=indi2, xend=parti1, yend=parti2, linetype=meas)) +
  geom_text(aes(x=indi1, y=indi2, label=ind.trt), family = "Times New Roman", fontface="bold", size=7) +
  
  scale_x_continuous(paste("Dim 1 ", "(", round(mfa3$eig[1,2],1), "%", ")", sep="")) +
  scale_y_continuous(paste("Dim 2 ", "(", round(mfa3$eig[2,2],1), "%", ")", sep="")) +
  
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
  