## PPT data 
## need to use determine significant differences then do LSD
##  start whit the nested ANOVA
## 

ppt.lm = lm(as.matrix(ppt[,c("TotalPhenol", "Anthocyanin", "SPP.Au", "LPP.Au", "SPP.LPP.Au", "Tannin", "NTP")]) 
            ~ wine + wine/fermRep, data=ppt) 

## the nested ANOVA will give a samller LSD....  
#Total Phenol
tp.lm = lm(TotalPhenol ~ wine, data=ppt)
tp.LSD = LSD.test(tp.lm, trt="wine", group=TRUE)
tp.LSD$statistics
tp.lmN = lm(TotalPhenol ~ wine + wine/fermRep, data=ppt)
tpN.LSD = LSD.test(tp.lmN, trt="wine", group=TRUE)
tpN.LSD$statistics

#Anthocyanin
ano.lm = lm(Anthocyanin ~ wine, data=ppt)
ano.LSD = LSD.test(ano.lm, trt="wine", group=TRUE)
ano.LSD$statistics
ano.lmN = lm(Anthocyanin ~ wine + wine/fermRep, data=ppt)
anoN.LSD = LSD.test(ano.lmN, trt="wine", group=TRUE)
anoN.LSD$statistics

#spp
spp.lm = lm(SPP.Au ~ wine, data=ppt)
spp.LSD = LSD.test(spp.lm, trt="wine", group=TRUE)
spp.LSD$statistics
spp.lmN = lm(SPP.Au ~ wine + wine/fermRep, data=ppt)
sppN.LSD = LSD.test(spp.lmN, trt="wine", group=TRUE)
sppN.LSD$statistics

#lpp
lpp.lm = lm(LPP.Au ~ wine, data=ppt)
lpp.LSD = LSD.test(lpp.lm, trt="wine", group=TRUE)
lpp.LSD$statistics
lpp.lmN = lm(LPP.Au ~ wine + wine/fermRep, data=ppt)
lppN.LSD = LSD.test(lpp.lmN, trt="wine", group=TRUE)
lppN.LSD$statistics

#lpp/spp
lppspp.lm = lm(LPP.SPP.Au ~ wine, data=ppt)
lppspp.LSD = LSD.test(lppspp.lm, trt="wine", group=TRUE)
lppspp.LSD$statistics
lppspp.lmN = lm(LPP.SPP.Au ~ wine + wine/fermRep, data=ppt)
lppsppN.LSD = LSD.test(lppspp.lmN, trt="wine", group=TRUE)
lppsppN.LSD$statistics

#spp+lpp
spplpp.lm = lm(SPP.LPP.Au ~ wine, data=ppt)
spplpp.LSD = LSD.test(spplpp.lm, trt="wine", group=TRUE)
spplpp.LSD$statistics
spplpp.lmN = lm(SPP.LPP.Au ~ wine + wine/fermRep, data=ppt)
spplppN.LSD = LSD.test(spplpp.lmN, trt="wine", group=TRUE)
spplppN.LSD$statistics

#tannin
tan.lm = lm(Tannin ~ wine, data=ppt)
tan.LSD = LSD.test(tan.lm, trt="wine", group=TRUE)
tan.LSD$statistics
tan.lmN = lm(Tannin ~ wine + wine/fermRep, data=ppt)
tanN.LSD = LSD.test(tan.lmN, trt="wine", group=TRUE)
tanN.LSD$statistics

#NTP
ntp.lm = lm(NTP ~ wine, data=ppt)
ntp.LSD = LSD.test(ntp.lm, trt="wine", group=TRUE)
ntp.LSD$statistics
ntp.lmN = lm(NTP ~ wine + wine/fermRep, data=ppt)
ntpN.LSD = LSD.test(ntp.lmN, trt="wine", group=TRUE)
ntpN.LSD$statistics

# bind together the LSD
ppt.means <- data.frame(
  tpN.LSD$groups[order(tpN.LSD$groups$trt),],
  anoN.LSD$groups[order(anoN.LSD$groups$trt),],
  tanN.LSD$groups[order(tanN.LSD$groups$trt),],
  ntpN.LSD$groups[order(ntpN.LSD$groups$trt),],
  sppN.LSD$groups[order(sppN.LSD$groups$trt),],
  lppN.LSD$groups[order(lppN.LSD$groups$trt),],
  lppsppN.LSD$groups[order(lppsppN.LSD$groups$trt),],
  spplppN.LSD$groups[order(spplppN.LSD$groups$trt),]
)

colnames(ppt.means) <- c("tp","mean","group","antho","mean","group","tan","mean","group","ntp","mean","group",
  "spp","mean","group","lpp","mean","group","lpp/spp","mean","group","spp+lpp","mean","group")
# the individul LSD values

cbind(
tpN.LSD$statistics[4],
anoN.LSD$statistics[4],
tanN.LSD$statistics[4],
ntpN.LSD$statistics[4],
sppN.LSD$statistics[4],
lppN.LSD$statistics[4],
lppsppN.LSD$statistics[4],
spplppN.LSD$statistics[4]
)



#####  Most everything below is garbage......    maybe steal the plot basic parameters



########  TANNIN PPT DATA  ###########
## total tannin
pdf(file="~/Dropbox/Frost_Research/Extended Macer Wines/Protein PPT analysis/AlltotTannin.pdf", width = 11, height = 8.5)
ggplot(ppt, aes(x=wine, y=Tannin)) + 
  geom_boxplot() +
  geom_point(data=ppt, aes(x=wine, y=Tannin, color=fermRep), position = position_jitter(w = 0.15, h = 0), size = 4) +
  stat_sum_single(mean, shape=9, color="black", size=6) + 
  scale_x_discrete(limits=c("PD", "SUB", "POC", "PO1", "PO2", "PO4", "PO6", "PO8", "S8")) +
  theme(axis.text = element_text(size=16)) +
  theme(axis.title = element_text(size=14)) +
  ylab(label="Tannin mg/L catechin eq.") +
  xlab(label="Wine") +
  scale_color_manual(name="Ferm Rep", values=c("red", "green", "blue")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16, face="bold"),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.title = element_text(size = 12))
dev.off()

## total phenolics mg/L CE
pdf(file="~/Dropbox/Frost_Research/Extended Macer Wines/Protein PPT analysis/AlltotPhenolics2.pdf", width = 11, height = 8.5)
ggplot(ppt, aes(x=wine, y=TotalPhenol)) + 
  geom_boxplot() +
  geom_point(data=ppt, aes(x=wine, y=TotalPhenol, color=fermRep), position = position_jitter(w = 0.15, h = 0), size = 4) +
  stat_sum_single(mean, shape=9, color="black", size=6) + 
  scale_x_discrete(limits=c("PD", "SUB", "POC", "PO1", "PO2", "PO4", "PO6", "PO8", "S8")) +
  theme(axis.text = element_text(size=16)) +
  theme(axis.title = element_text(size=14)) +
  ylab(label="Total Phenolics mg/L catechin eq.") +
  xlab(label="Wine") +
  scale_color_manual(name="Ferm Rep", values=c("red", "green", "blue")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.title = element_text(size = 12))
dev.off()

## anthocyanin mg/L MG3
pdf(file="~/Dropbox/Frost_Research/Extended Macer Wines/Protein PPT analysis/Allantho.pdf", width = 11, height = 8.5)
ggplot(ppt, aes(x=wine, y=Anthocyanin)) + 
  geom_boxplot() +
  geom_point(data=ppt, aes(x=wine, y=Anthocyanin, color=fermRep), position = position_jitter(w = 0.15, h = 0), size = 4) +
  stat_sum_single(mean, shape=9, color="black", size=6) + 
  scale_x_discrete(limits=c("PD", "SUB", "POC", "PO1", "PO2", "PO4", "PO6", "PO8", "S8")) +
  theme(axis.text = element_text(size=16)) +
  theme(axis.title = element_text(size=14)) +
  ylab(label="Anthocyanin mg/L Malvidin-3-glucoside") +
  xlab(label="Wine") +
  scale_color_manual(name="Ferm Rep", values=c("red", "green", "blue")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.title = element_text(size = 12))
dev.off()

## SPP ###
pdf(file="~/Dropbox/Frost_Research/Extended Macer Wines/Protein PPT analysis/Allspp.pdf", width = 11, height = 8.5)
ggplot(ppt, aes(x=wine, y=SPP.Au)) + 
  geom_boxplot() +
  geom_point(data=ppt, aes(x=wine, y=SPP.Au, color=fermRep), position = position_jitter(w = 0.15, h = 0), size = 4) +
  stat_sum_single(mean, shape=9, color="black", size=6) + 
  scale_x_discrete(limits=c("PD", "SUB", "POC", "PO1", "PO2", "PO4", "PO6", "PO8", "S8")) +
  theme(axis.text = element_text(size=16)) +
  theme(axis.title = element_text(size=14)) +
  ylab(label="Small Polymeric Pigments (Au)") +
  xlab(label="Wine") +
  scale_color_manual(name="Ferm Rep", values=c("red", "green", "blue")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.title = element_text(size = 12))
dev.off()

##  LPP  ###
pdf(file="~/Dropbox/Frost_Research/Extended Macer Wines/Protein PPT analysis/All_lpp.pdf", width = 11, height = 8.5)
ggplot(ppt, aes(x=wine, y=LPP.Au)) + 
  geom_boxplot() +
  geom_point(data=ppt, aes(x=wine, y=LPP.Au, color=fermRep), position = position_jitter(w = 0.15, h = 0), size = 4) +
  stat_sum_single(mean, shape=9, color="black", size=6) + 
  scale_x_discrete(limits=c("PD", "SUB", "POC", "PO1", "PO2", "PO4", "PO6", "PO8", "S8")) +
  theme(axis.text = element_text(size=16)) +
  theme(axis.title = element_text(size=14)) +
  ylab(label="Large Polymeric Pigments (Au)") +
  xlab(label="Wine") +
  scale_color_manual(name="Ferm Rep", values=c("red", "green", "blue")) +
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(linetype = "solid", color = "black", fill=NA),
        panel.grid.major = element_line(color="transparent"),
        legend.title = element_text(size = 12))
dev.off()



