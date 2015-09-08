## analysis of the pt data, and plots from ggplot


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



