#### TDS #######
PO1 = tds.subset(da = tds, factA = tds$Wine, factB = tds$Attribute, samp = "PO1", n = 60)
PO1.plot = EM.tds.graph(PO1, cline=0.2, sline=0.285, title="TDS PO1")
PO1.plot
library(splines)
PO1.plot = EM.tds.graph(PO1, cline=0.2, sline=0.285, title="TDS PO1")


PO1[PO1$DomRate >= 0.285 & PO1$taste == "Bitter",]


#subset for astringency
PO1.ast <- PO1[PO1$DomRate >= 0.285 & PO1$taste == "Astringent",]
#subtract the sig line
PO1.ast$Dom0 <- PO1.ast$DomRate - 0.285
#create spline function
#other ways to do this
dd <- splinefun(x=PO1.ast$time, y=PO1.ast$Dom0, method = "natural")
#integrate stepwise
intg <- lapply(1:(nrow(PO1.ast)-1), FUN = function(i) 
  integrate(dd, lower = PO1.ast$time[i], upper = PO1.ast$time[i+1]))
#add to the df
PO1.ast$area <- c(0,unlist(lapply(1:43, FUN = function(i) intg[[i]]$value)))
#culumlitive sum -- add to the df
PO1.ast$areaCulm <- cumsum(PO1.ast$area)
#length at max --- length at top
lat <- nrow(PO1.ast[PO1.ast$DomRate == max(PO1.ast$DomRate),])
sum(PO1.ast[PO1.ast$DomRate == max(PO1.ast$DomRate),])/lat

PO1.ast$spline <- unlist(lapply(PO1.ast$time, FUN = dd))
(PO1.ast$Dom0 - PO1.ast$spline)^2

apply(1:nrow(PO1.ast), FUN = function(i) integrate(dd, lower = PO1.ast$time[i], upper = PO1.ast$time[i+1]))

