#### TDS #######

save(list = "tds", file = "tds.Rdata")




PO1 = tds.subset(da = tds, factA = tds$Wine, factB = tds$Attribute, samp = "PO1", n = 60)
PO1.plot = EM.tds.graph(PO1, cline=0.2, sline=0.285, title="TDS PO1")
PO1.plot
library(splines)
PO1.plot = EM.tds.graph(PO1, cline=0.2, sline=0.285, title="TDS PO1")


PO1_A <- tds.subset(da = tds, factA = tds$Bottle, factB = tds$Attribute, samp = "PO1_A", n = 20)
PO1_B <- tds.subset(da = tds, factA = tds$Bottle, factB = tds$Attribute, samp = "PO1_B", n = 20)
PO1_C <- tds.subset(da = tds, factA = tds$Bottle, factB = tds$Attribute, samp = "PO1_C", n = 20)

PO1_A.plot = EM.tds.graph(PO1_A, cline=0.2, sline=0.347, title="TDS PO1_A")
PO1_B.plot = EM.tds.graph(PO1_B, cline=0.2, sline=0.347, title="TDS PO1_B")
PO1_C.plot = EM.tds.graph(PO1_C, cline=0.2, sline=0.347, title="TDS PO1_C")


aa <- tdsExt(PO1_A, "Astringent", 0.347)
bb <- tdsExt(PO1_B, "Astringent", 0.347)
cc <- tdsExt(PO1_C, "Astringent", 0.347)

aa$ExtParm
bb$ExtParm
cc$ExtParm
