tds.subset = function(da, factA, factB, samp, n,...) {
  #da - the dataframe
  #factA -- "wine"
  #factB -- "taste aattribute"
  # n #of evaluations
  # THIS FUNCTION IS SPECIFIC FOR THE EM WINES
  a1 = da[factA == samp & factB == "Astringent", ]
  a2 = da[factA == samp & factB == "Bitter",]
  a3 = da[factA == samp & factB == "Sweet",]
  a4 = da[factA == samp & factB == "Sour",]
  a5 = da[factA == samp & factB == "Hot",]
  
  t1.freq = data.frame(time = seq(0, 90, 1),
                       frequency = colSums(a1[17:length(a1)]),
                       DomRate = (colSums(a1[17:length(a1)]))/n,
                       taste = rep("Astringent", 91, row.names = seq(0,90)))
  
  t2.freq = data.frame(time = seq(0, 90, 1),
                       frequency = colSums(a2[17:length(a2)]),
                       DomRate = (colSums(a2[17:length(a2)]))/n,
                       taste = rep("Bitter", 91, row.names = seq(0,90)))
  
  t3.freq = data.frame(time = seq(0, 90, 1),
                       frequency = colSums(a3[17:length(a3)]),
                       DomRate = (colSums(a3[17:length(a3)]))/n,
                       taste = rep("Sweet", 91, row.names = seq(0,90)))
  
  t4.freq = data.frame(time = seq(0, 90, 1),
                       frequency = colSums(a4[17:length(a4)]),
                       DomRate = (colSums(a4[17:length(a4)]))/n,
                       taste = rep("Sour", 91, row.names = seq(0,90)))
  
  t5.freq = data.frame(time = seq(0, 90, 1),
                       frequency = colSums(a5[17:length(a5)]),
                       DomRate = (colSums(a5[17:length(a5)]))/n,
                       taste = rep("Hot", 91, row.names = seq(0,90)))
  
  out = rbind(t1.freq, t2.freq, t3.freq, t4.freq, t5.freq)
  
  
}
  
 