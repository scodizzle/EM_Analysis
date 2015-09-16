## function to extract parameters from TDS curves
## takes data from the tds.subset function and pulls out 
##

tdsExt <- function(df, att, sigLine, ...){
  
  
  df.e <- df[df$DomRate >= sigLine & df$taste == att,] #subset for astringency
  rownames(df.e) = 1:nrow(df.e)
  df.e$Dom0 <- df.e$DomRate - sigLine #subtract the sig line
  
  #create spline function
  #other ways to do this, maybe alter the method....
  dd <- splinefun(x=df.e$time, y=df.e$Dom0, method = "natural")
  
  #integrate stepwise
  idx <- nrow(df.e)-1
  is <- lapply(1:idx, FUN = function(i) integrate(dd, lower = df.e$time[i], upper = df.e$time[i+1]))
  df.e$area <- c(0,unlist(lapply(1:idx, FUN = function(i) is[[i]]$value))) #add to the df
  df.e$areaCulm <- cumsum(df.e$area) #culumlitive sum -- add to the df
  
  #total parameters
  maxDomT <- max(df.e$DomRate) #max dom Rate
  areaT <- df.e$areaCulm[nrow(df.e)] #total area under curve
  ddT <- df.e$time[nrow(df.e)] - df.e$time[1] #time duration above the sig line
  tmaxT <- nrow(df.e[df.e$DomRate == max(df.e$DomRate),]) #length of time at max intensity  
  tAmaxT <- sum(df.e[df.e$DomRate == max(df.e$DomRate),]$time)/tmaxT #time of max dom rate
  
  #post Expecterate parameters
  pe <- df.e[df.e$time > 25,] #subset to only have post expecterate
  maxDomPe <- max(pe$DomRate) #max post expt DomRate
  areaPe <- sum(pe$area) #sum up the area
  ddPe <- pe$time[nrow(pe)] - pe$time[1] #duration post Exp above sig line
  tmaxPe <- nrow(pe[pe$DomRate == max(pe$DomRate),]) #length of time at max intensity post exp  
  tAmaxPe <- sum(pe[pe$DomRate == max(pe$DomRate),]$time)/tmaxPe #time of max dom rate post exp
  
  ExtParm <- cbind(maxDomT, maxDomPe, areaT, areaPe, ddT, ddPe, tmaxT, tmaxPe, tAmaxT, tAmaxPe)
  print(list(intg = df.e, ExtParm = ExtParm))
  
}

tdsExt(df=PO1, att="Astringent", sigLine = 0.285)

