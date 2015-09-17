## function to extract parameters from TDS curves
## takes data from the tds.subset function and pulls out 
##

tdsExt.2 <- function(df, att, sigLine, ...){
  
  
  #df.e <- df[df$DomRate >= sigLine & df$taste == att,] #subset for astringency
  df.e <- df[df$taste == att,] #subset for astringency
  rownames(df.e) = 1:nrow(df.e)
  #df.e$Dom0 <- df.e$DomRate - sigLine #subtract the sig line
  
  #create spline function
  #other ways to do this, maybe alter the method....
  #dd <- splinefun(x=df.e$time, y=df.e$Dom0, method = "natural")
  sfx <- splinefun(df.e$time, y=df.e$DomRate, method = "natural")
  
  #integrate stepwise
  idx <- nrow(df.e)-1
  is <- lapply(0:90, FUN = function(i) integrate(sfx, lower = i, upper = i+1))
  df.e$dxSpline <- unlist(lapply(1:91, FUN = function(i) if(is[[i]]$value < 0) {
    return(0)} else {return(is[[i]]$value)})) #dx...  DROPS values less than 0!!
  
  df.e$dxCulu <- cumsum(dxSpline) # culumlitive sum of the dx
  
  df.e$dxSigLine <- unlist(lapply(1:91, FUN = function(i) #account for the sigLine
    if(df.e$DomRate[i] < sigLine) {return(0)} else {return(df.e$dxSpline[i] - sigLine)}))
  

  #total parameters
  maxDomT <- max(df.e$DomRate) #max dom Rate 
  tmaxT <- nrow(df.e[df.e$DomRate == maxDomT,]) #length of time at max intensity 
  tAmaxT <- sum(df.e[df.e$DomRate == maxDomT,]$time)/tmaxT #time of max dom rate
  areaT <- df.e$SplineCulm[nrow(df.e)] #total area under curve
  ddT <- df.e$time[nrow(df.e)] - df.e$time[1] #time duration above the sig line
   
  #post Expecterate parameters
  pe <- df.e[df.e$time > 25,] #subset to only have post expecterate
  maxDomPe <- max(pe$DomRate) #max post expt DomRate
  tmaxPe <- nrow(pe[pe$DomRate == maxDomPe,]) #length of time at max intensity post exp  
  tAmaxPe <- sum(pe[pe$DomRate == maxDomPe,]$time)/tmaxPe #time of max dom rate post exp
  areaPe <- sum(pe$dxSpline) #sum up the area
  ddPe <- pe$time[nrow(pe)] - pe$time[1] #duration post Exp above sig line

  
  ExtParm <- cbind(maxDomT,tmaxT,tAmaxT,areaT,ddT, maxDomPe,tmaxPe,tAmaxPe,areaPe,ddPe)
  print(list(intg = df.e, ExtParm = ExtParm, spFUN = ss))
  
}

test <- tdsExt.2(df=PO1, att="Astringent", sigLine = 0.285)

