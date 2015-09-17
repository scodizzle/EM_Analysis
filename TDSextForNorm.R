## function to extract parameters from TDS curves
## takes data from the tds.subset function and pulls out 
##

tdsExt<- function(df, att, sigLine, ...){
  
  
  #df <- df[df$domRate >= sigLine & df$taste == att,] #subset for astringency
  df <- df[df$sensation == att,] #subset for astringency
  rownames(df) = 1:nrow(df)
  #df$Dom0 <- df$domRate - sigLine #subtract the sig line
  
  #create spline function
  sfx <- splinefun(df$time, y=df$domRate, method = "natural")
  
  #integrate stepwise
  is <- lapply(0:(nrow(df)-1), FUN = function(i) integrate(sfx, lower = i, upper = i+1))
  df$dxSpline <- unlist(lapply(1:length(is), FUN = function(i) if(is[[i]]$value < 0) {
    return(0)} else {return(is[[i]]$value)})) #dx...  DROPS values less than 0!!
  
  df$dxCulu <- cumsum(df$dxSpline) # culumlitive sum of the dx
  
  df$dxSigLine <- unlist(lapply(1:nrow(df), FUN = function(i) #account for the sigLine
    if(df$domRate[i] < sigLine) {return(0)} else {return(df$dxSpline[i] - sigLine)}))
  
  df$dxSigCulu <- cumsum(df$dxSigLine)
  

  #total parameters
  area <- df$dxSigCulu[nrow(df)]    #Area above sig line
  maxDom <- max(df$domRate) #max dom Rate 
  tAmax <- nrow(df[df$domRate == maxDom,]) #length of time AT max intensity 
  tmax <- sum(df[df$domRate == maxDom,]$time)/tAmax #time of max dom rate
  dur <- colSums(df !=0)[7] #time duration above the sig line
   
  #post Expecterate parameters
  #pe <- df[df$time > 25,] #subset to only have post expecterate
  #maxDomPe <- max(pe$domRate) #max post expt domRate
  #tmaxPe <- nrow(pe[pe$domRate == maxDomPe,]) #length of time at max intensity post exp  
  #tAmaxPe <- sum(pe[pe$domRate == maxDomPe,]$time)/tmaxPe #time of max dom rate post exp
  #areaPe <- sum(pe$dxSpline) #sum up the area
  #ddPe <- pe$time[nrow(pe)] - pe$time[1] #duration post Exp above sig line

  
  ExtParm <- cbind(area,maxDom,tAmax,tmax,dur)
  print(list(intg = df, ExtParm = ExtParm, spFUN = sfx))
  
}


