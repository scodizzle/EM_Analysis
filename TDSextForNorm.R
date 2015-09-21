## function to extract parameters from TDS curves
## takes data from the tds.subset function and pulls out 
## 

tdsExtNORM<- function(dat, att, sigLine, df, ...){
  
  dat <- dat[dat$sensation == att,] #subset for astringency
  rownames(dat) = 1:nrow(dat)
  
  dat.fit <- lm(dat$domRate ~ ns(dat$time, df), data=dat) #model the data!! use lm an  spline 
  sfx <- splinefun(1:100, y=dat.fit$fitted.values , method = "natural")
  
  #integrate stepwise
  is <- lapply(0:(nrow(dat)-1), FUN = function(i) integrate(sfx, lower = i, upper = i+1))
  dat$dxSpline <- unlist(lapply(1:length(is), FUN = function(i) if(is[[i]]$value < 0) {
    return(0)} else {return(is[[i]]$value)})) #dx...  DROPS values less than 0!!
  
  dat$dxCulu <- cumsum(dat$dxSpline) # culumlitive sum of the dx
  
  # account for the sigLine by subtracting from the dxSpline integration, not the area of 1 sec under the 
  # sigLine is the same valus as the sigLine  :-)
  dat$dxSigLine <- unlist(lapply(1:nrow(dat), FUN = function(i) 
    if(dat$dxSpline[i] < sigLine) {return(0)} else {return(dat$dxSpline[i] - sigLine)}))
  
  dat$dxSigCulu <- cumsum(dat$dxSigLine)
  

  #total parameters
  area <- dat$dxSigCulu[nrow(dat)]    #Area above sig line
  maxDom <- max(dat$domRate) #max dom Rate 
  tAmax <- nrow(dat[dat$domRate == maxDom,]) #length of time AT max intensity 
  tmax <- sum(dat[dat$domRate == maxDom,]$time)/tAmax #time of max dom rate
  dur <- colSums(dat !=0)[7] #time duration above the sig line
   
  #post Expecterate parameters
  #pe <- dat[dat$time > 25,] #subset to only have post expecterate
  #maxDomPe <- max(pe$domRate) #max post expt domRate
  #tmaxPe <- nrow(pe[pe$domRate == maxDomPe,]) #length of time at max intensity post exp  
  #tAmaxPe <- sum(pe[pe$domRate == maxDomPe,]$time)/tmaxPe #time of max dom rate post exp
  #areaPe <- sum(pe$dxSpline) #sum up the area
  #ddPe <- pe$time[nrow(pe)] - pe$time[1] #duration post Exp above sig line

  
  ExtParm <- data.frame(area,maxDom,tAmax,tmax,dur, row.names=1)
  print(list(intg = dat, ExtParm = ExtParm, spFUN = sfx))
  
}

tdsExtNORM(test, att = "Bitter", sigLine = 0.297, df = 18)

