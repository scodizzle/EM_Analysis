## function to extract parameters from TDS curves
## takes data from the tds.subset function and pulls out 
##

tdsExt <- function(df, att, sigLine, ...){
  
  
  df.e <- df[df$DomRate >= sigLine & df$taste == att,] #subset for astringency
  df.e$Dom0 <- df.e$DomRate - sigLine #subtract the sig line
  
  #create spline function
  #other ways to do this, maybe alter the method....
  dd <- splinefun(x=df.e$time, y=df.e$Dom0, method = "natural")
  
  #integrate stepwise
  intg <- lapply(1:(nrow(df.e)-1), FUN = function(i) integrate(dd, lower = df.e$time[i], upper = df.e$time[i+1]))
  
  df.e$area <- c(0,unlist(lapply(1:43, FUN = function(i) intg[[i]]$value))) #add to the df
  
  df.e$areaCulm <- cumsum(df.e$area) #culumlitive sum -- add to the df
  
  print(df.e)
  
  
}

tdsExt(df=PO1, att="Astringent", sigLine = 0.285)
nx
