
# normalise between 0 and 1

# dt = d_import_partners_3yr
# start_column = 5
# meth = 2

myNormaliseQ<-function(dt,start_column,meth){
  x <- as.data.frame(dt)
  st_col <- start_column
  # quantiles
  xQ <- quantile(x[,st_col:ncol(x)],probs=c(0.01,0.025,0.5,0.975,0.99),na.rm=T)
  xM <- min(x[,st_col:ncol(x)],na.rm=T)
  xM[2] <- max(x[,st_col:ncol(x)],na.rm=T)
  
  if (meth == 1) {
  # normalise between min and max 
  xN <- (x[,st_col:ncol(x)] - xM[1])/(xM[2] - xM[1])
  }
  else{
    # normalise between 2.5 and 97.5 quantiles 
    xN <- (x[,st_col:ncol(x)] - xQ[2])/(xQ[4] - xQ[2])
    
  }
  
  # less than 0 to 0 and more than 1 to 1
  xN[xN < 0] = 0
  xN[xN > 1] = 1
  # join the country names
  x[,st_col:ncol(x)] <- xN
  x <- as.tibble(x)
  return(x)
}