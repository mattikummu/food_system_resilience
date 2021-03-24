
# dt <- t1
# n_b = 50
# start_col = 2
myFun_Bootstrap_MIX_v2<-function(dt,n_b,start_col){
    # store id
    x <- data.frame(dt[1])
    # store timeseries
    t_data1 <- dt[start_col:length(dt)] 
    
    # store time
    t_time1 <- c(1:length(t_data1))
    
    
    
    # prepare data
    
    xy1 <- data.frame(cbind(t_time1,t_data1))
    colnames(xy1) <- c("yr","indic")
    
    # pearson's footstrap
    
    # define function
    pearson <- function(d,i=c(1:n)){
      d2 <- d[i,]
      return(cor(d2$yr,d2$indic))
    }
    
    # calculate 
    if (is.na(t_data1[1])) {
      x[1,2] <- data.frame(NA)
      x[1,3] <- data.frame(NA)
    } else if (abs(max(t_data1) - min(t_data1))==0) {
      x[1,2] <- data.frame(0)
      x[1,3] <- data.frame(0)
    } else if (sum(t_data1, na.rm=T) == 0) {
      x[1,2] <- data.frame(0)
      x[1,3] <- data.frame(0)
    } else if (mean(t_data1, na.rm=T) == 1) {
      x[1,2] <- data.frame(0)
      x[1,3] <- data.frame(0)
    } else { 
      linearMod1 <- lm(indic ~ yr, data=xy1) 
      x[1,2] <- linearMod1$coefficients[2] * length(t_data1)
      
      t_res.test1 <- trend.test(t_data1, R=n_b)
      
      x[1,3] <- data.frame(t_res.test1$p.value)
    }
   
    
  return(x)
}