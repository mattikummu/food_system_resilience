
# dt <- t1
# n_b = 50
# start_col = 2
myFun_Bootstrap_MIX<-function(dt,n_b,start_col){
    # store id
    x <- data.frame(dt[1])
    # store timeseries
    t_data1 <- dt[start_col:(start_col+7)] 
    t_data2 <- dt[(start_col+8):(start_col+16)] 
    # store time
    t_time1 <- c(1:length(t_data1))
    t_time2 <- c(1:length(t_data2))
    
    
    # prepare data
    
    xy1 <- data.frame(cbind(t_time1,t_data1))
    colnames(xy1) <- c("yr","indic")
    
    
    xy2 <- data.frame(cbind(t_time2,t_data2))
    colnames(xy2) <- c("yr","indic")

    
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
    
    if (is.na(t_data2[1])) {
      x[1,4] <- data.frame(NA)
      x[1,5] <- data.frame(NA)
    } else if (abs(max(t_data2) - min(t_data2))==0) {
      x[1,4] <- data.frame(0)
      x[1,5] <- data.frame(0)
    } else if (sum(t_data2, na.rm=T) == 0) {
      x[1,4] <- data.frame(0)
      x[1,5] <- data.frame(0)
    } else if (mean(t_data2, na.rm=T) == 1) {
      x[1,4] <- data.frame(0)
      x[1,5] <- data.frame(0)
    }  else { 
      linearMod2 <- lm(indic ~ yr, data=xy2) 
      x[1,4] <- linearMod2$coefficients[2] * length(t_data2)
      
      t_res.test2 <- trend.test(t_data2, R=n_b)
      
      x[1,5] <- data.frame(t_res.test2$p.value)
    }
    
  return(x)
}