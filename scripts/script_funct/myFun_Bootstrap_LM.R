
# dt <- t1
# n_b = 50
# start_col = 2
myFun_Bootstrap_LM<-function(dt,n_b,start_col){
    # store id
    x <- data.frame(dt[1])
    # store timeseries
    t_data1 <- dt[start_col:(start_col+8)] 
    t_data2 <- dt[(start_col+9):(start_col+17)] 
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
    } else if (sum(t_data1) == 0) {
      x[1,2] <- data.frame(NA)
      x[1,3] <- data.frame(NA)
    } else if (mean(t_data1) == 1) {
      x[1,2] <- data.frame(NA)
      x[1,3] <- data.frame(NA)
    } else { 
      linearMod1 <- lm(indic ~ yr, data=xy1) 
      x[1,2] <- linearMod1$coefficients[2] * length(t_data1)
      
      
      bootc <- boot(data=xy1,statistic=pearson,R=n_b)
      
      # p value
      cvs <- quantile(bootc$t0 - bootc$t + 1, c(0.05, 0.95), na.rm = T)
      x[1,3] <- mean(bootc$t > cvs[1] & bootc$t < cvs[2])
    }
    
    if (is.na(t_data2[1])) {
      x[1,4] <- data.frame(NA)
      x[1,5] <- data.frame(NA)
    } else if (sum(t_data2) == 0) {
      x[1,4] <- data.frame(NA)
      x[1,5] <- data.frame(NA)
    } else if (mean(t_data2) == 1) {
      x[1,4] <- data.frame(NA)
      x[1,5] <- data.frame(NA)
    }  else { 
      linearMod2 <- lm(indic ~ yr, data=xy2) 
      x[1,4] <- linearMod2$coefficients[2] * length(t_data2)
      
      bootc2 <- boot(data=xy2,statistic=pearson,R=n_b)
      
      # p value
      cvs2 <- quantile(bootc2$t0 - bootc2$t + 1, c(0.05, 0.95), na.rm = T)
      x[1,5] <- mean(bootc2$t > cvs2[1] & bootc2$t < cvs2[2])
    }
    
  return(x)
}