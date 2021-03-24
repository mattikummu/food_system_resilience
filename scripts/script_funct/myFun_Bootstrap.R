
# dt <- t1
# n_b = 50
# start_col = 2
myFun_Bootstrap<-function(dt,n_b,start_col){
    x <- data.frame(dt[1])
    t_data1 <- dt[start_col:(start_col+9)] 
    t_data2 <- dt[(start_col+10):(start_col+17)] 
    
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
      t_res.test1 <- trend.test(t_data1, R=n_b)
      x[1,2] <- data.frame(t_res.test1$t0)
      x[1,3] <- data.frame(t_res.test1$p.value)
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
      t_res.test2 <- trend.test(t_data2, R=n_b)
      x[1,4] <- data.frame(t_res.test2$t0)
      x[1,5] <- data.frame(t_res.test2$p.value)
    }
    
  return(x)
}