myTrend_MIX<-function(dt,n_boot,id_col,start_column){
  
  # dt = d_import_partners_3yr_n
  # n_boot = 99
  # start_column <- 5
  # id_col = 2
  
  dt_trend <- as.data.frame(dt[,c(id_col,start_column:(start_column+1))])
  dt_trend[!is.na(dt_trend)] <- NA 
  
  library(pastecs)
  
  source("script_funct/myFun_Bootstrap_MIX_v2.R")
  
  set.seed(21)
  
  #i =1
  for(i in 1:nrow(dt)){
    
    # to numeric
    
    t1 <- as.numeric(dt[i,c(id_col,start_column:ncol(dt))])
    t1[is.na(t1)] = 0
    # bootstrap
    t_res <- myFun_Bootstrap_MIX_v2(t1,n_boot,2)
    dt_trend[i,] <- data.frame(t_res)
    #dt_trend = clusterR(t1, myFun_Bootstrap)
    
  }
  
  colnames(dt_trend) = c("fao_id","ratio_2nd","p_value_2nd")
  dt_trend <- as.tibble(dt_trend)
  
  dt_trend$ratio_2nd[dt_trend$p_value_2nd > 0.1] <- 0
  
  
  return(dt_trend)
}
