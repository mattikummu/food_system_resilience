myRemoveDubl<-function(dt1,dt2,pop_data){
  
  # combine the layers
  dt_comb <- merge.data.frame(as.data.frame(dt1),as.data.frame(dt2),by='fao_id',all=T)
  
  # combine with the population data
  dt_comb <- merge.data.frame(as.data.frame(dt_comb),as.data.frame(pop_data),by='fao_id',all=T)
  
  # identify rows without fao_id
  t_r <- which(grepl(-99, dt_comb$fao_id))
  # remove those rows
  if(length(t_r)==0){
  } else {
    dt_comb <- dt_comb[(t_r[length(t_r)]+1):nrow(dt_comb),]    
    }
  
  
  # remove dublicates
  dt_comb <- dt_comb[!duplicated(dt_comb), ]
  
  # include only those where fao_id is larger than 0
  dt_comb <- subset(dt_comb, fao_id > 0)
  
  return(dt_comb)
}
