myPopCount<-function(pop_data,trend_data){
  
  # trend_data <- d_res_trend
  # pop_data <- d_cntry_pop
  
  d_pop_count <- merge(trend_data,pop_data,by='fao_id', all = TRUE)  %>% 
     mutate(trend_sign = 0)
  
  
  # examine the trend direction
  t_neg <- d_pop_count$ratio_2nd < 0 # & d_pop_count$p_value_2nd < 0.1
  t_pos <- d_pop_count$ratio_2nd > 0 # & d_pop_count$p_value_2nd < 0.1
  t_na <- is.na(d_pop_count$ratio_2nd) | is.na(d_pop_count$country)
  
  d_pop_count$trend_sign[t_neg] = -1
  d_pop_count$trend_sign[t_pos] = 1
  d_pop_count$trend_sign[t_na] = -99
  
  # remove potential dublicates
  d_pop_count <- d_pop_count[!duplicated(d_pop_count), ]
  
  t_sum <- d_pop_count %>% 
    group_by(trend_sign) %>% 
    summarise( n(),
               sum_pop = sum(pop2013, na.rm = T))
  
  return(t_sum)
}
