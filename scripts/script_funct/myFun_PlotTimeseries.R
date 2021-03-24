## plot div_supply by countries
# dt <- d_div_prod
myPlotTimeseries<-function(dt){
  #dt[is.na(dt)] = 0
  name_s <- colnames(dt[5])
  name_e <- colnames(dt[ncol(dt)])
  x <- dt %>% gather("year","value",name_s[]:name_e[],na.rm = T)
  x$year <- as.numeric(str_extract(x$year, "[0-9]+"))
  p <- list()
  
  ylim_min = ifelse(min(x$value,na.rm = T) < 0,min(x$value,na.rm = T),0)
  ylim_max = ifelse(max(x$value,na.rm = T) > 1,max(x$value,na.rm = T),1)
  for (i in 1:nrow(dt)) {
    cntry <- dt[i,]$id
    data_cntry <- x[x$id == cntry, ] 
    
    p[[i]] <- ggplot(data=data_cntry, aes(x=year,y=value)) +
      geom_line() +
      ylab("Index") +
      ggtitle(dt[i,]$cntry)+
      
      scale_x_continuous(breaks = seq(1963, 2014, by = 10)) +
      ylim(ylim_min, ylim_max)+
      theme_tufte() +
      
      theme (
        panel.grid.major.y = element_line(size = 0.1, linetype = 'solid',colour = "black"),
        plot.title = element_text(size=10),
        axis.ticks = element_line(size = 0.1, linetype = 'solid',
                                  colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
      )
  }
  return(p)
}