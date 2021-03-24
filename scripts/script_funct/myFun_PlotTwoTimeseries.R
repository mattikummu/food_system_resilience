## plot div_supply by countries
myPlotTwoTimeseries<-function(dt,dt2){
  
  name_s <- colnames(dt[5])
  name_e <- colnames(dt[ncol(dt)])
  
  x <- dt %>% gather("year","value",name_s[]:name_e[],na.rm = T)
  x2 <- dt2 %>% gather("year","value",name_s[]:name_e[],na.rm = T)
  # year column to numeric value
  x$year <- as.numeric(str_extract(x$year, "[0-9]+"))
  x2$year <- as.numeric(str_extract(x$year, "[0-9]+"))
  # list to store plots
  p <- list()
  
  for (i in 1:nrow(dt)) {
    cntry <- dt[i,]$id
    data_cntry <- x[x$id == cntry, ] 
    data_cntry2 <- x2[x2$id == cntry, ] 
    
    p[[i]] <- ggplot() +
      geom_line(data=data_cntry, aes(x=year,y=value)) +
      geom_line(data=data_cntry2, aes(x=year,y=value),linetype="dotted", color = "red")+
      
      ylab("Index") +
      ggtitle(dt[i,]$cntry)+
      
      scale_x_continuous(breaks = seq(1963, 2014, by = 10)) +
      ylim(0, 1)+
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