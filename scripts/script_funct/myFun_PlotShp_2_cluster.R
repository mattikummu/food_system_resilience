myPlotShp_2_summary<-function(dt,fl,lims){
  
  # quo_col_opt <- enquo(col_opt)
  
  quo_fl <- enquo(fl)
  
  lim_min = ifelse(min(dt$yr1989,na.rm = T) < 0,-1,0)
  lim_max = 1
  pl <- ggplot() +
    geom_sf(aes(fill = !! quo_fl), data = dt, colour = "grey80", lwd = 0.1)+
    
    scale_fill_gradient2(low = muted("red"), mid = "white",
      high = muted("blue"), midpoint = 0, space = "Lab",
      na.value = "lightyellow2", limits = lims, oob=squish) +
    
    coord_sf(crs = "+proj=eqearth +wktext") +
    ggtitle(quo_fl)+
    theme_minimal() +
    theme(axis.text = element_blank()) +
    theme(legend.position="bottom")
  return(pl)
}
