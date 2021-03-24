myPlotShp_v2_summary<-function(dt,fl,lims){
  
  # quo_col_opt <- enquo(col_opt)
  
  quo_fl <- enquo(fl)
  #pal = scico(9, direction = -1, palette = 'lajolla')
  pal = viridis(9, direction = -1, option = "E")
  pl <- ggplot() +
    geom_sf(aes(fill = !! quo_fl), data = dt, colour = "grey80", lwd = 0.1)+
    
    scale_fill_gradient(low = pal[1], high = pal[9],  space = "Lab",
      na.value = "lightyellow2", limits = lims, oob=squish) +
    
    coord_sf(crs = "+proj=eqearth +wktext") +
    ggtitle(quo_fl)+
    theme_minimal() +
    theme(axis.text = element_blank()) +
    theme(legend.position="bottom")
  return(pl)
}
