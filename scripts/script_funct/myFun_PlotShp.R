myPlotShp<-function(dt,fl,limits,col_opt){
  
  # quo_col_opt <- enquo(col_opt)
  
  quo_fl <- enquo(fl)
  
  lim_min = ifelse(min(dt$yr1989,na.rm = T) < 0,-1,0)
  lim_max = 1
  pl <- ggplot() +
    geom_sf(aes(fill = !! quo_fl), data = dt, colour = "grey80", lwd = 0.1)+
    
    # scale_fill_viridis(option= "magma", direction = -1, na.value = "grey20", 
    #                    rescaler = function(x, to = c(0, 1), from = NULL) {
    #                      ifelse(x<1.2, 
    #                             scales::rescale(x,
    #                                             to = to,
    #                                             from = c(min(x, na.rm = TRUE), 1)),
    #                             1)}) +
    
    scale_fill_gradientn(colors = viridis(10,option=col_opt ,direction=-1), space ="Lab" , na.value = "lightyellow2",
                         limits = c(0,1), oob=squish)+

    # oob=squish for values outside limits
    
    coord_sf(crs = "+proj=eqearth +wktext") +
    ggtitle(quo_fl)+
    theme_minimal() +
    theme(axis.text = element_blank()) +
    theme(legend.position="bottom")
  return(pl)
}
