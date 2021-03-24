## Clustering for resilience work

library(tidyverse)
library(tibble)
library(sf)
library(sp)

library(RColorBrewer)
library(scales)

library(scico)
library(viridis)

library(zoo)

require(easycsv)
require("data.table")

library(ggplot2)
library(ggthemes) 
library(patchwork)

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(21)
# 
# own functions

source("script_funct/myFun_PlotShp.R")
source("script_funct/myFun_PlotShp_v2.R")
source("script_funct/myFun_PlotShp_2.R")


t_comb_data <- read.csv('../output/comb_data_v10.csv') %>% 
  as_tibble(.) %>% 
  drop_na(.) # remove countries with some NA values


t_comb_data_kcal <- t_comb_data[,c(1:2,c(seq(4,(4+9),by=3),seq(5,(5+9),by=3)))]
t_comb_data_proteins <- t_comb_data[,c(1:2,c(seq(16,(16+9),by=3),seq(17,(17+9),by=3)))]
t_comb_data_fats <- t_comb_data[,c(1:2,c(seq(28,(28+9),by=3),seq(29,(29+9),by=3)))]
t_comb_data_veggies <- t_comb_data[,c(1:2,c(seq(40,(40+9),by=3),seq(41,(41+9),by=3)))]


# read shp files
shp_cntrySml <- st_read("../input_data/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>% 
  rmapshaper::ms_simplify(., keep = 0.05, keep_shapes = T) %>%
  st_as_sf()

shp_cntrySml[,2:7] = NULL


myFun_cluster <-function(data){
  #data = t_comb_data_kcal
  # clusters using present resilience index
  clusters_res <- kmeans(data[,c(3:6)], 6, iter.max = 100)
  
  clusters_change <- kmeans(data[,c(7:10)], 6, iter.max = 100)
  
  t_cluster_centres_res <- as_tibble(clusters_res$centers) %>% 
    mutate(cluster_id = 1:n()) %>% 
    gather(.,key = parameter,value,-cluster_id)
  
  t_cluster_centres_change <- as_tibble(clusters_change$centers) %>% 
    mutate(cluster_id = 1:n()) %>% 
    gather(.,key = parameter,value,-cluster_id) %>% 
    mutate(bool = case_when(value < 0 ~ -1, 
                            value > 0 ~ 1, 
                            TRUE ~ 0))
  
  # define levels
  t_cluster_centres_res$cluster_id <- as.factor(t_cluster_centres_res$cluster_id)
  t_cluster_centres_res$cluster_id <- factor(t_cluster_centres_res$cluster_id, levels = as.factor(6:1))
  t_cluster_centres_change$cluster_id <- as.factor(t_cluster_centres_change$cluster_id)
  t_cluster_centres_change$cluster_id <- factor(t_cluster_centres_change$cluster_id, levels = as.factor(6:1))
  #levels(temp_trend_top$admin)
  
  t_cluster_centres_res$parameter <- as.factor(t_cluster_centres_res$parameter)
  t_cluster_centres_res$parameter  <- factor(t_cluster_centres_res$parameter , levels = as.factor(colnames(clusters_res$centers)))
  t_cluster_centres_change$parameter <- as.factor(t_cluster_centres_change$parameter)
  t_cluster_centres_change$parameter  <- factor(t_cluster_centres_change$parameter , levels = as.factor(colnames(clusters_change$centers)))
  
  t_cluster_centres_change$bool <- as.factor(t_cluster_centres_change$bool)
  
  #write_excel_csv(t_cluster_centres,"cluster_centres_v5.csv")
  
  t_clust <- as_tibble( data[,1:2] )
  t_clust[,3] <- as_tibble(clusters_res$cluster)
  t_clust[,4] <- as_tibble(clusters_change$cluster)
  colnames(t_clust) <-c("fao_id","admin","cluster_res","cluster_change")
  
  
  # combine data from our calculations to the shp file
  shp_clusters <- left_join(shp_cntrySml,t_clust[,c(1,3:ncol(t_clust))],by='fao_id')
  
  # save geopackage
  
  # plot clusters
  
  p_cluster_res <- ggplot() +
    geom_sf(aes(fill = factor(cluster_res)), data = shp_clusters, colour = "grey80", lwd = 0.1)+
    scale_fill_brewer(palette = "Accent", na.value = "lightyellow2",name="clusters (resilience)")+
    coord_sf(crs = "+proj=eqearth +wktext") +
    #ggtitle("clusters")+
    theme_minimal() +
    theme(axis.text = element_blank()) +
    theme(legend.position="bottom")
  
  p_cluster_change <- ggplot() +
    geom_sf(aes(fill = factor(cluster_change)), data = shp_clusters, colour = "grey80", lwd = 0.1)+
    scale_fill_brewer(palette = "Accent", na.value = "lightyellow2",name="clusters (change)")+
    coord_sf(crs = "+proj=eqearth +wktext") +
    #ggtitle("clusters")+
    theme_minimal() +
    theme(axis.text = element_blank()) +
    theme(legend.position="bottom")
  
  p_cluster_res_centres <- ggplot(t_cluster_centres_res, aes(x=parameter, y=cluster_id)) +
    geom_point(aes(fill=value), size=7, shape=22)+
    
    scale_fill_gradientn(colors = viridis(10,option="E" ,direction=-1), space ="Lab" , na.value = "lightyellow2",
                         limits = c(0.2,0.9), oob=squish) +
    theme_tufte()+
    theme(axis.title=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "none",
          text = element_text(size=13))+
    scale_x_discrete(position = "top") +
    scale_size_identity()
  
  # ggplot(t_cluster_centres_res, aes(parameter, cluster_id, label = round(value,digits=2), colour = value)) +
  # geom_text()+
  # scale_colour_scico(begin=0.2, end = 0.8, direction = -1, palette ="berlin")+
  
  
  p_cluster_change_centres <- ggplot(t_cluster_centres_change, aes(x=parameter, y=cluster_id, group=bool)) +
    geom_point(aes(shape=bool, fill=value), size=4.5)+
    scale_shape_manual(values=c(25, 24))+
    scale_fill_gradient2(low = muted("red"), mid = "white",
                         high = muted("blue"),limits=c(-0.2,0.2),oob=squish)+
    theme_tufte()+
    theme(axis.title=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position = "none",
          text = element_text(size=13))+
    scale_x_discrete(position = "top") +
    scale_size_identity()
  
  # ggplot(t_cluster_centres_change, aes(parameter, cluster_id, label = round(value,digits=2), colour = value)) +
  #   geom_text()+
  #   scale_colour_scico(begin=0.2, end = 0.8, direction = -1, palette ="berlin")+
  
  
  p_cluster <- p_cluster_res + p_cluster_change + p_cluster_res_centres + p_cluster_change_centres + plot_layout(ncol = 2, width = c(1, 1))
  return(p_cluster)
}

p_kcal <- myFun_cluster(t_comb_data_kcal)
p_prot <- myFun_cluster(t_comb_data_proteins)
p_fats <- myFun_cluster(t_comb_data_fats)
p_veggies <- myFun_cluster(t_comb_data_veggies)

w <-20
h <- 15
ggsave("../output_figures/fig6a_cluster_kcal.pdf",p_kcal,width = w, height = h,units = "cm")
ggsave("../output_figures/fig6b_cluster_prot.pdf",p_prot,width = w, height = h,units = "cm")
ggsave("../output_figures/fig6c_cluster_fats.pdf",p_fats,width = w, height = h,units = "cm")
ggsave("../output_figures/fig6d_cluster_veggies.pdf",p_veggies,width = w, height = h,units = "cm")


# save geopackages for each clustering

myFun_cluster_shp <-function(data){
  #data = t_comb_data_kcal
  # clusters using present resilience index
  clusters_res <- kmeans(data[,c(3:6)], 6, iter.max = 100)
  
  clusters_change <- kmeans(data[,c(7:10)], 6, iter.max = 100)
  
  t_cluster_centres_res <- as_tibble(clusters_res$centers) %>% 
    mutate(cluster_id = 1:n()) %>% 
    gather(.,key = parameter,value,-cluster_id)
  
  t_cluster_centres_change <- as_tibble(clusters_change$centers) %>% 
    mutate(cluster_id = 1:n()) %>% 
    gather(.,key = parameter,value,-cluster_id) %>% 
    mutate(bool = case_when(value < 0 ~ -1, 
                            value > 0 ~ 1, 
                            TRUE ~ 0))
  
  # define levels
  t_cluster_centres_res$cluster_id <- as.factor(t_cluster_centres_res$cluster_id)
  t_cluster_centres_res$cluster_id <- factor(t_cluster_centres_res$cluster_id, levels = as.factor(6:1))
  t_cluster_centres_change$cluster_id <- as.factor(t_cluster_centres_change$cluster_id)
  t_cluster_centres_change$cluster_id <- factor(t_cluster_centres_change$cluster_id, levels = as.factor(6:1))
  #levels(temp_trend_top$admin)
  
  t_cluster_centres_res$parameter <- as.factor(t_cluster_centres_res$parameter)
  t_cluster_centres_res$parameter  <- factor(t_cluster_centres_res$parameter , levels = as.factor(colnames(clusters_res$centers)))
  t_cluster_centres_change$parameter <- as.factor(t_cluster_centres_change$parameter)
  t_cluster_centres_change$parameter  <- factor(t_cluster_centres_change$parameter , levels = as.factor(colnames(clusters_change$centers)))
  
  t_cluster_centres_change$bool <- as.factor(t_cluster_centres_change$bool)
  
  #write_excel_csv(t_cluster_centres,"cluster_centres_v5.csv")
  
  t_clust <- as_tibble( data[,1:2] )
  t_clust[,3] <- as_tibble(clusters_res$cluster)
  t_clust[,4] <- as_tibble(clusters_change$cluster)
  colnames(t_clust) <-c("fao_id","admin","cluster_res","cluster_change")
  
  
  # combine data from our calculations to the shp file
  shp_clusters <- left_join(shp_cntrySml,t_clust[,c(1,3:ncol(t_clust))],by='fao_id')
  
  # save geopackage
  return(shp_clusters)
}

shp_kcal <- myFun_cluster_shp(t_comb_data_kcal)
shp_prot <- myFun_cluster_shp(t_comb_data_proteins)
shp_fats <- myFun_cluster_shp(t_comb_data_fats)
shp_veggies <- myFun_cluster_shp(t_comb_data_veggies)

# save geopackage
st_write(shp_kcal,"../output/clusters_kcal.gpkg",layer_options = 'OVERWRITE=YES', append = TRUE)
st_write(shp_prot,"../output/clusters_prot.gpkg",layer_options = 'OVERWRITE=YES', append = TRUE)
st_write(shp_fats,"../output/clusters_fats.gpkg",layer_options = 'OVERWRITE=YES', append = TRUE)
st_write(shp_veggies,"../output/clusters_veggies.gpkg",layer_options = 'OVERWRITE=YES', append = TRUE)

