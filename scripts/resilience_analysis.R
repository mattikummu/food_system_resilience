
## code for Kummu et al (2020) Interplay of trade and food system resilience: 
## Gains on supply diversity over time at the cost of trade independency
## https://doi.org/10.1016/j.gfs.2020.100360

## contact: matti.kummu@aalto.fi


# read needed libraries

library(tidyverse)
library(tibble)
library(sf)
library(sp)
library(ggplot2)
library(ggthemes) 
library(RColorBrewer)
library(scales)
library(patchwork)
library(dplyr)

library(viridis)

library(zoo)

require(easycsv)
require("data.table")

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(21)
# 
# add source links to own functions
source("script_funct/myFun_Trend_MIX.R")
source("script_funct/myFun_Normalise.R")
source("script_funct/myFun_RemoveDublicates.R")
source("script_funct/myFun_popcount.r")

source("script_funct/myFun_PlotShp.R")
source("script_funct/myFun_PlotShp_v2.R")
source("script_funct/myFun_PlotShp_2.R")
source("script_funct/myFun_PlotShp_2_summary.R")

# read country shp file
shp_cntry <- st_read("../input_data/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp")
# simplify the shapefile
shp_cntrySml <- rmapshaper::ms_simplify(shp_cntry, keep = 0.05, keep_shapes = T) %>%
  st_as_sf()

# clear non-needed columns
shp_cntrySml[,2:7] = NULL


# Read resilience data, created by Pekka
directory = '../output/'
fname = c('normalized_shannon_index_kcal_food_supply.csv','normalized_shannon_index_prot_food_supply.csv',
          'normalized_shannon_index_fat_food_supply.csv','normalized_shannon_index_fruits_Food.csv',
          'normalized_shannon_index_kcal_production.csv', 'normalized_shannon_index_prot_production.csv',
          'normalized_shannon_index_fat_production.csv','normalized_shannon_index_fruits_Production.csv',
          'trade_ENS_kcal_by_cntry.csv','trade_ENS_prot_by_cntry.csv','trade_ENS_fat_by_cntry.csv','trade_ENS_fruits.csv')

# trade dependency by Miina
trade_dep <- read_sf('../output/mean_TD.csv')
temp_fruits <- read_sf('../output/mean_TD_fruits_veg.csv')
# combine datasets
trade_dep <- as_tibble(cbind(trade_dep,temp_fruits[,4]))
trade_dep$field_1 <- 1:nrow(trade_dep)


# country list
d_cntry_list <- read.csv('../input_data/cntry_fao_id.csv')
# clear not-needed columns
d_cntry_list[,2:10] <-  NULL

# load population data
d_cntry_pop <- read.csv('../input_data/UN_population.csv')
# select only the  wanted columns
d_cntry_pop <- data.frame(d_cntry_pop$Country,d_cntry_pop$FAO_id,d_cntry_pop$X1989,d_cntry_pop$X2013)
# rename columns
colnames(d_cntry_pop) <- c('country','fao_id','pop1989','pop2013')


# create empty result tables
t_combined_comb <- as_tibble(d_cntry_list[,c(2,1)])

names_meas <- c('prod','supp','indep','imp')
names_var <- c('kcal','prot','fat','veg')

d_pop_summary <- data.frame(matrix(ncol = 11, nrow = 16))
colnames(d_pop_summary) <- c('trend_sign','prod_div-n','prod_div-pop','supply_div-n','supply_div-pop','trade_dep-n',
                             'trade_dep-pop','import_partners-n','import_partners-pop'
                             ,'resilience-n','resilience-pop')

p_div_prod_no_legend <- list()
p_div_supply_no_legend <- list()
p_div_dep_no_legend <- list()
p_div_imp_no_legend <- list()

p_div_prod <- list()
p_div_supply <- list()
p_div_dep <- list()
p_div_imp <- list()

# go through each dataset (i.e. kcal, protein, fat, fruits) -------------------------------------------------

# NOTE: LONG LOOP - until row 455

for (i_data in 1:4) {
  
  d_div_supply <- read_sf( paste0(directory,fname[[i_data]] ))
  d_div_prod <- read_sf( paste0(directory,fname[[i_data+4]] ))
  d_import_partners <- read_sf( paste0(directory,fname[[i_data+8]]))
  
  #trade_dep$index<-1:nrow(trade_dep)
  t_d_trade_dep <- tidyr::spread(trade_dep[c(2,3,i_data+3)],Year,colnames(trade_dep[i_data+3]))
  
  colnames(t_d_trade_dep)[1] <- "fao_id"
  # base data to first columns
  t_base <- d_div_supply[,1:4]
  # merge
  d_trade_dep <- as_tibble( merge(t_base,t_d_trade_dep,by="fao_id") )
  # swap 1st and 2nd columns
  d_trade_dep <- d_trade_dep[,c(2,1,3:length(d_trade_dep))]
  
  
  t_names <- colnames(d_div_prod)
  
  
  # calculate three year average for TD
  
  # year breaks
  m <- 1986:2013
  breaks<- m[seq(1, length(m), 3)]
  rm(m)
  
  #create labels
  l <- paste("yr",breaks[1:length(breaks)],sep = "")
  
  
  # interpolate missing values of import partners
  
  # create empty table
  d_import_partners_interp <- d_import_partners 
  d_import_partners_interp[,5:ncol(d_import_partners)] = NA 
  
  d_import_partners_interp <- d_import_partners_interp %>% 
    mutate_at(vars(-c('field_1','fao_id','Area','subregion')), as.numeric)
  
  # fill missing values
  for(i in 1:nrow(d_import_partners)) {
    
    # interpolation
    t <- na.approx(as.numeric(d_import_partners[i,5:ncol(d_import_partners)]), na.rm = FALSE)
    # extrapolation - use closest observed value
    t_exp <- na.locf(t, na.rm=F)
    t_exp2 <- na.locf(t_exp, na.rm=F, fromLast=T)
    
    d_import_partners_interp[i,5:ncol(d_import_partners)] <- as.list(t_exp2)
    
  }
  
  d_import_partners_3yr = d_import_partners_interp
  
  
  # change labels
  cn <- c("id","fao_id","cntry","region")
  
  colnames(d_trade_dep) <- c(cn,l[2:length(l)])
  colnames(d_div_prod) <- c(cn,l[2:length(l)])
  colnames(d_div_supply) <- c(cn,l[2:length(l)])
  colnames(d_import_partners_3yr) <- c(cn,l[2:length(l)])
  
  
  # data to numeric
  
  d_trade_dep <- mutate(d_trade_dep, across(.cols = yr1989:yr2013, as.numeric))
  d_div_prod <- mutate(d_div_prod, across(.cols = yr1989:yr2013, as.numeric))
  d_div_supply <- mutate(d_div_supply, across(.cols = yr1989:yr2013, as.numeric))
  d_import_partners_3yr <- mutate(d_import_partners_3yr, across(.cols = yr1989:yr2013, as.numeric))
  
  
  
  
  # normalise between 0 and 1, by using 2.5 and 97.5 percentiles
  
  d_div_prod_n <- myNormaliseQ(d_div_prod,5,2)
  d_div_supply_n <- myNormaliseQ(d_div_supply,5,2)
  d_import_partners_3yr_n <- myNormaliseQ(d_import_partners_3yr,5,2)
  
  # for trade dependency another kind of normalising
  
  d_trade_dep_n <-  d_trade_dep
  
  for (i in 1:nrow(d_trade_dep)){
    t <- as.numeric(d_trade_dep[i,5:ncol(d_trade_dep)])
    t1 <- t  / 2
    t1 <- t1  + 0.5
    
    t1[t1 > 1] <- 1
    t1[t1 < 0] <- 0
    
    # trade dependency so that 1 is net exporting and 0 net importing (now opposite)
    t2 <- 1-t1
    d_trade_dep_n[i,5:ncol(d_trade_dep)] <- as.list(t2)
  }
  
  
  
  # calculate resilience
  
  d_res_indicators <- merge(d_cntry_list,d_div_prod_n,by='fao_id', all = TRUE) %>% 
    merge(d_div_supply_n,by='fao_id', all = TRUE) %>% 
    merge(d_import_partners_3yr_n,by='fao_id', all = TRUE) %>% 
    merge(d_trade_dep_n,by='fao_id', all = TRUE)
  
  # remove extra rows
  d_res_indicators <- d_res_indicators[1:241,] 
  
  # store new tibble with fao_id:s
  d_res <- tibble(d_res_indicators[,1])
  
  for( i in 1:(length(breaks)-1) ) {
    d_res[,i+1] <- tibble(rowMeans(d_res_indicators[,c(i+5,i+17,i+29,i+41)], na.rm = T))
  }
  
  # fao_id to numeric
  d_res[1] <- data.matrix(d_res[1])
  
  colnames(d_res) <- c('fao_id',l[2:length(l)])
  
  
  # estimate trend
  d_res_trend <- myTrend_MIX(d_res,1000,1,2)
  d_div_prod_trend <- myTrend_MIX(d_div_prod_n,1000,2,5)
  d_div_supply_trend <- myTrend_MIX(d_div_supply_n,1000,2,5)
  d_import_partners_trend <- myTrend_MIX(d_import_partners_3yr_n,1000,2,5)
  d_trade_dep_trend <- myTrend_MIX(d_trade_dep_n,1000,2,5)
  
  
  # combine layers and remove dublicates
  
  # population to numeric
  d_cntry_pop[3] <- data.matrix(d_cntry_pop[3])  
  
  # remove dublicates
  d_res_com <- myRemoveDubl(d_res,d_res_trend,d_cntry_pop)
  
  d_div_prod_comb <- myRemoveDubl(d_div_prod_n,d_div_prod_trend,d_cntry_pop)
  d_div_supply_comb <- myRemoveDubl(d_div_supply_n,d_div_supply_trend,d_cntry_pop) 
  d_trade_dep_comb <- myRemoveDubl(d_trade_dep_n,d_trade_dep_trend,d_cntry_pop)
  d_import_partners_comb <- myRemoveDubl(d_import_partners_3yr_n,d_import_partners_trend,d_cntry_pop)
  
  
  # find countries for which data exist
  t_data_exist <- subset(d_div_prod_comb, !is.na(ratio_2nd), select = fao_id) %>% 
    data.matrix() %>% 
    as_tibble()
  t_de_2 <- subset(d_div_supply_comb, !is.na(ratio_2nd), select = fao_id)%>% 
    data.matrix() %>% 
    as_tibble()
  t_de_3 <- subset(d_trade_dep_comb, !is.na(ratio_2nd), select = fao_id)%>% 
    data.matrix() %>% 
    as_tibble()
  t_de_4 <- subset(d_import_partners_comb, !is.na(ratio_2nd), select = fao_id)%>% 
    data.matrix() %>% 
    as_tibble()
  
  
  # then countries, for which all the indicators are available
  d_all_data_exist <- Reduce(intersect, list(t_data_exist,t_de_2,t_de_3,t_de_4))
  
  d_all_data_exist <- d_all_data_exist %>% 
    mutate(all_data = 1)
  
  # include only those where all indicators exist and data as well
  d_res_trend <- merge(d_res_trend,d_all_data_exist,by='fao_id', all = TRUE)
  d_res_trend <- subset(d_res_trend, !is.na(all_data) & !is.na(ratio_2nd))
  
  
  d_res_com <- merge(d_res_com,d_all_data_exist,by='fao_id', all = TRUE)
  d_res_com <- subset(d_res_com, !is.na(all_data) & !is.na(ratio_2nd))
  
  head(d_all_data_exist)
  
  # calculate population under different resilience classes
  
  t_res_classes <- data.frame(seq(0, 1, by=0.2))
  t_res <- data.frame(matrix(ncol = 4, nrow = 5))
  
  for(i in 1:5){
    t_res[i,1] <- sum(as.matrix( subset(d_res_com, yr1989 > t_res_classes[i,1] & yr1989 < t_res_classes[i+1,1], select = pop1989) ))
    t_res[i,2] <- sum(as.matrix( subset(d_res_com, yr2013 > t_res_classes[i,1] & yr2013 < t_res_classes[i+1,1], select = pop2013) ))
  }
  
  t_res[,3] <- t_res[,1] / sum(t_res[,1])
  t_res[,4] <- t_res[,2] / sum(t_res[,2])
  
  # in millions
  t_res[,1:2] <- t_res[,1:2]/1000
  
  t_res <- t_res[,c(1,3,2,4)]
  
  #write_excel_csv(t_res,"resilience_pop_summary_v10.csv")
  
  # highest and lowest absolute change
  
  
  # calculate population over different trend classes
  
  temp_d_pop_summary <- myPopCount(d_cntry_pop,d_div_prod_trend) 
  t2 <- myPopCount(d_cntry_pop,d_div_supply_trend)
  t3 <- myPopCount(d_cntry_pop,d_trade_dep_trend)
  t4 <- myPopCount(d_cntry_pop,d_import_partners_trend)
  
  t10 <- myPopCount(d_cntry_pop,d_res_trend)
  
  temp_d_pop_summary <- merge(temp_d_pop_summary,t2,by='trend_sign', all = TRUE)
  temp_d_pop_summary <- merge(temp_d_pop_summary,t3,by='trend_sign', all = TRUE)
  temp_d_pop_summary <- merge(temp_d_pop_summary,t4,by='trend_sign', all = TRUE)
  temp_d_pop_summary <- merge(temp_d_pop_summary,t10,by='trend_sign', all = TRUE)
  
  
  
  # collect data
  t_data <- (temp_d_pop_summary[,c(3,5,7,9,11)])
  # divide with total to get percentage of total population
  t_data_shr <- t_data / colSums(t_data)
  # label rows
  rownames(t_data) <- c('no_data','neg','no_change','pos')
  
  # head(t_data)
  
  # combine the trend classes
  
  t_comb <- merge(d_cntry_pop,d_div_prod_trend[,c(1,2)],by='fao_id', all = TRUE)
  t_comb <- merge(t_comb,d_div_supply_trend[,c(1,2)],by='fao_id', all = TRUE)
  colnames(t_comb) <- c('fao_id','cntry','pop1989','pop2013','prod_div','supply_div')
  
  t_comb <- merge(t_comb,d_trade_dep_trend[,c(1,2)],by='fao_id', all = TRUE)
  t_comb <- merge(t_comb,d_import_partners_trend[,c(1,2)],by='fao_id', all = TRUE) #%>% 
  # mutate(trend_sign_prod = 0) %>% 
  # mutate(trend_sign_suppl = 0) %>% 
  # mutate(trend_sign_dep = 0)  %>% 
  # mutate(trend_sign_imp = 0)
  
  colnames(t_comb) <- c('fao_id','cntry','pop1989','pop2013','chng_prod_div','chng_supply_div',
                        'chng_trade_dep','chng_import_partners')
  # head(t_comb)
  # # find the negative and positive signes
  # t_neg <- t_comb[,5:8] <0 
  # t_pos <- t_comb[,5:8] >0 
  # 
  # t_comb[,9:12][t_neg] = -1
  # t_comb[,9:12][t_pos] = 1
  
  
  # store data --------------------------------------------------------------
  
  d_pop_summary[(4*(i_data-1)+1):((4*(i_data-1)+4)),1:11] <- temp_d_pop_summary
  
  #head(d_div_prod_comb)
  #t_combined_comb[1:nrow(t_comb),1:4] <- t_comb[,1:4]
  t_combined_comb <- merge(t_combined_comb,d_div_prod_comb[,c(1,5,13,14)],by='fao_id')
  t_combined_comb <- merge(t_combined_comb,d_div_supply_comb[,c(1,5,13,14)],by='fao_id')
  t_combined_comb <- merge(t_combined_comb,d_trade_dep_comb[,c(1,5,13,14)],by='fao_id')
  t_combined_comb <- merge(t_combined_comb,d_import_partners_comb[,c(1,5,13,14)],by='fao_id')
  
  
  for (i_names in 1:4) {
    colnames(t_combined_comb)[(i_data-1)*12 + (i_names-1)*3 + 3:5] <- c(paste0(names_meas[i_names],'_',names_var[i_data],'_1989'),
                                                                        paste0(names_meas[i_names],'_',names_var[i_data],'_2013'),
                                                                        paste0(names_meas[i_names],'_',names_var[i_data],'_trend'))
  }
  head(t_combined_comb)
  
  
  # 
  # # combine data from our calculations to the shp file
  shp_div_prod <- merge(shp_cntrySml,d_div_prod_comb,by='fao_id')
  shp_div_supply <- merge(shp_cntrySml,d_div_supply_comb,by='fao_id')
  shp_import_partners <- merge(shp_cntrySml,d_import_partners_comb,by='fao_id')
  shp_trade_dep <- merge(shp_cntrySml,d_trade_dep_comb,by='fao_id')
  
  
  
  # plot in equal earth projection ------------------------------------------
  
  p_prod <- list()
  p_prod[[2]] <- myPlotShp_v2(shp_div_prod,yr1989,c(0,1),"E") + theme(legend.position = "none")
  p_prod[[3]] <- myPlotShp_v2(shp_div_prod,yr2013,c(0,1),"E") + theme(legend.position = "none")
  p_prod[[4]] <- myPlotShp_2(shp_div_prod,ratio_2nd,c(-0.3,0.3)) + theme(legend.position = "none")
  
  p_div_prod_no_legend[[i_data]] <- p_prod[[2]] + p_prod[[3]] + p_prod[[4]]
  
  
  p_supp <- list()
  p_supp[[2]] <- myPlotShp_v2(shp_div_supply,yr1989,c(0,1),"E") + theme(legend.position = "none")
  p_supp[[3]] <- myPlotShp_v2(shp_div_supply,yr2013,c(0,1),"E") + theme(legend.position = "none")
  p_supp[[4]] <- myPlotShp_2(shp_div_supply,ratio_2nd,c(-0.3,0.3)) + theme(legend.position = "none")
  
  p_div_supply_no_legend[[i_data]] <- p_supp[[2]] + p_supp[[3]] + p_supp[[4]]
  
  
  p_dep <- list()
  p_dep[[2]] <- myPlotShp_v2(shp_trade_dep,yr1989,c(0,1),"E") + theme(legend.position = "none")
  p_dep[[3]] <- myPlotShp_v2(shp_trade_dep,yr2013,c(0,1),"E") + theme(legend.position = "none")
  p_dep[[4]] <- myPlotShp_2(shp_trade_dep,ratio_2nd,c(-0.3,0.3)) + theme(legend.position = "none")
  
  p_div_dep_no_legend[[i_data]] <- p_dep[[2]] + p_dep[[3]] + p_dep[[4]]
  
  
  p_imp <- list()
  p_imp[[2]] <- myPlotShp_v2(shp_import_partners,yr1989,c(0,1),"E") + theme(legend.position = "none")
  p_imp[[3]] <- myPlotShp_v2(shp_import_partners,yr2013,c(0,1),"E") + theme(legend.position = "none")
  p_imp[[4]] <- myPlotShp_2(shp_import_partners,ratio_2nd,c(-0.3,0.3)) + theme(legend.position = "none")
  
  p_div_imp_no_legend[[i_data]] <- p_imp[[2]] + p_imp[[3]] + p_imp[[4]]
  
  
  p_prod <- list()
  p_prod[[2]] <- myPlotShp_v2(shp_div_prod,yr1989,c(0,1),"E")
  p_prod[[3]] <- myPlotShp_v2(shp_div_prod,yr2013,c(0,1),"E")
  p_prod[[4]] <- myPlotShp_2(shp_div_prod,ratio_2nd,c(-0.3,0.3))
  
  p_div_prod[[i_data]] <- p_prod[[2]] + p_prod[[3]] + p_prod[[4]]
  
  p_supp <- list()
  p_supp[[2]] <- myPlotShp_v2(shp_div_supply,yr1989,c(0,1),"E")
  p_supp[[3]] <- myPlotShp_v2(shp_div_supply,yr2013,c(0,1),"E")
  p_supp[[4]] <- myPlotShp_2(shp_div_supply,ratio_2nd,c(-0.3,0.3))
  
  p_div_supply[[i_data]] <- p_supp[[2]] + p_supp[[3]] + p_supp[[4]]
  
  p_dep <- list()
  p_dep[[2]] <- myPlotShp_v2(shp_trade_dep,yr1989,c(0,1),"E")
  p_dep[[3]] <- myPlotShp_v2(shp_trade_dep,yr2013,c(0,1),"E")
  p_dep[[4]] <- myPlotShp_2(shp_trade_dep,ratio_2nd,c(-0.3,0.3))
  
  p_div_dep[[i_data]] <- p_dep[[2]] + p_dep[[3]] + p_dep[[4]]
  
  
  p_imp <- list()
  p_imp[[2]] <- myPlotShp_v2(shp_import_partners,yr1989,c(0,1),"E")
  p_imp[[3]] <- myPlotShp_v2(shp_import_partners,yr2013,c(0,1),"E")
  p_imp[[4]] <- myPlotShp_2(shp_import_partners,ratio_2nd,c(-0.3,0.3))
  
  p_div_imp[[i_data]] <- p_imp[[2]] + p_imp[[3]] + p_imp[[4]]
  
  
  # end for loop ------------------------------------------------------------
  
  
}


# save data ----------------------------------------------------------------

write_excel_csv(t_combined_comb,"../output/comb_data_v10.csv")

write_excel_csv(d_pop_summary,"../output/pop_summary_v10.csv")




# plot maps ---------------------------------------------------------------

p_fig1 <- p_div_prod_no_legend[[2]] / p_div_prod_no_legend[[4]] / 
  p_div_supply_no_legend[[2]] / p_div_supply[[4]] 

p_fig2 <- p_div_dep_no_legend[[2]] /p_div_dep_no_legend[[4]] /
  p_div_imp_no_legend[[2]] /p_div_imp[[4]]

ggsave("../output_figures/fig1_prod_supply_v10.pdf",p_fig1,width = 21, height = 21,units = "cm")
ggsave("../output_figures/fig2_indep_import_v10.pdf",p_fig2,width = 21, height = 21,units = "cm")


p_comb_prod <- 
  p_div_prod_no_legend[[1]] / 
  p_div_prod_no_legend[[2]] / 
  p_div_prod_no_legend[[3]] /
  p_div_prod[[4]]
p_comb_supply <- p_div_supply_no_legend[[1]] / p_div_supply_no_legend[[2]] / p_div_supply_no_legend[[3]] / p_div_supply[[4]]
p_comb_dep <- p_div_dep_no_legend[[1]] / p_div_dep_no_legend[[2]] / p_div_dep_no_legend[[3]] / p_div_dep[[4]]
p_comb_imp <- p_div_imp_no_legend[[1]] / p_div_imp_no_legend[[2]] / p_div_imp_no_legend[[3]] / p_div_imp[[4]]


ggsave("../output_figures/figS1_div_res_ind_prod_v10.pdf",p_comb_prod,width = 21, height = 21,units = "cm")
ggsave("../output_figures/figS2_div_res_ind_supply_v10.pdf",p_comb_supply,width = 21, height = 21,units = "cm")
ggsave("../output_figures/figS3_div_res_ind_dep_v10.pdf",p_comb_dep,width = 21, height = 21,units = "cm")
ggsave("../output_figures/figS4_div_res_ind_imp_v10.pdf",p_comb_imp,width = 21, height = 21,units = "cm")

# save geopackages
# st_write(shp_res,"shp_res.gpkg",layer_options = 'OVERWRITE=YES', update = TRUE)
# st_write(shp_div_prod,"shp_div_prod.gpkg",layer_options = 'OVERWRITE=YES', update = TRUE)
# st_write(shp_div_supply,"shp_div_supply.gpkg",layer_options = 'OVERWRITE=YES', update = TRUE)
# st_write(shp_import_partners,"shp_import_partners.gpkg",layer_options = 'OVERWRITE=YES', update = TRUE)
# st_write(shp_trade_dep,"shp_trade_dep.gpkg",layer_options = 'OVERWRITE=YES', update = TRUE)
# 



# plot barplot ------------------------------------------------------------

d_pop_summary = st_read("../output/pop_summary_v10.csv") %>% 
  mutate(var = c('1-kcal','1-kcal','1-kcal','1-kcal','2-proteins','2-proteins','2-proteins','2-proteins',
                 '3-fats','3-fats','3-fats','3-fats','4-vitamins','4-vitamins','4-vitamins','4-vitamins'))

#t = gather(d_pop_summary[,c(1,3,12)], 'prod_pop',-trend_sign,-var)

# temp = d_pop_summary[,c(1,3,12)]
# 
# temp$var <- as.factor(as.character(temp$var))
# temp$prod_div.pop <- as.numeric(as.character(temp$prod_div.pop))
# temp$trend_sign <- as.factor(as.character(temp$trend_sign))
# 
# 
# ggplot(temp, aes(factor(trend_sign), prod_div.pop, fill = var)) + 
#   geom_bar(stat="identity", position = "dodge") + 
#   scale_fill_brewer(palette = "Set1")

titles = c('production','supply','independency','import partners')

p <- list()

for(i in 1:4){
  t_pop = as.numeric(as.character(d_pop_summary[,(i*2+1)]))/10^6;
  df <- data.frame(change = c("1-no_data","2-neg","3-no_change","4-pos","1-no_data","2-neg","3-no_change","4-pos",
                              "1-no_data","2-neg","3-no_change","4-pos","1-no_data","2-neg","3-no_change","4-pos"),
                   #pop_shr=t_data_shr[,i],
                   var = as.factor(as.character(d_pop_summary[,12])),
                   pop=round(t_pop,digits=1))
  
  p[[i]] <- 
    ggplot(df, aes(factor(change), pop, fill = forcats::fct_rev(var))) + 
    geom_bar(stat="identity", position = "dodge",width=.75) + 
    scale_fill_brewer(palette = "Set2") +
    ylim(0,6)+
    
    # geom_text(aes(label=pop), hjust=1, color="white", size=4.5)+
    #ylim(0, 0.8)+
    theme_minimal()+
    theme(axis.ticks.x=element_line())+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    theme(panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    ggtitle(titles[i])+
    coord_flip()
}

# add axis label to first plot
p[[1]] <- p[[1]] + theme(axis.text.y=element_text(face="bold"))
# remove legend from 1-3 plots
p[[1]] <- p[[1]] + theme(legend.position = "none")
p[[2]] <- p[[2]] + theme(legend.position = "none")
p[[3]] <- p[[3]] + theme(legend.position = "none")
p[[4]] <- p[[4]] + guides(fill = guide_legend(reverse = TRUE)) + theme(legend.title = element_blank())

p_bar_pop <- p[[1]] + p[[2]] + p[[3]] + p[[4]] + plot_layout(nrow=1)
ggsave("../output_figures/pop_summary_v10.pdf",p_bar_pop,width = 24, height = 7,units = "cm")




#barplot(t_data,legend.text = rownames(t_data),beside=T,xlab='Row', ylab='Value', horiz=T)
p <- list()

for(i in 1:5){
  
  df <- data.frame(change = c("1-no_data","2-neg","3-no_change","4-pos"),
                   pop_shr=t_data_shr[,i],
                   pop=round(t_data[,i]/10^6,digits=1))
  
  p[[i]] <- ggplot(data=df, aes(x=change, y=pop_shr)) +
    geom_bar(stat="identity", fill="steelblue")+
    geom_text(aes(label=pop), hjust=1, color="white", size=4.5)+
    ylim(0, 0.8)+
    theme_minimal()+
    theme(axis.ticks.x=element_line())+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    theme(panel.grid.minor = element_blank(),
          panel.background = element_blank())+
    ggtitle(colnames(t_data[i]))+
    coord_flip()
}

# add axis label to first plot
p[[1]] <- p[[1]] + theme(axis.text.y=element_text(face="bold"))


p_bar_pop <- p[[1]] + p[[2]] + p[[3]] + p[[4]] + p[[5]] + plot_layout(nrow=1)
ggsave("../output_figures/pop_summary_v5.pdf",p_bar_pop,width = 24, height = 7,units = "cm")






