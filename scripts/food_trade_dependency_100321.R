#   R script for the article
#   Interplay of trade and food system resilience: Gains on supply diversity over time at the cost of trade independency
#   
#   M. Kummu, P. Kinnunen, E. Lehikoinen, M. Porkka, C. Queiroz, E. Röös, M. Troell and C. Weil
#   Global Food Security 24 (2020) 100360
#   https://doi.org/10.1016/j.gfs.2020.100360
#   
#   Script by Miina Porkka. This script was used to calculate the 'Independency from imports' indicator. 
#   For questions regarding this script please contact Miina directly (miina.porkka@aalto.fi). For questions 
#   regarding the other indicators please contact corresponding author Matti Kummu (matti.kummu@aalto.fi).

# Note that this code will actually give you dependency on trade (net imports/supply), which was the original 
# indicator that was later revised (but independency, i.e. domestic production/supply, can be calculated as 1- dependency).

rm(list = ls())

library(tidyverse)
library(here)
library(janitor)
library(zoo)

setwd(here::here())

# Load data ---------------------------------------------------------------

# UNCOMMENT AND RUN THESE LINES WHEN RUNNING THE SCRIPT WITH A DIFFERENT FAOSTAT RELEASE
# foodBalanceSheets <- read.table("input_data/FoodBalanceSheets_E_All_Data_(Normalized).csv", header=TRUE,
#                                                                  sep=",")
# foodBalanceSheets <- subset(foodBalanceSheets, select = -c(Year.Code, Flag))
# save(foodBalanceSheets, file="input_data/FBS.Rdata")

load("input_data/FBS.Rdata") #9/2/2018 release of FAOSTAT Food Balance Sheets; used in Kummu et al. 2020
load("input_data/itemListShort.Rdata")
load("input_data/countryList.Rdata")

elementList <- c(5142, 5611, 5511, 5911, 5072, 645, 664, 674, 684) #Necessary FBS elements

# Subset and reshape ------------------------------------------------------

FBS_data <- filter(foodBalanceSheets, Item.Code %in% itemListShort[,1] & Area.Code %in% countryList[,1] & Element.Code %in% elementList & Year>=1987 & Year<=2013) %>%
  select(-c("Element.Code", "Unit")) %>%
  pivot_wider(names_from = Element, values_from = Value) %>%
  clean_names()

FBS_data[is.na(FBS_data)] <- 0 # Assume Na=0

# Calculate dietary supply, prod and net imports in kcal, prot, fat -------

# Calculate new net imports and human food production (as in Porkka et al. 2013)
FBS_data$netImp <- FBS_data$import_quantity - FBS_data$export_quantity
FBS_data$foodProd <- FBS_data$food + FBS_data$export_quantity - FBS_data$import_quantity - FBS_data$stock_variation #food production calculated as in Porkka et al. 2013

# Calculate kg to kcal/prot/fat conversion factors
FBS_data$kcalPerKg <- (FBS_data$food_supply_kcal_capita_day * 365) / FBS_data$food_supply_quantity_kg_capita_yr
FBS_data$protgPerKg <- (FBS_data$protein_supply_quantity_g_capita_day * 365) / FBS_data$food_supply_quantity_kg_capita_yr
FBS_data$fatgPerKg <- (FBS_data$fat_supply_quantity_g_capita_day * 365) / FBS_data$food_supply_quantity_kg_capita_yr

# Inf (cases where supply = 0) to NA
FBS_data$kcalPerKg[is.infinite(FBS_data$kcalPerKg)] <- NA
FBS_data$protgPerKg[is.infinite(FBS_data$protgPerKg)] <- NA
FBS_data$fatgPerKg[is.infinite(FBS_data$fatgPerKg)] <- NA

# Calculate DS (dietary supply) in terms of kcal, prot & fat
FBS_data$DSkcal <- FBS_data$food*1000000*FBS_data$kcalPerKg
FBS_data$DSprot <- FBS_data$food*1000000*FBS_data$protgPerKg
FBS_data$DSfat <- FBS_data$food*1000000*FBS_data$fatgPerKg

# Calculate DP (dietary production) in terms of kcal, prot & fat (not needed in Kummu et al 2020)
FBS_data$DPkcal <- FBS_data$foodProd*1000000*FBS_data$kcalPerKg
FBS_data$DPprot <- FBS_data$foodProd*1000000*FBS_data$protgPerKg
FBS_data$DPfat <- FBS_data$foodProd*1000000*FBS_data$fatgPerKg

# Calculate NIMP (net imports) in terms of kcal, prot & fat
FBS_data$NIMPkcal <- FBS_data$netImp*1000000*FBS_data$kcalPerKg
FBS_data$NIMPprot <- FBS_data$netImp*1000000*FBS_data$protgPerKg
FBS_data$NIMPfat <- FBS_data$netImp*1000000*FBS_data$fatgPerKg

# Calculate trade dependency ----------------------------------------------

# Summarise necessary variables by country and year
FBS_country_sums <- FBS_data %>% 
  select(c("area_code", "area", "item_code", "year", ends_with("kcal"), ends_with("prot"), ends_with("fat"))) %>%
  pivot_longer(cols = DSkcal:NIMPfat, names_to = "variable", values_to = "value") %>%
  group_by(area_code, area, year, variable) %>%
  summarise(sum = sum(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = sum)

# Calculate TD (trade dependency) in terms of kcal, prot and fat (net imports divided by dietary supply)
FBS_country_sums$TDkcal <- FBS_country_sums$NIMPkcal/FBS_country_sums$DSkcal
FBS_country_sums$TDprot <- FBS_country_sums$NIMPprot/FBS_country_sums$DSprot
FBS_country_sums$TDfat <- FBS_country_sums$NIMPfat/FBS_country_sums$DSfat

# Remove unnecessary variables
trade_dependency <- FBS_country_sums %>% select(c(area_code, area, year, TDkcal, TDprot, TDfat))

# Fill gaps, USSR ---------------------------------------------------------

parentCountries = c(228,248,51,62,15,186,206)

n = 15*5

data_USSR = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkcal = numeric(n), TDprot = numeric(n), TDfat = numeric(n))

endRow = 0

for (j in 1987:1991) {
  tempYearData = trade_dependency[trade_dependency$area_code==parentCountries[1] & trade_dependency$year == j,]
  
  USSR.temp = data.frame("area_code" = c(1,52,57,63,73,108,113,119,126,146,185,208,213,230,235)) #all former USSR countries
  USSR.temp$area = "proxy data"
  USSR.temp$year = as.numeric(j)
  USSR.temp$TDkcal = tempYearData$TDkcal #same TD values for each former USSR country, assuming equal distribution of food supply and imports 
  USSR.temp$TDprot = tempYearData$TDprot
  USSR.temp$TDfat = tempYearData$TDfat
  
  startRow = endRow + 1
  endRow = startRow + nrow(USSR.temp) - 1
  
  data_USSR[startRow:endRow,] = USSR.temp
}

rm(USSR.temp)

# Fill gaps, SFR ----------------------------------------------------------

n = 6*5

data_SFR = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkcal = numeric(n), TDprot = numeric(n), TDfat = numeric(n))

endRow = 0

for (j in 1987:1991) {
  tempYearData = trade_dependency[trade_dependency$area_code==parentCountries[2] & trade_dependency$year == j,]
  
  SFR.temp = data.frame("area_code" = c(80,154,98,272,273,198)) #all former SFR countries
  SFR.temp$area = "proxy data"
  SFR.temp$year = as.numeric(j)
  SFR.temp$TDkcal = tempYearData$TDkcal
  SFR.temp$TDprot = tempYearData$TDprot
  SFR.temp$TDfat = tempYearData$TDfat
  
  startRow = endRow + 1
  endRow = startRow + nrow(SFR.temp) - 1
  
  data_SFR[startRow:endRow,] = SFR.temp
}

rm(SFR.temp)

# Fill gaps, CS -----------------------------------------------------------

n = 2*6

data_CS = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkcal = numeric(n), TDprot = numeric(n), TDfat = numeric(n))

endRow = 0

for (j in 1987:1992) {
  tempYearData = trade_dependency[trade_dependency$area_code==parentCountries[3] & trade_dependency$year == j,]
  
  CS.temp = data.frame("area_code" = c(167,199)) #Czech Republic and Slovakia
  CS.temp$area = "proxy data"
  CS.temp$year = as.numeric(j)
  CS.temp$TDkcal = tempYearData$TDkcal
  CS.temp$TDprot = tempYearData$TDprot
  CS.temp$TDfat = tempYearData$TDfat
  
  startRow = endRow + 1
  endRow = startRow + nrow(CS.temp) - 1
  
  data_CS[startRow:endRow,] = CS.temp
}

rm(CS.temp)

# Fill gaps, BELUX -----------------------------------------------------------

n = 2*13

data_BELUX = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkcal = numeric(n), TDprot = numeric(n), TDfat = numeric(n))

endRow = 0

for (j in 1987:1999) {
  tempYearData = trade_dependency[trade_dependency$area_code==parentCountries[5] & trade_dependency$year == j,]
  
  BELUX.temp = data.frame("area_code" = c(255,256)) #Belgium and Luxembourg
  BELUX.temp$area = "proxy data"
  BELUX.temp$year = as.numeric(j)
  BELUX.temp$TDkcal = tempYearData$TDkcal
  BELUX.temp$TDprot = tempYearData$TDprot
  BELUX.temp$TDfat = tempYearData$TDfat
  
  
  startRow = endRow + 1
  endRow = startRow + nrow(BELUX.temp) - 1
  
  data_BELUX[startRow:endRow,] = BELUX.temp
}

rm(BELUX.temp)

# Fill gaps, SerMont -----------------------------------------------------------

n = 2*14

data_SerMont = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkcal = numeric(n), TDprot = numeric(n), TDfat = numeric(n))

endRow = 0

for (j in 1992:2005) {
  tempYearData = trade_dependency[trade_dependency$area_code==parentCountries[6] & trade_dependency$year == j,]
  
  SerMont.temp = data.frame("area_code" = c(272,273)) # Serbia, Montenegro
  SerMont.temp$area = "proxy data"
  SerMont.temp$year = as.numeric(j)
  SerMont.temp$TDkcal = tempYearData$TDkcal
  SerMont.temp$TDprot = tempYearData$TDprot
  SerMont.temp$TDfat = tempYearData$TDfat
  
  startRow = endRow + 1
  endRow = startRow + nrow(SerMont.temp) - 1
  
  data_SerMont[startRow:endRow,] = SerMont.temp
}

rm(SerMont.temp)

# Fill gaps, Sudan former and Ethiopia -------------------------------------------------

trade_dependency[trade_dependency$area_code==276,1] <- 206 #Use data for Sudan (current) for Sudan (former) for years after dissolution (since time series of Sudan former covers most of the study period) 

trade_dependency[trade_dependency$area_code==62,1] <- 238 #Use data for Ethiopia PDR for current day Ethiopia 

# Combine and save final data ---------------------------------------------

trade_dependency_final <- rbind(drop_na(trade_dependency), data_USSR, data_SFR, data_CS, data_BELUX, data_SerMont) %>%
  arrange(area_code, year) %>%
  filter(!(area_code %in% c(228, 248, 51, 62, 15, 186, 178, 151, 221, 351))) #Drop the "parent countries"

rm(data_USSR, data_SFR, data_CS, data_BELUX, data_SerMont)

# Check that all countries have data for the whole period
range <- trade_dependency_final %>%
  group_by(area_code) %>%
  summarise(minYear = min(year), maxYear = max(year))

# Calculate 3 year averages
years <- c(1987:2013)

meanTD <- trade_dependency_final %>%
  group_by(area_code) %>%
  summarise(Year = years[seq(from=3, to=27, by=3)], meanTDkcal = zoo::rollmean(TDkcal,3)[seq(from=1, to=25, by=3)], 
            meanTDprot = zoo::rollmean(TDprot,3)[seq(from=1, to=25, by=3)], meanTDfat = zoo::rollmean(TDfat,3)[seq(from=1, to=25, by=3)]) %>%
  rename(Area.Code = area_code)

# Save output
# save(meanTD, file="output/mean_TD.Rdata")
write.csv(meanTD, file="output/mean_TD.csv")

# SAME CALCULATIONS FOR FRUITS AND VEGETABLES -----------------------------

# load("input_data/FBS.Rdata")

# Subset and reshape
elementListFnV <- c(5142, 5611, 5511, 5911, 5072, 645)

FBS_data_FnV <- filter(foodBalanceSheets, Item.Code %in% c(2918, 2919) & Area.Code %in% countryList[,1] & Element.Code %in% elementListFnV & Year>=1987 & Year<=2013) %>%
  select(-c("Element.Code", "Unit")) %>%
  pivot_wider(names_from = Element, values_from = Value) %>%
  clean_names()

FBS_data_FnV[is.na(FBS_data_FnV)] <- 0 # Assume Na=0

# Calculate net imports of fruits and vegetables ------------------------
FBS_data_FnV$netImp <- FBS_data_FnV$import_quantity - FBS_data_FnV$export_quantity

# Calculate trade dependency ----------------------------------------------

# Summarise necessary variables by country and year
FBS_country_sums_FnV <- FBS_data_FnV %>% 
  select(c("area_code", "area", "item_code", "year", "food", "netImp")) %>%
  pivot_longer(cols = food:netImp, names_to = "variable", values_to = "value") %>%
  group_by(area_code, area, year, variable) %>%
  summarise(sum = sum(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = variable, values_from = sum)

# Calculate trade dependency
FBS_country_sums_FnV$TDkg <- FBS_country_sums_FnV$netImp/FBS_country_sums_FnV$food

# Remove unnecessary variables
trade_dependency_FnV <- FBS_country_sums_FnV %>% select(c(area_code, area, year, TDkg))

# Fruits and Veggies, fill in data gaps -----------------------------------

parentCountries = c(228,248,51,62,15,186,206)

n = 15*5

data_USSR = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkg = numeric(n))

endRow = 0

for (j in 1987:1991) {
  tempYearData = trade_dependency_FnV[trade_dependency_FnV$area_code==parentCountries[1] & trade_dependency_FnV$year == j,]
  
  USSR.temp = data.frame("area_code" = c(1,52,57,63,73,108,113,119,126,146,185,208,213,230,235)) #all former USSR countries
  USSR.temp$area = "proxy data"
  USSR.temp$year = as.numeric(j)
  USSR.temp$TDkg = tempYearData$TDkg
  
  startRow = endRow + 1
  endRow = startRow + nrow(USSR.temp) - 1
  
  data_USSR[startRow:endRow,] = USSR.temp
}

rm(USSR.temp)

# Fill gaps, SFR ----------------------------------------------------------

n = 6*5

data_SFR = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkg = numeric(n))

endRow = 0

for (j in 1987:1991) {
  tempYearData = trade_dependency_FnV[trade_dependency_FnV$area_code==parentCountries[2] & trade_dependency_FnV$year == j,]
  
  SFR.temp = data.frame("area_code" = c(80,154,98,272,273,198))  #all former SFR countries
  SFR.temp$area = "proxy data"
  SFR.temp$year = as.numeric(j)
  SFR.temp$TDkg = tempYearData$TDkg
  
  startRow = endRow + 1
  endRow = startRow + nrow(SFR.temp) - 1
  
  data_SFR[startRow:endRow,] = SFR.temp
}

rm(SFR.temp)

# Fill gaps, CS -----------------------------------------------------------

n = 2*6

data_CS = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkg = numeric(n))

endRow = 0

for (j in 1987:1992) {
  tempYearData = trade_dependency_FnV[trade_dependency_FnV$area_code==parentCountries[3] & trade_dependency_FnV$year == j,]
  
  CS.temp = data.frame("area_code" = c(167,199))
  CS.temp$Area = "proxy data"
  CS.temp$Year = as.numeric(j)
  CS.temp$TDkg = tempYearData$TDkg
  
  startRow = endRow + 1
  endRow = startRow + nrow(CS.temp) - 1
  
  data_CS[startRow:endRow,] = CS.temp
}

rm(CS.temp)

# Fill gaps, BELUX -----------------------------------------------------------

n = 2*13

data_BELUX = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkg = numeric(n))

endRow = 0

for (j in 1987:1999) {
  tempYearData = trade_dependency_FnV[trade_dependency_FnV$area_code==parentCountries[5] & trade_dependency_FnV$year == j,]
  
  BELUX.temp = data.frame("area_code" = c(255,256))
  BELUX.temp$Area = "proxy data"
  BELUX.temp$Year = as.numeric(j)
  BELUX.temp$TDkg = tempYearData$TDkg
  
  
  startRow = endRow + 1
  endRow = startRow + nrow(BELUX.temp) - 1
  
  data_BELUX[startRow:endRow,] = BELUX.temp
}

rm(BELUX.temp)

# Fill gaps, SerMont -----------------------------------------------------------

n = 2*14

data_SerMont = data.frame(area_code = numeric(n), area = numeric(n), year = numeric(n), TDkg = numeric(n))

endRow = 0

for (j in 1992:2005) {
  tempYearData = trade_dependency_FnV[trade_dependency_FnV$area_code==parentCountries[6] & trade_dependency_FnV$year == j,]
  
  SerMont.temp = data.frame("area_code" = c(272,273))
  SerMont.temp$Area = "proxy data"
  SerMont.temp$Year = as.numeric(j)
  SerMont.temp$TDkg = tempYearData$TDkg
  
  startRow = endRow + 1
  endRow = startRow + nrow(SerMont.temp) - 1
  
  data_SerMont[startRow:endRow,] = SerMont.temp
}

rm(SerMont.temp)

# Fill gaps, Sudan former and Ethiopia -------------------------------------------------

trade_dependency_FnV[trade_dependency_FnV$area_code==276,1] <- 206 #Use data for Sudan (current) for Sudan (former) for years after dissolution (since time series of Sudan former covers most of the study period) 
trade_dependency_FnV[trade_dependency_FnV$area_code==62,1] <- 238 #Use data for Ethiopia PDR for current day Ethiopia 

# Fruits and Veggies, combine and save data ----------------------------------------

trade_dependency_final_FnV <- rbind(drop_na(trade_dependency_FnV), data_USSR, data_SFR, data_CS, data_BELUX, data_SerMont) %>%
  arrange(area_code, year) %>%
  filter(!(area_code %in% c(228, 248, 51, 62, 15, 186, 178, 151, 221, 351))) #Drop the "parent countries"

rm(data_USSR, data_SFR, data_CS, data_BELUX, data_SerMont)

# Check that all countries have data for the whole period
range_FnV <- trade_dependency_final_FnV %>%
  group_by(area_code) %>%
  summarise(minYear = min(year), maxYear = max(year))

# calculate 3 year averages

years <- c(1987:2013)
meanTD.FnV <- trade_dependency_final_FnV %>%
  group_by(area_code) %>%
  summarise(Year = years[seq(from=3, to=27, by=3)], meanTDkg = zoo::rollmean(TDkg,3)[seq(from=1, to=25, by=3)]) %>%
  rename(Area.Code = area_code)

# Save output
# save(meanTD.FnV, file="output/mean_TD_fruits_veg.Rdata")
write.csv(meanTD.FnV, file="output/mean_TD_fruits_veg.csv")