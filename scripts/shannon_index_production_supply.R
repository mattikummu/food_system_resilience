## code for Kummu et al (2020) Interplay of trade and food system resilience: 
## Gains on supply diversity over time at the cost of trade independency
## https://doi.org/10.1016/j.gfs.2020.100360

## contact: pekka.kinnunen@aalto.fi

## Script for calculating Shannon index for different macronutrients (kcal, fat, prot, carbohydrates)

rm(list = ls())

# load libraries
library(readxl)
library(here)
library(tidyverse)
source("scripts/shannon_index_functions.R")


# set working directory
setwd(here::here())

# load country names and changes in countries existence
country_names <- read_xlsx("input_data/countries_years.xlsx")
data_gaps <- read_xlsx("input_data/data_gaps.xlsx")


# Resolution for the images
fig_out_width <-10
fig_out_height <- 10
reso_out <- 150

# path to output files
output_files = "output/"

# load FBS data and functional groups
load("input_data/FBS.Rdata")

functional_groups <- read_csv("input_data/FAOSTAT_ITEM_GROUP.csv", col_names = TRUE)

# Load FAO ids for countries
fao_ids <- read_csv('input_data/cntry_fao_id.csv')

# Unique FBS items
FBS_items <- read_csv('input_data/FAOSTAT_ITEM_GROUP.csv', col_names =TRUE) %>%
  pull(Item) %>%
  unique()

# Unique country names
cntry_names <- read_csv("input_data/FAOSTAT_COUNTRY_GROUP.csv", col_names = TRUE) %>%
  pull(Country) %>%
  unique()

#  remove food groups and food items with same name, e.g "Eggs" and "Milk - Excluding Butter"
groups_with_same_name <- c(2899,2848,2744)
items_with_same_name <-  c(2949, 2948,2928)

# gather input_data from raw data

FBS_data <- foodBalanceSheets %>%
  as_tibble() %>% 
  mutate(`Item Code` = Item.Code) %>%
  filter(Item != "Population") %>% 
  select(-c(Area.Code, Element.Code, Unit, Item.Code)) %>% 
  pivot_wider(names_from =Element, values_from = Value) %>% 
  filter(Area %in% cntry_names,
         Item %in% FBS_items,
         !(`Item Code` %in% items_with_same_name)) 

# Filter out domestic supply quantities that are negative to be able to calculate shannon index
# drop spices and non foods

drop_items <- c("Alcohol, Non-Food", "Spices, Other","Pepper","Pimento","Cloves")
drop_alcohol <- functional_groups %>% filter(`Item Group` == "Alcoholic Beverages") %>%  pull(Item)
drop_items <- c(drop_items, drop_alcohol)

FBS_data <-FBS_data %>%
  filter(`Domestic supply quantity` >=0) %>%
  filter(!(Item %in% drop_items))

# fill food data gaps and filter out Eritrea (no data in FAO)
food_data_no_gaps <- fill_data_gaps(food_data = FBS_data,
                                   data_gaps = data_gaps,
                                      ) %>%
  replace(is.na(.),0) %>%
  filter(Area != "Eritrea")

# Split the whole timeperiod into 3 year intervals
start_year <- 1987
end_year <- 2013
period_length <- 3
by_columns <-  c("Year", "Area", "Item")

#####

# transform kg to energy content and assign time periods
quant_vars <- c("Production","Import Quantity","Export Quantity","Domestic supply quantity",
                "Stock Variation","Food", "Feed","Losses","Seed" ,"Other uses","Processing")

# macronutrients per capita
food_data_macronutrients <- macronutrient_content(data = food_data_no_gaps) %>%
  mutate(prot_production = Production * prot_content,
         fat_production =  Production * fat_content,
         prot_food_supply = Food * prot_content,
         fat_food_supply = Food * fat_content,
         carbs_production = Production * carbs_content,
         carbs_food_supply = Food * carbs_content,
         kcal_food_supply = Food*kcal_content,
         kcal_production = Production*kcal_content) %>%
  assign_time_periods(period_length = period_length,
                      start_year = start_year,
                      end_year = end_year,
                      year_name = "Year")


# Write out a csv for macronutrient contents with 3-year averages according to the 1987-2013
food_data_macronutrients %>%
  select(Area, Year,timeperiod, Item, fat_content, prot_content, carbs_content,kcal_content) %>%
  group_by(Area,Item, timeperiod) %>%
  summarise(fat_content = mean(fat_content, na.rm = TRUE),
            prot_content = mean(prot_content, na.rm = TRUE),
            carbs_content = mean(carbs_content, na.rm = TRUE),
            kcal_content = mean(kcal_content, na.rm =TRUE)) %>%
  mutate_at(vars(ends_with("content")), funs(round(.,digits=3))) %>%
  write_csv(file = paste0(output_files, "macronutrient_content.csv"))

# select columns needed
food_data_macronutrients <- food_data_macronutrients %>%
  select(Area,timeperiod, Item,
         ends_with("_production"),
         ends_with("_supply"))



###### SHANNON INDICES FOR DIVERSTY ##########

####  Fruits

fruits_group <-functional_groups %>%
  filter(`Item Group` %in% c("Fruits - Excluding Wine", "Vegetables")) %>%
  pull(Item)

fruit_elem <- c("Production","Food")

fruits <- food_data_no_gaps %>%
  filter(Item %in% fruits_group) %>%
  assign_time_periods(period_length = period_length,
                      start_year = start_year,
                      end_year = end_year,
                      year_name = "Year")

fruits_shannon_scaled <- shannon_index_by_element(data = fruits,
                                           elem_name = fruit_elem,
                                           scale_index = TRUE)   %>%
  left_join(fao_ids, by = c("Area" = "admin"))


# save results in separate csv-files
for (var_name in fruit_elem) {
  filepath <- paste0(output_files, "normalized_shannon_index_fruits_",var_name,".csv", sep ="")

  fruits_shannon_scaled %>%
    select(c("fao_id","Area", "timeperiod","subregion",all_of(var_name))) %>%
    pivot_wider(names_from = "timeperiod", values_from = all_of(var_name )) %>%
    arrange(fao_id) %>% 
    write.csv (file = filepath)


}

#### Macronutrients


shannon_elements <- c("prot_production","fat_production",
                      "prot_food_supply", "fat_food_supply",
                      "carbs_food_supply", "carbs_production",
                      "kcal_food_supply", "kcal_production")

# Calcultate shannon index (H) for given elements, assign country groups
shannon_indices_macronutrients_scaled<- shannon_index_by_element(data = food_data_macronutrients,
                                           elem_name = shannon_elements,
                                           scale_index = TRUE)   %>%
  left_join(fao_ids, by = c("Area" = "admin"))


# save results in separate csv-files
for (var_name in shannon_elements) {
  filepath <- paste0(output_files, "normalized_shannon_index_",var_name,".csv")

  shannon_indices_macronutrients_scaled %>%
    select(c("fao_id","Area", "timeperiod","subregion",all_of(var_name))) %>%
    pivot_wider(names_from = "timeperiod", values_from = all_of(var_name) ) %>%
    arrange(fao_id) %>% 
    write.csv(file = filepath)
}





