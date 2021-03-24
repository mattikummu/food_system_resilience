## code for Kummu et al (2020) Interplay of trade and food system resilience: 
## Gains on supply diversity over time at the cost of trade independency
## https://doi.org/10.1016/j.gfs.2020.100360

## contact: pekka.kinnunen@aalto.fi


## Script for calculating Shannon and ENS-values for food trade 

library(tidyverse)
library(readxl)
library(here)

source("scripts/shannon_index_functions.R")

setwd(here::here())


# load trade data from FAO
# fill food data gaps and filter out Eritrea (no data in FAO)
country_names <- read_xlsx("input_data/countries_years.xlsx")
data_gaps <- read_xlsx("input_data/data_gaps.xlsx")

# Split the whole timeperiod into 3 year intervals
start_year <- 1987
end_year <- 2013
period_length <- 3
by_columns <-  c("Year", "Area", "Item")


# trade items and corresponding FAO Food Balance Sheet items
fao_primary_defs <- read_csv('input_data/FAO_FBS_COMMODITY_DESCRIPTIONS.csv', col_names =TRUE)  %>%
  select(`Item Code`, Item, Description) %>%
  filter(!is.na(Description)) %>%
  mutate(Description = gsub("Default composition:", "", Description))  %>%
  mutate(Description = strsplit(Description,"\\s+(?=[0-9])",perl=T)) %>%
  unnest(Description) %>%
  filter(Description != "") %>%
  separate(Description, into = c("Product Code", "Product"), sep  ="[^[:digit:]]", extra = "merge", fill = "right") %>%
  mutate(`Product Code` = as.numeric(`Product Code`)) %>%
  mutate(fao_primary_item = `Item Code`) %>%
  mutate(fao_primary_name = Item) %>%
  select(fao_primary_name, fao_primary_item, `Product Code`)

functional_groups <- read_csv("input_data/FAOSTAT_ITEM_GROUP.csv", col_names = TRUE)
drop_spices <- c("Spices, Other","Pepper","Pimento","Cloves")
drop_alcohol <- functional_groups %>% filter(`Item Group` == "Alcoholic Beverages") %>%  pull(Item)

drop_items <-c(drop_spices, drop_alcohol)

drop_trade_items <- fao_primary_defs %>%
  filter(fao_primary_name %in% drop_items) %>%
  pull(`Product Code`)

# load bilateral trade data 

load('input_data/fao_trade_matrix.RData', verbose = TRUE)

# remove double counting of watermelons and other melons
remove_double_counting <- c(567,568)
fao_primary_defs <- fao_primary_defs %>%
  filter(!(fao_primary_name == "Vegetables, Other" &  `Product Code` %in% remove_double_counting))

# countries to loop over
countries <- trade_data_raw$`Reporter Countries` %>%
  unique()

# Unique FBS items
FBS_items <- read_csv('input_data/FAOSTAT_ITEM_GROUP.csv', col_names =TRUE) %>%
  pull(Item) %>%
  unique()

#  remove food groups and food items with same name, e.g "Eggs" and "Milk - Excluding Butter"
groups_with_same_name <- c(2899,2848,2744)
items_with_same_name <-  c(2949, 2948,2928)

## FBS data to calculate energy content for each country, gather data from raw data
load("input_data/FBS.Rdata")
FBS_data <- foodBalanceSheets %>%
  as_tibble() %>% 
  mutate(`Item Code` = Item.Code) %>%
  filter(Item != "Population") %>% 
  select(-c(Area.Code, Element.Code, Unit, Item.Code)) %>% 
  pivot_wider(names_from =Element, values_from = Value) %>% 
  filter(Area %in% countries,
         Item %in% FBS_items,
         !(`Item Code` %in% items_with_same_name))
  

# Derive food macronutrient content from Food Balance Sheets
food_data_macronutrients <- fill_data_gaps(food_data = FBS_data,
                                   data_gaps = data_gaps,
                                  remove_proxies = FALSE  ) %>%
  replace(is.na(.),0) %>%
  filter(Area != "Eritrea") %>%
  macronutrient_content(.) %>%
  select(Area,  `Item Code`, Item,   Year, carbs_content, prot_content, fat_content, kcal_content)

# livestock yields
livestock_yields_raw <- read_csv(file='input_data/Production_LivestockPrimary_E_All_Data.csv',
                                 col_names = TRUE, guess_max = 20000) %>%
  filter(Element=="Yield/Carcass Weight")


# table for livestock parent products for live animals
live_animals_conversion <- read_csv('input_data/trade_live_animals_conversion.csv', col_names = TRUE)

# Get livestock yields for fbs animal items
# hg = 100 g, or 0.1 g/animal -> kg/animal
livestock_yields <- livestock_yields_raw %>%
  select( -c(ends_with("F"),"Area Code","Element Code"))  %>%
  gather( key = "Year", value = "value" ,Y1961:Y2017) %>%
  mutate(Year = Year %>% str_replace("Y","") %>% as.numeric) %>%
  mutate(unit_factor = ifelse(Unit== "hg/An", 0.1/1000,0.00001/1000 )) %>%
  mutate(carcass_yield = value*unit_factor) %>%
  left_join(live_animals_conversion, by=c("Item"="Livestock_yield_item")) %>%
  select(Area,FAO_trade_item,Item,"Item Code.x", "Item Code.y",FBS_item, Year, carcass_yield) %>%
  filter(!is.na(FAO_trade_item)) %>%
  mutate("Item Code meat" = `Item Code.x`, "Item Code" = `Item Code.y`) %>%
  select(-c(`Item Code.x`, `Item Code.y`, Item))

# empty result variable
trade_partners_diversity<- tibble()
trade_partners_fruits <- tibble()

# loop over every country
# assuming that FAO data with 0 means less than 0.5 tonne trade connection and NA means no trade
# between the countries

shannon_trade_elem <- c("prot_by_cntry", "fat_by_cntry", "carbs_by_cntry", "kcal_by_cntry")

functional_groups <- read_csv("input_data/FAOSTAT_ITEM_GROUP.csv", col_names = TRUE)
fruits_group <-functional_groups %>%
  filter(`Item Group` %in% c("Fruits - Excluding Wine", "Vegetables")) %>%
  pull(Item)

for (country in countries){

    # collect all trade items related to a given country
    temp_data <- trade_data_raw %>%
      filter(!(Item %in%drop_items))  %>%
      select(-c(`Reporter Country Code`,`Partner Country Code`)) %>%
      filter(`Reporter Countries`==country)

    years <- temp_data %>% pull(Year) %>% unique()

    # macronutrient data for the country
    temp_nutrient_data <- food_data_macronutrients %>%
      filter(Year %in% years) %>%
      filter(Area == country)

    if (nrow(temp_nutrient_data) >0) {

      # Shannon index for import and values.

      temp_data_imports <-temp_data %>% filter(Element=="Import Quantity")
      if (nrow(temp_data_imports)>0) {

        # First join conversion rates and livestock yields. Then calculate macronutrient imports for each
        # country and then calculate shannon indices
        temp_partners <- temp_data_imports %>%
          left_join(fao_primary_defs, by =c("Item Code" = "Product Code") ) %>%
          left_join(live_animals_conversion, by=c("Item Code")) %>%
          mutate(FBS_name = ifelse(is.na(FBS_item),fao_primary_name, FBS_item)) %>%
          left_join(livestock_yields, by = c("Reporter Countries"="Area", "Year",
                                             "Item Code" = "Item Code meat")) %>%
          select(`Reporter Countries`,`Partner Countries`,Year,Element, Unit,Item,
                 `Item Code`, FBS_name,value, carcass_yield ) %>%
          left_join(food_data_macronutrients,  by = c("Reporter Countries" = "Area",
                                                      "Year", "FBS_name"= "Item")) %>%
          filter(!is.na(FBS_name)) %>%
          mutate( yield = ifelse(is.na(carcass_yield), 1, carcass_yield)) %>%
          mutate(unit_factor = ifelse(Unit %in% c("tonnes", "1000 Head"), 1000,1)) %>%
          mutate(trade_prot = value * unit_factor* yield* prot_content,
                 trade_fat = value * unit_factor* yield* fat_content,
                 trade_carbs = value * unit_factor* yield* carbs_content,
                 trade_kcals = value * unit_factor * yield * kcal_content) %>%
          group_by(`Reporter Countries`,`Partner Countries`,Element,Year,FBS_name) %>%
          summarise(prot_by_cntry = sum(trade_prot, na.rm = TRUE),
                    fat_by_cntry = sum(trade_fat, na.rm = TRUE),
                    carbs_by_cntry = sum(trade_carbs, na.rm = TRUE),
                    kcal_by_cntry = sum(trade_kcals, na.rm =TRUE),
                    value = sum(value, na.rm= TRUE), .groups = "drop") %>%
          ungroup() %>%
          mutate(Area = `Reporter Countries`,
                 Item= FBS_name)

          # summarise import data per country and calculate the shannon index
          # from aggregate macronutrient imports for each country
          temp_shannon_trade <- temp_partners %>%
            ungroup() %>%
            group_by(Area,`Partner Countries`,Element,Year) %>%
            summarise(prot_by_cntry = sum(prot_by_cntry, na.rm = TRUE),
                      fat_by_cntry = sum(fat_by_cntry, na.rm = TRUE),
                      carbs_by_cntry = sum(carbs_by_cntry, na.rm = TRUE),
                      kcal_by_cntry = sum(kcal_by_cntry, na.rm =TRUE),
                      value = sum(value, na.rm= TRUE),
                      .groups = "drop") %>%
            assign_time_periods(period_length = period_length,
                                start_year = start_year,
                                end_year = end_year,
                                year_name = "Year") %>%
            shannon_index_by_element(data = .,
                                     elem_name = shannon_trade_elem,
                                     scale_index = FALSE,
                                     group_var  = "Partner Countries" )

        trade_partners_diversity <- bind_rows(trade_partners_diversity,
                                              temp_shannon_trade)

        temp_shannon_trade_fruits <- temp_partners %>%
          filter(Item %in% fruits_group) %>%
          ungroup() %>%
          group_by(Area,`Partner Countries`,Element,Year) %>%
          summarise(value = sum(value, na.rm= TRUE),.groups = "drop") %>%
          assign_time_periods(period_length = period_length,
                              start_year = start_year,
                              end_year = end_year,
                              year_name = "Year") %>%
            shannon_index_by_element(data = .,
                                     elem_name = "value",
                                     scale_index = FALSE,
                                     group_var = "Partner Countries" )

        trade_partners_fruits <- bind_rows(trade_partners_fruits,
                                           temp_shannon_trade_fruits)

      }
    }

    rm(list = ls(pattern = "temp_"))
}


trade_partners_fruits <- fill_trade_gaps(food_data = trade_partners_fruits,
                                         data_gaps = data_gaps)

trade_partners_diversity <- fill_trade_gaps(food_data = trade_partners_diversity,
                                            data_gaps = data_gaps)


trade_partners_diversity_normalized <- trade_partners_diversity %>%
  gather(key = "nutrient", value ="value_by_cntry", prot_by_cntry:kcal_by_cntry) %>%
  group_by(nutrient) %>%
  mutate(value_by_cntry= normalize_cols(value_by_cntry)) %>%
  spread(key = "timeperiod", value = value_by_cntry) %>%
  ungroup()

trade_partners_fruits_normalized <- trade_partners_fruits %>%
  ungroup() %>%
  mutate(value = normalize_cols(value)) %>%
  spread(key = "timeperiod", value = value)



trade_partners_ENS <- trade_partners_diversity %>%
  gather(key = "nutrient", value ="value_by_cntry", prot_by_cntry:kcal_by_cntry) %>%
  mutate(ENS = exp(value_by_cntry)) %>%
  select(-value_by_cntry) %>%
  group_by(nutrient) %>%
  spread(key = "timeperiod", value = ENS) %>%
  ungroup()


trade_partners_fruits_ENS <-trade_partners_fruits %>%
  ungroup() %>%
  mutate(ENS = exp(value)) %>%
  select(-value) %>%
  spread(key = "timeperiod", value = ENS)


# fao_ids
fao_ids <- read_csv('input_data/cntry_fao_id.csv')

result_with_ids <- left_join(trade_partners_diversity_normalized, fao_ids, by = c("Area" = "admin"))

trade_partners_ENS_ids <- left_join(trade_partners_ENS, fao_ids, by = c("Area" = "admin"))


for (var_name in shannon_trade_elem) {
  filepath <- paste("output/trade_ENS_",var_name,".csv", sep ="")

  trade_partners_ENS_ids %>%
    select(c("fao_id","Area","subregion",nutrient, `1989`:`2013`)) %>%
    filter(nutrient ==var_name) %>%
    select(-nutrient) %>%
    arrange(Area) %>% 
    write.csv (file = filepath)
}


for (var_name in shannon_trade_elem) {
  filepath <- paste("output/trade_shannon_index_",var_name,".csv", sep ="")

  result_with_ids %>%
    select(c("fao_id","Area","subregion",nutrient, `1989`:`2013`)) %>%
    filter(nutrient ==var_name) %>%
    select(-nutrient) %>%
    arrange(Area) %>% 
    write.csv (file = filepath)
}

trade_partners_fruits_ENS %>%
  left_join(fao_ids, by = c("Area" = "admin")) %>%
  select(c("fao_id","Area","subregion",`1989`:`2013`)) %>%
  arrange(Area) %>% 
  write.csv (file = "output/trade_ENS_fruits.csv")


trade_partners_fruits_normalized %>%
  left_join(fao_ids, by = c("Area" = "admin")) %>%
  select(c("fao_id","Area","subregion",`1989`:`2013`)) %>%
  arrange(Area) %>% 
  write.csv (file = "output/trade_shannon_index_fruits.csv")

#
#
# trade_partners_imports <- result_with_ids %>%
#   filter(Element == "Import Quantity")
#
#
# ggplot(data =trade_partners_imports) +
#   geom_point(aes(x = Year, y = shannon_vals, color = `Reporter Countries`), alpha =0.5) +
#   guides(color = FALSE)+
#   facet_wrap(~`subregion`)
#
# ggplot(data =trade_partners_imports) +
#   geom_point(aes(x = Year, y = ENS_vals, color = `Reporter Countries`)) +
#   guides(color = FALSE)+
#   facet_wrap(~`subregion`)
#
# ggplot(data =trade_partners_imports) +
#   geom_point(aes(x = Year, y = ENS_vals, color = subregion)) +
#   guides(color = FALSE)
#
# trade_partners_imports %>%
#   select(fao_id,`Reporter Countries`,Year,shannon_vals) %>%
#   spread(key = Year, value = shannon_vals, fill = 0) %>%
#   write.csv(file = "output/import_partners_shannon.csv")
#
# trade_partners_imports %>%
#   select(fao_id,`Reporter Countries`,Year,ENS_vals) %>%
#
#   spread(key = Year, value = ENS_vals, fill = 0) %>%
#   write.csv(file = "output/import_partners_ENS.csv")

