## code for Kummu et al (2020) Interplay of trade and food system resilience: 
## Gains on supply diversity over time at the cost of trade independency
## https://doi.org/10.1016/j.gfs.2020.100360

## contact: pekka.kinnunen@aalto.fi

### Helper functions for shannon_index_production_supply.R and 
### shannon_index_trade_partners.R scripts


trade_data_to_rdata <- function(path_to_trade_data) {
  # function transforms raw bilateral trade data from FAO to .RData file, 
  # in long format
  # Function is not actively called from any of the scripts, here just as an 
  # indication how to data was transformed
  
  trade_data_raw <- read_csv(path_to_trade_data,
                             col_names = TRUE, 
                             guess_max = 140000) %>%
    select( -c(ends_with("F")), -`Element Code` )  %>%
    filter(Unit %in% c("1000 Head", "Head" ,"tonnes"))  %>%
    pivot_longer( names_to = "Year", values_to = "value", cols = Y1986:Y2016) %>%
    mutate(Year = Year %>% str_replace("Y","") %>% as.numeric) %>%
    filter(!is.na(value)) 
  
  save(trade_data_raw, file= "input_data/fao_trade_matrix.RData")
  
}


macronutrient_content <- function(data) {
  ## calculate the macronutrient content for each food item
  ## input: dietary variables
  
  # energy content in kcals per gram of macronutrient
  energy_in_protein <- 4.0
  energy_in_fat <- 9.0
  energy_in_carbs <- 4.0

  diet_data <- data %>%
    mutate(fat_energy = energy_in_fat * `Fat supply quantity (g/capita/day)`) %>%
    mutate(protein_energy = energy_in_protein * `Protein supply quantity (g/capita/day)`) %>%
    mutate(carbs_energy = `Food supply (kcal/capita/day)` - fat_energy - protein_energy) %>%
    mutate(carbs_energy = if_else(carbs_energy<1,0, carbs_energy)) %>%
    mutate(carbs_supply_quantity = carbs_energy / energy_in_carbs) %>%
    mutate(kcal_content = `Food supply (kcal/capita/day)`*365 /  `Food supply quantity (kg/capita/yr)`, 
          carbs_content = carbs_supply_quantity  /1000*365 / `Food supply quantity (kg/capita/yr)`, 
           prot_content =`Protein supply quantity (g/capita/day)`/1000*365 / `Food supply quantity (kg/capita/yr)`,
           fat_content = `Fat supply quantity (g/capita/day)` /1000 * 365 / `Food supply quantity (kg/capita/yr)`) %>%
    mutate_at(vars(ends_with("content")), funs(if_else(!is.finite(.), 0,.))) 
    
  data_out <- diet_data #%>%
    #select(c(Area, Year,Item, "carbs_content", "fat_content", "prot_content"))
  
  # return macronutrient content in kg nutrient / kg food item
  return (data_out) 
}


assign_time_periods<-function(data,period_length,start_year, end_year,year_name){
  # time periods for the aggregation
  all_years  <- start_year:end_year
  nPeriods <- ceiling( (end_year-start_year) / period_length)
  
  # end year of the timeperiod
  time_periods <- seq(start_year+period_length-1, by = period_length, length.out = nPeriods ) %>%
    rep(each = period_length) %>%
    as_tibble() %>%
    mutate(value = ifelse(value>end_year,end_year,value))%>% 
    mutate(timeperiod = value) %>%
    select(timeperiod)
  
  time_periods <- time_periods[1:length(all_years),] %>%
    mutate(all_years = all_years)
  
  data <- data %>%
    mutate_(Year = (year_name))
  
  data_mean_time_periods <- data %>% 
    filter(Year >= start_year, Year <= end_year) %>% 
    left_join(y = time_periods, by = c(Year= "all_years"))
  
  return (data_mean_time_periods)
}


shannon_index_by_element <- function(data,elem_name, scale_index = FALSE, group_var = "Item") {
  ## create Shannon index for a given FAO FBS element
  data <- data %>% ungroup()

  # keep identification and element columns
  keep_cols <- c("Area","Item","timeperiod")
  output_cols <- c(keep_cols, elem_name)
  
  group_var <-sym(group_var)

  # calculate the annual mean value for each time period
  shannon_index_by_time_periods <- data %>% 
    group_by(timeperiod,Area,!! group_var) %>%
    summarise_at(vars(elem_name),funs(mean(.,na.rm =TRUE))) %>%
    mutate_at(vars(elem_name),funs(shr_of_group_total )) %>%
    mutate_at(vars(elem_name), ~replace(., is.nan(.), 0)) %>% 
    group_by(timeperiod, Area) %>%
    summarise_at(vars(elem_name), funs(shannon_value))

              
  # scale values to 1
  if (scale_index) {
   shannon_index_by_time_periods <- shannon_index_by_time_periods %>%
     ungroup() %>% 
     mutate_at(vars(elem_name), funs(normalize_cols) )
  }
  
  return (shannon_index_by_time_periods)
}

shr_of_group_total <- function(x){
  out <- x / sum(x, na.rm = TRUE)
}

shannon_value <- function(x) {
  out <- log(1/prod(x^x))
}

normalize_cols <- function(x) {
  max_value <- max(x, na.rm = TRUE)
  min_value <- min(x, na.rm = TRUE)
  output<- (x-min_value)/(max_value-min_value)
  return (output)
}

fill_data_gaps <- function(food_data, data_gaps, remove_proxies =TRUE) {
  ## Fill data based on first year of available data, the share of value to all proxy-country values
  
  #read short names of data to fill
  short_names <- data_gaps$`Short name`
  all_proxies <- data_gaps$`Proxy data source (kcal/cap)`
  all_names <- c(short_names, unique(all_proxies))
  cntry_fao_ids <- data_gaps$FAOSTAT
  
  element_names <- read_csv("input_data/FAOSTAT_ELEMENT_names.csv")
  
  col_names <- element_names %>%
    filter(Element != "Total Population - Both sexes") %>%
    pull(Element)
  
  food_supply <- "Food supply quantity (kg/capita/yr)"
  food_energy <- "Food supply (kcal/capita/day)"
  protein_supply <- "Protein supply quantity (g/capita/day)"
  fat_supply <- "Fat supply quantity (g/capita/day)"
  dietary_elems <- c(food_energy,food_supply,protein_supply,fat_supply)
  
  food_elems <- c("Production", "Import Quantity", "Export Quantity","Stock Variation","Other uses",
                  "Domestic supply quantity", "Food","Feed","Seed","Processing","Losses") 
  
  food_elems_proxy <- paste(food_elems, "shr", sep ="_")
  dietary_elems_shr <- paste(dietary_elems, "shr", sep = "_")
  
   # loop over each cntry needing a fill
  for (ii in 1:length(short_names)){
    
    # get time period for data gap
    gap_start <- data_gaps$`Gap start`[ii]
    gap_end <- data_gaps$`Gap end`[ii]
    proxy_timeperiod <- gap_start:gap_end
    

    # get names for countries and proxies
    cntry_name <- short_names[ii]
    cntry_id <- cntry_fao_ids[ii]
    
    proxy_name <- data_gaps$`Proxy data source (kcal/cap)`[ii]
    proxy_id <- data_gaps$`Proxy country code`[ii]
    
    cntries_with_same_proxy <- data_gaps %>% 
      filter(`Proxy data source (kcal/cap)`== proxy_name)
    
    cntries_with_same_proxy <- data_gaps %>%
      filter(`Proxy country code` == proxy_id)
    
    ### QUANTITIES 
    # Calculate the share of total proxy value for each variable
    
    proxy_data_gap_end <- food_data %>% 
      filter(Year == gap_end+1) %>% # get first year after the end of the gap
      filter(Area %in% cntries_with_same_proxy$`Short name`) %>%  # get all the countries related to the proxy 
      group_by(Item) %>%
      mutate_at(vars(food_elems),.funs = funs(shr_of_group_total )) %>% # calculate the shr of the group total
      mutate_at(vars(food_elems),.funs = funs(replace_na(.,0))) %>% 
      select(-Year) %>%
      select(-c(dietary_elems)) %>%
      filter(Area == cntry_name) %>%
      rename_at(vars(food_elems), .funs = funs(paste(., "shr", sep = "_")))
    
    
    # join data with proxy cntry and calculate the share in the country within the proxy
    cntry_data_all <- food_data%>%
      filter(Area == proxy_name) %>%
      filter(Year %in% proxy_timeperiod) %>%
      mutate(Area = cntry_name) %>%
      left_join(proxy_data_gap_end, by = c("Area","Item","Item Code")) 
    
    # Multiple the values of the proxy country with the share of values at the end of the gap
    cntry_data_all[,food_elems] <- cntry_data_all[,food_elems] * cntry_data_all[,food_elems_proxy]
   
     cntry_data_all <- cntry_data_all %>% 
       select(-food_elems_proxy)
      
     # combine results 
     food_data <- food_data %>% 
        filter(!(Area == cntry_name & Year %in% proxy_timeperiod)) %>%
        rbind(cntry_data_all)
     
  }
  
  # Filter out all proxy countries
  if (remove_proxies){
    food_data <- food_data %>%
      filter(!(Area %in% all_proxies)) 
  }
  
  output <- food_data
  return (output)
}

fill_trade_gaps <- function(food_data, data_gaps, remove_proxies =TRUE) {
  ## Fill shannon values as same for all the proxy countries. 
  
  #read short names of data to fill
  short_names <- data_gaps$`Short name`
  all_proxies <- data_gaps$`Proxy data source (kcal/cap)`
  all_names <- c(short_names, unique(all_proxies))

  years  <- tibble(years = 1987:2013, 
                   timeperiod = seq(1987+3-1, by = 3, length.out = 9 ) %>%
                     rep(each = period_length))
  
  # loop over each cntry needing a fill
  for (ii in 1:length(short_names)){
    
    cntry_name <- short_names[ii]
    proxy_name <- data_gaps$`Proxy data source (kcal/cap)`[ii]
    proxy_data <- food_data %>%
      filter(Area == proxy_name) 

    if (dim(proxy_data)[1] >0){
    
      # get time period for data gap
      gap_start <- data_gaps$`Gap start`[ii]
      
      gap_end <- data_gaps$`Gap end`[ii]
      proxy_timeperiod <- unique(years$timeperiod[years$years>=gap_start & years$years<=gap_end])
        
      # join data with proxy cntry and calculate the share in the country within the proxy
      cntry_data <- proxy_data %>% 
        mutate(Area = cntry_name)

     
      food_data <- food_data %>% 
        filter(!(Area == cntry_name & timeperiod %in% proxy_timeperiod)) %>%
        rbind(cntry_data)
    }
  }
  
  # Filter out all proxy countries
  if (remove_proxies){
    food_data <- food_data %>%
      filter(!(Area %in% all_proxies)) 
  }
  
  output <- food_data 
  return (output)
}









