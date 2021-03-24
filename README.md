# Codes for Kummu et al. 2020


This repository contains all the data, scripts and intermediary results used in Kummu et al. 2020, Interplay of trade and food system resilience: Gains on supply diversity over time at the cost of trade independency, https://doi.org/10.1016/j.gfs.2020.100360



## Brief descriptions for each file:
* shannon_index_functions.R : helper functions to fill data gaps, calculating macronutrient contents and shannon indices 
* shannon_index_production_supply.R : R-script that loads the FAO Food Balance data, calls functions from *shannon_index_functions.R* and outputs Shannon indices for production and supply in terms of food energy, protein, fat and  fruits and vegetables. Script stores the results into output the directory
* shannon_index_trade_partners.R : R-script that loads bilateral trade data from FAO, calls functions from *shannon_index_functions.R* and outputs Efficient Number of Species (ENS) for trade partners for food energy, protein, fat and fruits and vegetables. Script stores the results into the output directory.      
* food_trade_dependency_100321.R : R script that loads the FAO Food Balance data and outputs country-wise trade dependency in terms of kcal, g protein, g fat and kg of fruits and vegetables. Results are stored into the output directory.
* resilience_analysis.R: combining the output from scripts above, and calculating the resilience indicators
* resilience_cluster.R: clustering analysis based on the resilience indicators, calculated in resilience_analysis.R
* script_funct/*: functions used in trade_food_resilience.R script



## How to run the codes
 Scripts *shannon_index_production_supply.R*, *shannon_index_trade_partners.R*, and *food_trade_dependency_100321.R* produce intermediary results and should be run before *resilience_analysis.R* and *resilience_cluster.R*  scripts, which produce the final results. *resilience_cluster.R* script uses results from *resilience_analysis.R* script. 



### NOTE:
There might be slight difference in results produced here and the ones presented in the journal article as we modified the scripts to use the same version of FAO Food Balance Sheet. Difference between the two versions concerns the Food Balance Sheet values for the country of Kazakhstan during years 1993-2002. 
