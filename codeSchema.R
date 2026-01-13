

#### Create clean shapefile RDS files in intermediate shapefile data folder ####
source(here("data/code/clean_shapefiles.R"))

#### Clean water balance data and save in intermediate data folder ####
source(here("data/code/clean_water_balance_data.R"))

#### Clean groundwater level data and save in intermediate data folder ####
source(here("data/code/load_groundwater_levels.R")) 

#### Clean ag crop water use data and save in intermediate data folder ####
source(here("data/code/load_crop_water_use.R"))

#### Clean electricity data and save in intermediate data folder ####
source(here("data/code/clean_electricity_data.R")) 

#### Create farmer data to do farmer valuations in matlab #### 
source(here("data/code/clean_electricity_data.R")) 

#### MOVE TO MATLAB for farmer value estimation #### 
# with the new frmwk, this could be done in R, but it's fine for now
# get_farmer_cleanest_approximation.m 

# AFTER THIS: move to urban water data cleaning 
############ - we will end up redoing urban water hopefully with FTR data (not yet)
## GOAL FOR TOMORROW 
# Replicate some version of the final results and re-familiarize with counterfactual pipeline 
# - save RF Estimation code and dauco_network code for later cuz it sucks ass -


