dauco_dau_xw <- read_csv(here("data/intermediate/xws/dauco_dau_xw.csv"))

# Load crop/aw/etaw data 
years <- seq(2002, 2010, by = 1)
metrics <- c("AW", "ETAW", "ICA", "EP")
water_use_02_10 <- data.frame("year" = numeric(),               
                        "dau_id" = character(),
                        "dau_name" = character(),
                        "grain" = numeric(),           
                        "rice" = numeric(),                
                        "cotton" = numeric(),              
                        "sugar_beet" = numeric(),           
                        "corn" = numeric(),                
                        "dry_beans" = numeric(),            
                        "safflower" = numeric(),            
                        "other_field" = numeric(),          
                        "alfalfa" = numeric(),              
                        "pasture" = numeric(),             
                        "tomato_processing" = numeric(),    
                        "tomato_fresh" = numeric(),         
                        "cucurbits" = numeric(),            
                        "onion_garlic" = numeric(),       
                        "potatoes" = numeric(),             
                        "truck_crops" = numeric(),          
                        "almonds_pistachios" = numeric(), 
                        "other_decidious" = numeric(),      
                        "citrus_subtropical" = numeric(), 
                        "vineyard" = numeric(),  
                        "metric_name" = character())

for(year in years) {
  for(metric in metrics) {
    filename <- paste0(year, "_By_DAU.xls")
    sheetname <- paste(metric, "DAU", year, sep = '_')
    print(filename)
    print(sheetname)
    new_data <- readxl::read_xls(here(paste0("raw/water_balance/water_plan/", 
                                              filename)), 
                                  sheet = sheetname) 
    new_data %>% 
      filter(!is.na(Year)) %>% 
      mutate(metric_name = str_to_lower(metric)) -> new_data
    
    if(metric == "ICA") {
      new_data %>% 
        select(-`Irrigated Crop Area`) %>% 
        select(-`Irrigated Land Area`) %>% 
        select(-`MultCrop MA`) -> new_data
    }
    
    colnames(new_data) <- colnames(water_use_02_10)
    water_use_02_10 <- bind_rows(water_use_02_10, new_data)
  }
}

water_use_02_10 %>% 
  pivot_longer(cols = c("grain", "rice", "cotton", "sugar_beet", "corn", "dry_beans",          
                        "safflower", "other_field", "alfalfa", "pasture", "tomato_processing",  
                        "tomato_fresh", "cucurbits", "onion_garlic", "potatoes", "truck_crops",        
                        "almonds_pistachios", "other_decidious", "citrus_subtropical", 
                        "vineyard"),
               names_to = 'crop', 
               values_to = 'value') %>% 
  pivot_wider(id_cols = c("year", "dau_id", "dau_name", "crop"), 
              names_from = metric_name, 
              values_from = value) %>% 
  mutate(etaw = ifelse(ica == 0, 0, etaw)) %>% 
  mutate(etaw = ifelse(aw == 0, 0, etaw)) %>% 
  mutate(ep = ifelse(ica == 0, 0, ep)) %>% 
  left_join(dauco_dau_xw, by = c("dau_id", "year")) %>% 
  # this needs to be done better 
  # right now I'm keeping crop shares fixed and apportioning water
  # can use post 2010 data to guess how crop shares are shaped in earlier period 
  mutate(ica = 1000*ica*dau_share) %>% 
  mutate(aw = aw*ica) %>% 
  mutate(etaw = etaw*ica) %>% 
  mutate(ep = ep*ica) %>% 
  select(year, dauco_id, crop, ica, aw, etaw, ep) %>% 
  arrange(year, dauco_id) -> water_use_02_10_by_crop


regions <- c('nro', 'ncro', 'scro', 'sro')
years <- c(2011, 2012, 2013, 2014, 2015)
metrics <- c('aw', 'etaw', 'ica', 'ep')
water_use_11_15 <- data.frame("year" = numeric(),               
                        "ro" = character(),              
                        "hr" =  character(),              
                        "pa" =   character(),                
                        "dauco_id" = numeric(),        
                        "grain" = numeric(),           
                        "rice" = numeric(),                
                        "cotton" = numeric(),              
                        "sugar_beet" = numeric(),           
                        "corn" = numeric(),                
                        "dry_beans" = numeric(),            
                        "safflower" = numeric(),            
                        "other_field" = numeric(),          
                        "alfalfa" = numeric(),              
                        "pasture" = numeric(),             
                        "tomato_processing" = numeric(),    
                        "tomato_fresh" = numeric(),         
                        "cucurbits" = numeric(),            
                        "onion_garlic" = numeric(),       
                        "potatoes" = numeric(),             
                        "truck_crops" = numeric(),          
                        "almonds_pistachios" = numeric(), 
                        "other_decidious" = numeric(),      
                        "citrus_subtropical" = numeric(), 
                        "vineyard" = numeric(),            
                        "average_metric" = numeric(), 
                        "metric_name" = character())

for(region in regions) {
  for(year in years) {
    for(metric in metrics) {
      filename <- paste(year, region, metric, sep = '_')
      print(filename)
      new_data <- readxl::read_xlsx(here(paste0("raw/water_balance/water_plan/", filename, ".xlsx")), skip = 1) 
      new_data %>% 
        filter(!is.na(Year)) %>% 
        mutate(metric_name = metric) -> new_data
      colnames(new_data) <- colnames(water_use_11_15)
      water_use_11_15 <- bind_rows(water_use_11_15, new_data)
    }
  }
}

water_use_11_15 %>% 
  mutate(dauco_id = str_pad(dauco_id, width = 5, side = 'left', pad = "0")) %>% 
  select(-c("ro", "hr", "pa", "average_metric")) %>% 
  unique %>% 
  pivot_longer(cols  = c("grain", "rice", "cotton", "sugar_beet", "corn", "dry_beans",          
                         "safflower", "other_field", "alfalfa", "pasture", "tomato_processing",  
                         "tomato_fresh", "cucurbits", "onion_garlic", "potatoes", "truck_crops",        
                         "almonds_pistachios", "other_decidious", "citrus_subtropical", 
                         "vineyard"),
               names_to = 'crop', 
               values_to = 'value') %>% 
  pivot_wider(id_cols = c('year', 'dauco_id', 'crop'), 
              names_from = c('metric_name'),
              values_from = c('value'), 
              values_fn = mean) -> water_use_11_15_by_crop

read_xlsx(here("raw/water_balance/water_plan/Statewide2016AgDataByDAUCO_Volume_RevisedNov27_23.xlsx"), 
          sheet = "_Statewide_ICA_DAUCO_2016", skip = 1) -> ica_2016
read_xlsx(here("raw/water_balance/water_plan/Statewide2016AgDataByDAUCO_Volume_RevisedNov27_23.xlsx"), 
          sheet = "2016Statewide_AW_VolumeDauCo", skip = 1) -> aw_2016
read_xlsx(here("raw/water_balance/water_plan/Statewide2016AgDataByDAUCO_Volume_RevisedNov27_23.xlsx"), 
          sheet = "2016StatewideETAWVolumeDauCo", skip = 1) -> etaw_2016
read_xlsx(here("raw/water_balance/water_plan/Statewide2016AgDataByDAUCO_Volume_RevisedNov27_23.xlsx"), 
          sheet = "2016StatewideEP_VolumeDauco", skip = 1) -> ep_2016

ica_2016 <- ica_2016 %>% select(-`Multi-Crops`) %>% mutate(metric_name = "ica")
colnames(ica_2016) <- colnames(water_use_11_15)
aw_2016$metric_name = "aw"
colnames(aw_2016) <- colnames(water_use_11_15)
etaw_2016$metric_name = "etaw"
colnames(etaw_2016) <- colnames(water_use_11_15)
ep_2016$metric_name = "ep"
colnames(ep_2016) <- colnames(water_use_11_15)

read_xlsx(here("raw/water_balance/water_plan/Statewide2018AgData_ByDauCo_Volume_Nov14_23.xlsx"), 
          sheet = "2018_Regional_ICADauCoStatewide", skip = 1) -> ica_2018
read_xlsx(here("raw/water_balance/water_plan/Statewide2018AgData_ByDauCo_Volume_Nov14_23.xlsx"), 
          sheet = "Statewide_VolumeAW_DauCo_2018", skip = 1) -> aw_2018
read_xlsx(here("raw/water_balance/water_plan/Statewide2018AgData_ByDauCo_Volume_Nov14_23.xlsx"), 
          sheet = "Statewide_VolumeETAW_DauCo_2018", skip = 1) -> etaw_2018
read_xlsx(here("raw/water_balance/water_plan/Statewide2018AgData_ByDauCo_Volume_Nov14_23.xlsx"), 
          sheet = "Statewide_VolumeEP_DauCo_2018", skip = 1) -> ep_2018

ica_2018 <- ica_2018  %>% select(-`Multi-Crops`) %>% mutate(metric_name = "ica")
colnames(ica_2018) <- colnames(water_use_11_15 %>% select(-average_metric))
aw_2018$metric_name = "aw"
colnames(aw_2018) <- colnames(water_use_11_15 %>% select(-average_metric))
etaw_2018$metric_name = "etaw"
colnames(etaw_2018) <- colnames(water_use_11_15 %>% select(-average_metric))
ep_2018$metric_name = "ep"
colnames(ep_2018) <- colnames(water_use_11_15 %>% select(-average_metric))

read_xlsx(here("raw/water_balance/water_plan/Statewide_2019AgWaterDataByDauCO_VolumeNov8_23.xlsx"), 
          sheet = "2019ICA_Statewide", skip = 1) -> ica_2019
read_xlsx(here("raw/water_balance/water_plan/Statewide_2019AgWaterDataByDauCO_VolumeNov8_23.xlsx"), 
          sheet = "2019AW_VolumeStatewide", skip = 1) -> aw_2019
read_xlsx(here("raw/water_balance/water_plan/Statewide_2019AgWaterDataByDauCO_VolumeNov8_23.xlsx"), 
          sheet = "2019ETAW_VolumeStatewide", skip = 1) -> etaw_2019
read_xlsx(here("raw/water_balance/water_plan/Statewide_2019AgWaterDataByDauCO_VolumeNov8_23.xlsx"), 
          sheet = "2019EP_VolumeStatewide", skip = 1) -> ep_2019

ica_2019 <- ica_2019 %>% mutate(metric_name = "ica")
colnames(ica_2019) <- colnames(water_use_11_15)
ica_2019$average_metric = as.numeric(ica_2019$average_metric)
aw_2019$metric_name = "aw"
colnames(aw_2019) <- colnames(water_use_11_15)
aw_2019$year = as.numeric(aw_2019$year)
etaw_2019$metric_name = "etaw"
colnames(etaw_2019) <- colnames(water_use_11_15)
etaw_2019$year = as.numeric(etaw_2019$year)
ep_2019$metric_name = "ep"
colnames(ep_2019) <- colnames(water_use_11_15)
ep_2019$year = as.numeric(ep_2019$year)


read_xlsx(here("raw/water_balance/water_plan/Statewide_2020AgWaterDataByDauCO_VolumeNov8_23.xlsx"), 
          sheet = "2020StatewideICA", skip = 1) -> ica_2020
read_xlsx(here("raw/water_balance/water_plan/Statewide_2020AgWaterDataByDauCO_VolumeNov8_23.xlsx"), 
          sheet = "2020AW_VolumeStatewide", skip = 1) -> aw_2020
read_xlsx(here("raw/water_balance/water_plan/Statewide_2020AgWaterDataByDauCO_VolumeNov8_23.xlsx"), 
          sheet = "2020ETAW_VolumeStatewide", skip = 1) -> etaw_2020
read_xlsx(here("raw/water_balance/water_plan/Statewide_2020AgWaterDataByDauCO_VolumeNov8_23.xlsx"), 
          sheet = "2020EP_VolumeStatewide", skip = 1) -> ep_2020

ica_2020 <- ica_2020 %>% mutate(metric_name = "ica")
colnames(ica_2020) <- colnames(water_use_11_15)
aw_2020$metric_name = "aw"
colnames(aw_2020) <- colnames(water_use_11_15)
aw_2020$year = as.numeric(aw_2020$year)
etaw_2020$metric_name = "etaw"
colnames(etaw_2020) <- colnames(water_use_11_15)
etaw_2020$year = as.numeric(etaw_2020$year)
ep_2020$metric_name = "ep"
colnames(ep_2020) <- colnames(water_use_11_15)
ep_2020$year = as.numeric(ep_2020$year)

water_use_16_20 <- bind_rows(ica_2016, ica_2018, ica_2019, ica_2020, 
                             aw_2016, aw_2018, aw_2019, aw_2020, 
                             etaw_2016, etaw_2018, etaw_2019, etaw_2020, 
                             ep_2016, ep_2018, ep_2019, ep_2020)

## FIXME: something weird about dauco_id = 31136
water_use_16_20 %>% 
  filter(!is.na(year)) %>% 
  mutate(dauco_id = str_pad(dauco_id, width = 5, side = 'left', pad = "0")) %>% 
  select(-c("ro", "hr", "pa", "average_metric")) %>% 
  unique %>% 
  pivot_longer(cols  = c("grain", "rice", "cotton", "sugar_beet", "corn", "dry_beans",          
                         "safflower", "other_field", "alfalfa", "pasture", "tomato_processing",  
                         "tomato_fresh", "cucurbits", "onion_garlic", "potatoes", "truck_crops",        
                         "almonds_pistachios", "other_decidious", "citrus_subtropical", 
                         "vineyard"),
               names_to = 'crop', 
               values_to = 'value') %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% 
  pivot_wider(id_cols = c('year', 'dauco_id', 'crop'), 
              names_from = c('metric_name'),
              values_from = c('value'), 
              values_fn = mean) -> water_use_16_20_by_crop

water_use_02_10_by_crop %>% 
  bind_rows(water_use_11_15_by_crop, water_use_16_20_by_crop) %>% 
  mutate(consumptive_share = ifelse(is.nan(etaw / aw), NA, etaw/aw)) %>% 
  mutate(copy_etaw = etaw) %>% 
  # fix the few dauco_crops where the consumptive_share is > 1 
  # I think it's likely they just flipped etaw and aw 
  mutate(etaw = ifelse(consumptive_share > 1, aw, etaw)) %>% 
  mutate(aw = ifelse(consumptive_share > 1, copy_etaw, aw)) %>% 
  mutate(consumptive_share = etaw/aw) %>% 
  mutate(aw = ifelse(ica == 0, 0, aw)) %>% 
  mutate(etaw = ifelse(ica == 0, 0, etaw)) %>% 
  mutate(ep = ifelse(ica == 0, 0, ep)) %>% 
  mutate(et = etaw + ep) %>% 
  mutate(aw_share = ifelse(et > 0, etaw/et, NA)) %>% 
  select(-copy_etaw) -> water_use_by_crop

write_csv(water_use_by_crop, here("data/intermediate/water_use_by_crop.csv"))


