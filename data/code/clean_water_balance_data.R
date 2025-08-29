library(tidyverse)
library(here)


########## Load and combine water balance data for 2002-2020 ############

years <- seq(2002, 2016, by = 1)
year_folders <- data.frame(folder =  list.files(here("raw/water_balance/CA_DWR_WaterBalance_20210519_wys2002-2016")))
water_balance <- data.frame()

for(year in years) {
  year_folders %>% 
    filter(str_detect(folder, paste0("_", year))) %>% 
    filter(!str_detect(folder, ".txt")) %>% 
    pull(folder) -> folder
  
  new_wb <- read_csv(here(paste0("raw/water_balance/CA_DWR_WaterBalance_20210519_wys2002-2016/",
                                 folder, "/CA-DWR-WaterBalance-Level2-DP-1000-", year, "-DAUCO.csv")),
                     show_col_types = FALSE)
  
  water_balance <- bind_rows(water_balance, new_wb)
  
}

wb2018 <- read_csv(here("raw/water_balance/ca_dwr_waterbalance_20230721_2018-md563baa7cb32b496cd92e74b6360364837/CA-DWR-WaterBalance-Level2-DP-1000-2018-DAUCO.csv"))
wb2019 <- read_csv(here("raw/water_balance/ca_dwr_waterbalance_20230721_2019-md5fd89bcf58101a493c9e3cd82755dad7b/CA-DWR-WaterBalance-Level2-DP-1000-2019-DAUCO.csv"))
wb2020 <- read_csv(here("raw/water_balance/ca_dwr_waterbalance_20230721_2020-md54a092de3d7cf29fdc34aa58549e79859/CA-DWR-WaterBalance-Level2-DP-1000-2020-DAUCO.csv"))

water_balance <- bind_rows(water_balance, wb2018, wb2019, wb2020)

write_csv(water_balance, here("data/intermediate/water_balance.csv"))

##### Produce other intermediate tables from water_balance data #######

groundwater_vars <- c("Groundwater Extraction - Unadjudicated - Agriculture", 
                      "Groundwater Extraction - Adjudicated - Agriculture",
                      "Groundwater Extraction - Banked - Agriculture")

water_balance  %>% 
  filter(CategoryA == "Water Supplies") %>% 
  filter(CategoryC %in% groundwater_vars) %>% 
  group_by(DAU,Year) %>% 
  summarize(groundwater = sum(KAcreFt)*1000) %>% 
  group_by(DAU) %>% 
  mutate(year_count = n()) %>% 
  # drop some weird daucos that go in and out, 
  # most have 0 gw other have small amounts
  filter(year_count == 18) %>% 
  select(-year_count) %>% 
  ungroup -> dauco_groundwater

write_csv(dauco_groundwater, here("data/intermediate/dauco_groundwater.csv"))

water_balance %>% 
  filter(CategoryA == "Agriculture") %>% 
  filter(CategoryC == "Applied Water") %>% 
  mutate(balance_aw = KAcreFt*1000) %>% 
  select(DAU, Year, balance_aw) %>%
  # later years have weird duplicates with zeros 
  group_by(DAU, Year) %>% 
  summarize(balance_aw = max(balance_aw)) %>% 
  group_by(DAU) %>% 
  mutate(year_count = n()) %>% 
  filter(year_count == 18) %>% 
  select(-year_count) %>% 
  ungroup-> dauco_balance_aw

write_csv(dauco_balance_aw, here("data/intermediate/dauco_balance_aw.csv"))

dauco_groundwater %>% 
  left_join(dauco_balance_aw, by = c("DAU", "Year")) %>% 
  mutate(dauco_id = parse_number(DAU)) %>% 
  select(dauco_id, year = Year, balance_aw, groundwater) %>% 
  mutate(groundwater_share = ifelse(balance_aw != 0 , groundwater/balance_aw, 0)) %>% 
  # QUICK FIX ME: one dauco has groundwater share > 1 
  mutate(groundwater_share = ifelse(groundwater_share > 1, 1, groundwater_share)) %>% 
  select(dauco_id, year, groundwater_share) -> dauco_gw_share

write_csv(dauco_gw_share, here("data/intermediate/dauco_gw_share.csv"))

water_balance %>% 
  filter(CategoryA == "Urban") %>% 
  select(CategoryC) %>% unique %>% pull(CategoryC) -> urban_water_demand_vars

water_balance %>% 
  filter(CategoryA == "Water Supplies") %>% 
  filter(str_detect(CategoryC, "Urban")) %>% 
  select(CategoryC) %>% unique %>% pull(CategoryC) -> urban_water_supply_vars

supply_xw <- read_csv(here("raw/xws/urban_water_supply_xw.csv"))

water_balance %>% 
  filter(CategoryC %in% urban_water_supply_vars) %>% 
  mutate(dauco_id = parse_number(DAU)) %>% 
  group_by(dauco_id, hr = HR_NAME, year = Year, CategoryC) %>% 
  summarize(supply = sum(1000*KAcreFt)) -> dauco_urban_supply

#write_csv(dauco_urban_supply, here("data/intermediate/dauco_urban_supply.csv"))

dauco_urban_supply %>% 
  mutate(water_source = ifelse(str_detect(CategoryC, "Groundwater"), "gw", 
                               ifelse(str_detect(CategoryC, "Central Valley"), "entitlement", 
                                      ifelse(str_detect(CategoryC, "State Water"), "entitlement",
                                             ifelse(str_detect(CategoryC, "Transfer"), "transfer", 
                                                    ifelse(str_detect(CategoryC, "Imports"), "right", "right")))))) %>% 
  group_by(year, dauco_id, hr, water_source) %>% 
  summarize(total_supply = sum(supply)) -> dauco_urban_supply_agg

#write_csv(dauco_urban_supply_agg, here("data/intermediate/dauco_urban_supply_agg.csv"))

#water_balance %>% 
#  filter(CategoryC %in% urban_water_supply_vars) %>% 
#  mutate(dauco_id = parse_number(DAU)) %>% 
#  left_join(supply_xw, by = "CategoryC") %>% 
#  group_by(dauco_id, hr = HR_NAME, year = Year, supply_source) %>% 
#  summarize(supply = sum(1000*KAcreFt)) -> dauco_urban_supply_agg_2

water_balance %>% 
  filter(CategoryC %in% urban_water_supply_vars) %>% 
  group_by(year = Year, CategoryC) %>% 
  summarize(supply = sum(1000* KAcreFt)) -> supply_stats 

water_balance %>% 
  filter(CategoryC %in% urban_water_supply_vars) %>% 
  left_join(supply_xw, by = "CategoryC") %>% 
  group_by(year = Year, supply_source) %>% 
  summarize(supply = sum(1000* KAcreFt)) -> agg_supply_stats 

supply_stats %>% 
  group_by(year) %>% 
  mutate(supply_share = supply/sum(supply),
         total_supply = sum(supply)) -> supply_totals

water_balance %>% 
  filter(CategoryA == "Urban") %>% 
  filter(str_detect(CategoryC, "Applied Water -")) %>% 
  filter(!str_detect(CategoryC, "Net Water Use")) %>% 
  mutate(af = 1000 * KAcreFt) %>% 
  mutate(dauco_id = parse_number(DAU)) %>% 
  select(CategoryA, year = Year, dauco_id, CategoryC, af) %>% 
  group_by(year, dauco_id) %>% 
  summarize(urban_aw = sum(af)) -> total_urban_aw

water_balance %>% 
  filter(CategoryA == "Water Supplies") %>% 
  filter(str_detect(CategoryC, "Agriculture")) %>% 
  select(CategoryC) %>% unique %>% pull(CategoryC) -> ag_water_supply_vars

water_balance %>% 
  filter(CategoryC %in% ag_water_supply_vars) %>% 
  mutate(dauco_id = parse_number(DAU)) %>% 
  group_by(dauco_id, hr = HR_NAME, year = Year, CategoryC) %>% 
  summarize(supply = sum(1000*KAcreFt)) -> dauco_ag_supply

dauco_ag_supply %>% 
  mutate(water_source = ifelse(str_detect(CategoryC, "Groundwater"), "gw", 
                               ifelse(str_detect(CategoryC, "Central Valley"), "entitlement", 
                                      ifelse(str_detect(CategoryC, "State Water"), "entitlement",
                                             ifelse(str_detect(CategoryC, "Transfer"), "entitlement", 
                                                    ifelse(str_detect(CategoryC, "Imports"), "entitlement", "right")))))) %>% 
  group_by(year, dauco_id, hr, water_source) %>% 
  summarize(total_supply = sum(supply)) -> dauco_ag_supply_agg

# Maybe use later, I don't know yet. 

water_balance %>% 
  select(dauco_id = DAU, dau_name = DAU_NAME) %>% 
  unique %>% 
  mutate(dauco_id = parse_number(dauco_id))-> dau_names

dau_names %>% 
  group_by(dau_name) %>%
  summarize(count = n()) -> dau_duplicates

dauco_balance_aw %>% 
  mutate(dauco_id = as.character(parse_number(DAU))) %>% 
  mutate(dauco_id = str_pad(dauco_id, 5, "left", pad = "0")) %>% 
  mutate(dau_id = str_sub(dauco_id, 1, 3)) %>% 
  group_by(year = Year, dau_id) %>% 
  mutate(total_dau_aw = sum(balance_aw)) %>% 
  ungroup %>% 
  mutate(dau_share = ifelse(total_dau_aw > 0, balance_aw/total_dau_aw, NA)) %>% 
  select(year, dauco_id, dau_id, balance_aw, dau_share, total_dau_aw) -> dauco_dau_xw

