library(tidyverse)
library(tigris)
library(here)
library(sf)
library(ggmap)
library(tigris)
library(leaflet)
library(terra)
library(readxl)
library(viridis)
library(fixest)
options(stringsAsFactors = FALSE)


dauco_shapes <- readRDS(here("data/intermediate/shapefiles/dauco_shapes.rds"))
dauco_depths <- read_csv(here("data/intermediate/dauco_depths.csv"))
water_balance <- read_csv(here("data/intermediate/water_balance.csv"))
dauco_gw_share <- read_csv(here("data/intermediate/dauco_gw_share.csv"))
water_use_by_crop <- read_csv(here("data/intermediate/water_use_by_crop.csv"))
hr_pumping_stats <- read_csv(here("data/intermediate/hr_pumping_stats.csv"))

hr_pumping_stats %>% 
  group_by(year) %>%
  summarize(sum(grid_cost_million))

water_use_by_crop %>%  
  # FIXME: doesn't have water estimates in 2011
  filter(dauco_id != "20850") %>% 
  filter(crop != "sugar_beet") %>% 
  # FIXME: put these adjustments in the loading water use script instead of in here. 
  # weird dauco_ids (25415, 25515, 25615) shows up with different PAs in 2012 strangely?? 
  # all are in SCRO and have ica == na
  #filter(!(ro == "SCRO" & is.na(ica))) %>% 
  mutate(aw = ifelse(dauco_id == "23015" & year == 2014 & crop == "citrus_subtropical", etaw/0.8996416, aw)) %>% 
  mutate(etaw = ifelse(dauco_id == "23215" & year == 2011 & crop == "truck_crops", etaw*10, etaw)) %>% 
  mutate(etaw = ifelse(dauco_id == "23310" & year == 2011 & crop == "citrus_subtropical", etaw*10, etaw)) %>% 
  mutate(aw = ifelse(dauco_id == "23310" & year == 2011 & crop == "citrus_subtropical", etaw/0.87, aw)) %>% 
  select(year, dauco_id, crop, aw, etaw, ep, ica) %>% 
  # there are some daucos we don't observe in each year, but they either have no ag production or very little (<800 acres) 
  #filter(year %in% c(seq(2011, 2015, by = 1))) %>% 
  filter(year %in% c(seq(2002, 2020, by = 1))) %>% 
  group_by(dauco_id, crop) %>% 
  mutate(year_count = n())  %>% 
  filter(year_count == 18) %>% 
  mutate(ever_grown = as.numeric(max(ica) > 0)) %>% 
  # add some acreage to each crop 
  ungroup %>% 
  mutate(ica = ifelse(ica == 0, 1, ica)) %>% 
  left_join(dauco_shapes, by = 'dauco_id') %>% 
  ungroup %>% 
  st_drop_geometry %>% 
  mutate(aw = ifelse(is.na(aw), 0, aw)) %>% 
  group_by(dauco_id, crop) %>% 
  mutate(dauco_total_aw = sum(aw)) %>% 
  mutate(dauco_total_etaw = sum(etaw)) %>% 
  mutate(dauco_aw_per_acre = sum(aw)/sum(ica)) %>% 
  group_by(year, county, crop) %>% 
  mutate(county_total_aw = sum(aw)) %>% 
  mutate(county_total_etaw = sum(etaw)) %>% 
  mutate(county_aw_per_acre = sum(aw)/sum(ica)) %>% 
  group_by(year, crop) %>% 
  mutate(state_total_aw = sum(aw)) %>% 
  mutate(state_total_etaw = sum(etaw)) %>% 
  mutate(ca_aw_per_acre = sum(aw)/sum(ica)) %>% 
  ungroup  %>% 
  mutate(aw_per_acre = ifelse(aw != 0, aw/ica, 
                              ifelse(dauco_total_aw != 0, dauco_aw_per_acre,
                              ifelse(county_total_aw != 0, county_aw_per_acre, ca_aw_per_acre)))) %>% 
  ## FIXME: filling in price gaps with just averages?? idk 
  mutate(consumptive_share = ifelse(aw > 0, etaw/aw, 
                                    ifelse(dauco_total_aw > 0, dauco_total_etaw / dauco_total_aw, 
                                    ifelse(county_total_aw > 0, county_total_etaw/county_total_aw, state_total_etaw/state_total_aw))))  %>% 
  mutate(consumptive_share = ifelse(consumptive_share < 0.4, state_total_etaw/state_total_aw, consumptive_share)) %>% 
  # FIXME: TRINITY COUNTY has no GW depth data, drop for now %>% 
  # filter(county != "Trinity") %>% 
  select(year, dauco_id, crop, aw, etaw, ep, ica, ever_grown, aw_per_acre, consumptive_share) %>% 
  mutate(dauco_id = str_pad(dauco_id, 5, "left", pad = "0")) %>% 
  group_by(year, dauco_id) %>% 
  mutate(total_ica = sum(ica), 
         total_aw = sum(aw), 
         total_etaw = sum(etaw), 
         total_ep = sum(ep)) %>% 
  mutate(alpha = total_etaw/total_aw) %>% 
  mutate(zero_production_count = sum(as.numeric(ica == 0))) %>% 
  mutate(crop_count = 19 - zero_production_count) %>% 
  group_by(dauco_id) %>% 
  mutate(max_total_ica = max(total_ica)) %>%
  # need to get specific estimates of fallowing 
  ungroup %>% 
  mutate(total_irrigable_acreage = max_total_ica*1.01) %>% 
  mutate(crop_share = ifelse(ica != 0, 
                             ica/total_irrigable_acreage,
                             0.01*(1 - (total_ica/total_irrigable_acreage))/crop_count)) %>% 
  mutate(adj_aw = aw_per_acre*crop_share*total_irrigable_acreage) %>% 
  group_by(dauco_id, year) %>% 
  mutate(fallow_share = 1 - sum(crop_share)) %>%
  mutate(adj_total_aw = sum(adj_aw)) %>% 
  left_join(dauco_gw_share %>% mutate(dauco_id = str_pad(dauco_id, 5, "left", pad = "0")), 
            by = c("dauco_id", "year")) %>% 
  filter(!is.na(groundwater_share)) %>% 
  mutate(sw = (1 - groundwater_share)*adj_total_aw) %>% 
  filter(!is.na(sw)) %>% 
  mutate(gw = adj_total_aw - sw) %>% 
  left_join(dauco_depths %>% 
              mutate(year = as.numeric(year)) %>% 
              mutate(dauco_id = str_pad(dauco_id, 5, "left", pad = "0")), 
            by = c("dauco_id", "year")) %>%  
  rowwise %>%
  mutate(depth = max(depth, 1)) -> long_dauco_data

long_dauco_data %>% 
  filter(is.na(depth)) %>% 
  group_by(dauco_id) %>% 
  mutate(alltime_gw = sum(gw)) %>% 
  filter(alltime_gw > 0) %>% 
  select(dauco_id, year) %>% 
  unique %>% 
  group_by(dauco_id) %>% 
  summarize(count = n()) %>% 
  pull(dauco_id) -> gw_users_missing_depth

dauco_dist <- read_csv(here("raw/long_dauco_distances.csv"))

# impute missing depth data by averaging the closest 3 dauco depths by 1/distance
dauco_dist %>% 
  mutate(dauco_id = str_pad(dauco_id, 5, "left", "0")) %>% 
  mutate(other = str_pad(other, 5, "left", "0")) %>% 
  filter(dauco_id %in% gw_users_missing_depth) %>% 
  filter(!(other %in% gw_users_missing_depth)) %>% 
  group_by(dauco_id) %>% 
  slice_min(order_by = distance, n = 3) %>% ungroup %>% 
  left_join(dauco_depths %>%
              mutate(dauco_id = 
                       str_pad(dauco_id, 5, "left", "0")), 
            by = c("other" = "dauco_id")) %>% 
  filter(!is.na(year)) %>% 
  group_by(dauco_id, year) %>% 
  summarize(fix_depth = sum((1/distance)*depth)/sum(1/distance), 
            fix_gamma = sum((1/distance)*gamma)/sum(1/distance)) %>% 
  mutate(dauco_id = as.character(dauco_id)) %>% 
  mutate(year = as.numeric(year)) -> missing_depth_fixes



long_dauco_data %>% 
  left_join(missing_depth_fixes) %>% 
  mutate(depth = ifelse(is.na(depth), fix_depth, depth), 
         gamma = ifelse(is.na(gamma), fix_gamma, gamma)) %>%
  mutate(gamma = 30*gamma) %>% 
  #mutate(gamma = 6.35e-4) %>% 
  select(-fix_depth) %>% select(-fix_gamma) %>% 
  # drop one more dauco that is being annoying 00753
  filter(!is.na(depth)) -> long_dauco_data
  
##### estimate pumping efficiency 

water_balance %>% 
  select(dauco_id = DAU, pa = PA, hr = HR_NAME) %>% 
  unique %>% 
  mutate(dauco_id = str_pad(parse_number(dauco_id), 5, "left", "0")) -> regions_xw


water_balance %>% 
  filter(CategoryA == "Water Supplies") %>% 
  filter(str_detect(CategoryC, "Groundwater")) %>% 
  filter(str_detect(CategoryC, "Agriculture") | str_detect(CategoryC, "Urban")) %>% 
  mutate(dauco_id = str_pad(parse_number(DAU), 5, "left", "0")) %>% 
  mutate(af = 1000*KAcreFt) %>% 
  select(CategoryC, dauco_id, year = Year, af) %>% unique %>% 
  group_by(dauco_id, year) %>% 
  summarize(total_gw = sum(af)) -> total_gw_by_dauco

long_dauco_data %>% 
  ungroup %>% 
  filter(year %in% seq(2005, 2015, by = 1)) %>% 
  select(year, dauco_id, gw, depth, gamma) %>% 
  unique %>% 
  left_join(regions_xw, by = c("dauco_id")) %>% 
  group_by(year, pa) %>% 
  mutate(pa_depth = mean(depth, na.rm = TRUE)) %>% 
  mutate(pa_gamma = mean(gamma, na.rm = TRUE)) %>% 
  ungroup %>% 
  # need to improve depth data so I don't have to do this bullshit 
  mutate(depth = ifelse(is.na(depth), mean(depth, na.rm = TRUE), depth))  %>% 
  mutate(depth = ifelse(depth <= 0, 2, depth)) %>% 
  mutate(gamma = ifelse(is.na(gamma), mean(gamma, na.rm = TRUE), gamma)) %>% 
  rowwise %>% 
  left_join(total_gw_by_dauco) %>% 
  # TOTAL GW vs GW IS A NEW CHANGE 08/19/24 THAT HAS NOT BEEN SAVED/CARRIED THROUGH
  mutate(unweighted_kwh = (total_gw*depth + 0.5*gamma*(total_gw^2))) %>% 
  left_join(hr_pumping_stats %>% mutate(year = as.numeric(year)), by = c("year", "hr")) %>% 
  mutate(target_hr_kwh = 1e6*energy_gwh) -> dauco_pumping_efficiency 

dauco_pumping_efficiency %>% 
  group_by(year,pa, hr, target_hr_kwh) %>% 
  summarize(unweighted_kwh = sum(unweighted_kwh)) %>% 
  pivot_wider(id_cols = c("year", "hr", "target_hr_kwh"), 
              names_from = pa, names_prefix = "pa", 
              values_from = unweighted_kwh) -> pump_efficiency_data

# THIS DIDN"T WORK AT ALL
# for(curr_hr in unique(pump_efficiency_data$hr)) {
#   pump_efficiency_data %>% 
#     ungroup %>%
#     filter(hr == curr_hr) %>% 
#     select_if(~all(!is.na(.))) -> hr_efficiency_data
#     
#   pa_vars <- hr_efficiency_data %>% select(contains("pa")) %>% colnames
#   
#   lm.formula <- as.formula(paste0("target_hr_kwh ~ - 1 + ", paste(pa_vars, collapse = " + "))) 
#   model <-  lm(lm.formula, hr_efficiency_data)
#   
#   # Non-negative least squares optimization
#   nnls_result <- nnls(model.matrix(model), hr_efficiency_data$target_hr_kwh)
#   
#   print(nnls_result)
# }

pump_efficiency_data %>% 
  pivot_longer(cols = contains("pa"), 
               names_to = "pa", 
               values_to = "unweighted_kwh") %>% 
  group_by(year, hr) %>% 
  summarize(target_hr_kwh = max(target_hr_kwh), 
            unweighted_kwh = sum(unweighted_kwh, na.rm = TRUE)) %>% 
  feols(target_hr_kwh ~ -1 + hr:unweighted_kwh) -> hr_efficiency_reg

pump_coefs <- as.data.frame(coef(hr_efficiency_reg))
colnames(pump_coefs) <- "rho"
pump_coefs$hr <- rownames(pump_coefs)
pump_coefs %>% 
  mutate(hr = str_remove(hr, ":unweighted_kwh")) %>% 
  mutate(hr = str_remove(hr, "hr")) -> pump_coefs
rownames(pump_coefs) <- NULL  

dauco_pumping_efficiency %>% 
  left_join(pump_coefs) %>% 
  mutate(gw_cost = rho*p_kwh*unweighted_kwh) %>% 
  mutate(gw_ac = gw_cost/gw) %>% 
  mutate(gw_mc = rho*p_kwh*(depth + gamma*gw)) %>% 
  select(year, dauco_id, depth, gamma, rho, p_kwh, gw, gw_cost, gw_ac, gw_mc) -> gw_cost_data

gw_cost_data %>% 
  ungroup %>% 
  summarize(sum(gw*gw_mc, na.rm = TRUE)/sum(gw, na.rm = TRUE))

long_dauco_data %>% 
  left_join(gw_cost_data) -> long_dauco_data


write_csv(long_dauco_data, here("data/intermediate/long_dauco_data.csv"))

### make final data 

long_dauco_data %>% 
  filter(!is.na(gw_mc)) %>% 
  #filter(year %in% c(2012, 2013, 2014, 2015)) %>% 
  filter(year != 2009) %>% ungroup -> rest_dauco_data

write_csv(rest_dauco_data, here("data/intermediate/rest_dauco_data.csv"))

### write for matlab 

rest_dauco_data %>% 
  select(year, dauco_id, depth) %>% 
  unique %>% 
  arrange(year, dauco_id) %>% 
  pivot_wider(id_cols = dauco_id, names_from = year, values_from = depth) %>% 
  select(-dauco_id) -> depth_data 

write_csv(depth_data, here("data/intermediate/matlab/depth_data.csv"))


rest_dauco_data %>% 
  select(dauco_id, gamma) %>% 
  group_by(dauco_id) %>% 
  summarize(gamma = max(gamma)) %>% 
  select(-dauco_id)  -> gamma_data 

write_csv(gamma_data, here("data/intermediate/matlab/gamma_data.csv"))

rest_dauco_data %>% 
  select(dauco_id, total_irrigable_acreage) %>% 
  group_by(dauco_id) %>% 
  summarize(L = max(total_irrigable_acreage)) %>% 
  select(-dauco_id)  -> L_data 

write_csv(L_data, here("data/intermediate/matlab/L_data.csv"))

rest_dauco_data %>% 
  select(year, dauco_id, sw) %>% 
  unique %>% 
  arrange(year, dauco_id) %>% 
  pivot_wider(id_cols = dauco_id, names_from = year, values_from = sw) %>% 
  select(-dauco_id) -> sw_data 

write_csv(sw_data, here("data/intermediate/matlab/sw_data.csv"))

rest_dauco_data %>% 
  select(dauco_id, rho) %>% 
  unique %>% 
  arrange(dauco_id) %>% 
  select(-dauco_id) -> rho_data 

write_csv(rho_data, here("data/intermediate/matlab/rho_data.csv"))

rest_dauco_data %>% 
  select(year, dauco_id, p_kwh) %>% 
  unique %>% 
  arrange(year, dauco_id) %>% 
  pivot_wider(id_cols = dauco_id, names_from = year, values_from = p_kwh) %>% 
  select(-dauco_id) -> p_kwh_data 

write_csv(p_kwh_data, here("data/intermediate/matlab/p_kwh_data.csv"))

#rest_dauco_data %>% 
#  select(dauco_id, noGW) %>% 
#  unique %>% 
#  arrange(dauco_id) %>% 
#  select(-dauco_id) -> noGW_data 
#
#write_csv(noGW_data, here("data/intermediate/matlab/noGW_data.csv"))

# rest_dauco_data %>% 
#   select(year, dauco_id, crop, price) %>% 
#   arrange(year, dauco_id, crop) %>% 
#   pivot_wider(id_cols = c("year", "dauco_id"), names_from = crop, values_from = price) %>% 
#   arrange(year, dauco_id) %>% 
#   select(-year, -dauco_id) -> price_data 
# 
# write_csv(price_data, here("data/intermediate/matlab/price_data.csv"))

rest_dauco_data %>% 
  select(year, dauco_id, crop, aw_per_acre) %>% 
  arrange(year, dauco_id, crop) %>% 
  pivot_wider(id_cols = c("year", "dauco_id"), names_from = crop, values_from = aw_per_acre) %>% 
  arrange(year, dauco_id) %>% 
  select(-year, -dauco_id) -> aw_data 

write_csv(aw_data, here("data/intermediate/matlab/aw_data.csv"))

rest_dauco_data %>% 
  select(year, dauco_id, crop, ever_grown) %>% 
  arrange(year, dauco_id, crop) %>% 
  pivot_wider(id_cols = c("year", "dauco_id"), names_from = crop, values_from = ever_grown) %>% 
  arrange(year, dauco_id) %>% 
  select(-year, -dauco_id) -> ever_grown_data 

write_csv(ever_grown_data, here("data/intermediate/matlab/ever_grown_data.csv"))

rest_dauco_data %>% 
  select(year, dauco_id, crop, crop_share) %>% 
  arrange(year, dauco_id, crop) %>% 
  pivot_wider(id_cols = c("year", "dauco_id"), names_from = crop, values_from = crop_share) %>% 
  arrange(year, dauco_id) %>% 
  select(-year, -dauco_id) -> share_data 

write_csv(share_data, here("data/intermediate/matlab/share_data.csv"))

rest_dauco_data %>% 
  select(year, dauco_id, total_aw, total_etaw) %>% 
  unique() %>% 
  mutate(alpha = total_etaw/total_aw) %>% 
  # for some reason dauco 25615 does not have any water use in 2012 
  mutate(alpha = ifelse(alpha < 0.5, 0.79, alpha)) %>% 
  mutate(alpha = ifelse(is.na(alpha), 0.8672701, alpha)) %>% 
  select(year, dauco_id, alpha) %>% 
  pivot_wider(id_cols = dauco_id, names_from = year, values_from = alpha) %>% 
  select(-dauco_id) -> alpha_data

write_csv(alpha_data, here("data/intermediate/matlab/alpha_data.csv"))

dauco_ag_supply_agg %>% 
  filter(water_source != "gw") %>% 
  mutate(dauco_id = str_pad(dauco_id, 5, "left", "0")) %>% 
  filter(dauco_id %in% rest_dauco_data$dauco_id) %>% 
  filter(year %in% rest_dauco_data$year) %>% 
  group_by(dauco_id, year) %>% 
  mutate(total_volume = sum(total_supply)) %>%
  filter(water_source == "entitlement") %>% 
  mutate(entitlement_share = total_supply/total_volume) %>% 
  ungroup %>% 
  mutate(entitlement_share = ifelse(is.nan(entitlement_share), 0, entitlement_share)) %>% 
  arrange(year, dauco_id) %>% 
  pivot_wider(id_cols = dauco_id, names_from = year, values_from = entitlement_share) %>% 
  select(-dauco_id) -> entitlement_shares

write_csv(entitlement_shares, here("data/intermediate/matlab/entitlement_data.csv"))

dauco_hr_xw <- read_csv(here("data/intermediate/xws/dauco_hr_xw.csv"))
hr_index_xw <- read_csv(here("data/intermediate/xws/hr_index_xw.csv"))


rest_dauco_data %>% 
  mutate(unit_id = as.numeric(dauco_id)) %>%
  select(unit_id) %>% 
  unique() %>% 
  arrange(unit_id) %>% 
  left_join(dauco_hr_xw %>% mutate(dauco_id = as.numeric(dauco_id)), 
            by = c("unit_id" = "dauco_id")) %>% 
  left_join(hr_index_xw) %>% 
  mutate(sim_id = seq(1, n(), by = 1)) %>% 
  mutate(is_dauco = 1) %>% 
  select(unit_id, sim_id, hr_index, is_dauco) -> farmer_unit_info

write_csv(farmer_unit_info, here("data/intermediate/matlab/farmer_unit_info.csv"))


#hr_nums %>% rename(seller_region = hr, seller_hr_num = hr_num) %>% 
#  merge(data.frame(year = unique(rest_dauco_data$year))) %>% 
#  merge(data.frame(asset_type = c("right", "entitlement"))) %>% 
#  merge(hr_nums %>% rename(buyer_region = hr, buyer_hr_num = hr_num)) %>% 
#  left_join(hr_trade_amount) %>% 
#  select(seller_hr_num, buyer_hr_num, year, asset_type, transfer_volume) %>% 
#  arrange(seller_hr_num, buyer_hr_num) -> all_hr_trade_data
#
#for(curr_year in unique(rest_dauco_data$year)) {
#  
#  all_hr_trade_data %>% 
#    filter(year == curr_year) %>% 
#    filter(asset_type == "right") %>% 
#    mutate(transfer_volume = ifelse(is.na(transfer_volume), 0, transfer_volume)) %>%
#    arrange(seller_hr_num, buyer_hr_num) %>% 
#    pivot_wider(id_cols = seller_hr_num, names_from = buyer_hr_num, 
#                names_prefix = "hr_", values_from = transfer_volume) %>% 
#    select(-seller_hr_num) -> right_trade_matrix
#  
#  write_csv(right_trade_matrix, here(paste0("data/intermediate/matlab/right_trade_matrix_", curr_year, ".csv")))
#  
#  all_hr_trade_data %>% 
#    filter(year == curr_year) %>% 
#    filter(asset_type == "entitlement") %>% 
#    mutate(transfer_volume = ifelse(is.na(transfer_volume), 0, transfer_volume)) %>%
#    arrange(seller_hr_num, buyer_hr_num) %>% 
#    pivot_wider(id_cols = seller_hr_num, names_from = buyer_hr_num, 
#                names_prefix = "hr_", values_from = transfer_volume) %>% 
#    select(-seller_hr_num) -> entitle_trade_matrix
#  
#  write_csv(entitle_trade_matrix, here(paste0("data/intermediate/matlab/entitle_trade_matrix_", curr_year, ".csv")))
#
#  
#}


##### SCRATCH ######## 

regions_xw %>% 
  select(hr) %>% unique %>% 
  arrange(hr) %>% 
  mutate(hr_num = seq(1, n(), by = 1)) -> hr_nums

regions_xw %>% 
  filter(dauco_id %in% rest_dauco_data$dauco_id) %>% 
  unique %>% left_join(hr_nums) %>% arrange(dauco_id) %>% 
  select(dauco_id, hr_num) -> dauco_hr_xw

urban_wtp_data <- read_csv(here("data/clean_data/urban_wtp_data_from_audit.csv"))

urban_wtp_data %>% 
  filter(year == 2018) %>% 
  mutate(dem_constant = (exp(dem_intercept))) %>% 
  mutate(Q_bar = vol_supply) %>% 
  mutate(sim_id = seq(length(unique(rest_dauco_data$dauco_id)) + 1, 
                      length(unique(rest_dauco_data$dauco_id)) + n(), 
                      by = 1)) %>% 
  select(pws_id, sim_id, price_coef, dem_constant, Q_bar, phi, mwtp) %>% 
  #mutate(more = ((100/dem_constant)^price_coef) - Q_bar) %>% 
  arrange(pws_id) %>% 
  mutate(phi = ifelse(is.na(phi), 0, phi)) -> urban_data_for_sim

write_csv(urban_data_for_sim, "data/clean_data/urban_data_for_sim.csv")

rest_dauco_data %>% 
  mutate(unit_id = as.numeric(dauco_id)) %>%
  select(unit_id) %>% 
  unique() %>% 
  arrange(unit_id) %>% 
  left_join(dauco_hr_xw %>% mutate(dauco_id = as.numeric(dauco_id)), 
            by = c("unit_id" = "dauco_id")) %>% 
  bind_rows(pws_hr_xw %>% 
              filter(pws_id %in% urban_data_for_sim$pws_id) %>% 
              arrange(pws_id) %>% 
              left_join(hr_nums) %>% 
              select(unit_id = pws_id, hr_num) %>% 
              mutate(unit_id = as.numeric(unit_id))) %>% 
  mutate(sim_id = seq(1, n(), by = 1)) %>% 
  mutate(is_dauco = as.numeric(sim_id <= nrow(depth_data))) %>% 
  select(unit_id, sim_id, hr_num, is_dauco) -> unit_info

write_csv(unit_info, here("data/clean_data/unit_info.csv"))

##### SCRATCH ########## 

pws_dauco_xw %>% 
  arrange(dauco_id) %>% 
  left_join(dauco_dist) %>% 
  mutate(other = str_pad(other, 5, "left", "0")) %>% 
  filter(other %in% rest_dauco_data$dauco_id) %>% 
  arrange(pws_id, other) -> pws_dauco_distances

dauco_dist %>% 
  mutate(dauco_id = str_pad(dauco_id, 5, "left", "0")) %>% 
  mutate(other = str_pad(other, 5, "left", "0")) %>% 
  arrange(dauco_id, other) -> long_dauco_dist 

unit_info %>% 
  select(unit_id) %>% 
  merge(unit_info %>% select(other_id = unit_id)) %>% 
  left_join(pws_dauco_xw %>% mutate(dauco_id = str_pad(dauco_id, 5, "left", "0")), by = c("unit_id" = "pws_id")) %>% 
  left_join(pws_dauco_xw %>% mutate(other = str_pad(dauco_id, 5, "left", "0")) %>% select(other, pws_id), by = c("other_id" = "pws_id"))  %>% 
  mutate(dauco_id = ifelse(is.na(dauco_id), unit_id, dauco_id)) %>% 
  mutate(other= ifelse(is.na(other), other_id, other)) %>% 
  left_join(unit_info %>% select(unit_id, seller_id = sim_id)) %>% 
  left_join(unit_info %>% select(other_id = unit_id, buyer_id = sim_id))  %>% 
  left_join(long_dauco_dist) %>% 
  select(seller_id, buyer_id, distance) %>% 
  arrange(seller_id, buyer_id) %>% 
  # distance problem (handful of NAs) FIX LATER
  mutate(distance = ifelse(is.na(distance), 
                           mean(distance, na.rm = TRUE),
                           distance))-> distance_data_long


write_csv(distance_data_long, here("data/clean_data/distance_data_long.csv"))

######### estimate gamma 
##### NONE OF THIS WORKED 



long_dauco_data %>%
  group_by(dauco_id, year) %>% 
  summarize(count = n()) -> fuck

raw_rain <- read_csv("data/raw_data/rain/clean_rain_data.csv")

raw_rain %>% 
  mutate(rain_id = seq(1, nrow(.), by = 1)) %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  mutate(geometry = st_set_crs(geometry, st_crs(dauco_shapes$geometry))) -> rain_shapes

rain_shapes %>% 
  st_intersection(dauco_shapes) %>% 
  st_drop_geometry %>% 
  group_by(year, dauco_id) %>% 
  summarize(rain = mean(rain, na.rm = TRUE)) -> dauco_rain

rm(rain_shapes)
rm(raw_rain)

long_dauco_data %>% 
  select(dauco_id, year, total_irrigable_acreage, sw, gw, total_ep, alpha, depth) %>% 
  unique %>% left_join(regions_xw) %>% 
  left_join(dauco_rain %>% mutate(dauco_id = str_pad(dauco_id, 5, "left", "0"))) %>% 
  arrange(dauco_id, year) %>%  # Sort the data by id and year
  group_by(dauco_id) %>%
  mutate(depth_next_year = lead(depth)) %>%
  ungroup() %>% 
  mutate(depth_change = depth_next_year - depth) %>% 
  mutate(rain = (rain * total_irrigable_acreage *404.86)/(1000*3048.8)) %>% 
  filter(rain < 50) %>% 
  mutate(new_water = alpha*(sw + gw + rain - total_ep)) %>% 
  mutate(new_water_per = new_water/total_irrigable_acreage) %>% 
  mutate(adj_gw = (1-alpha)*gw) %>% 
  mutate(adj_gw_per = adj_gw / total_irrigable_acreage) %>% 
  filter(gw > 0) %>% 
  feols(depth_next_year ~ -1 + depth + adj_gw_per + new_water | dauco_id + year) -> reg_est

summary(reg_est)


rest_dauco_data %>% 
  filter(year == 2014) %>% 
  filter(dauco_id == "00753")




