library(tidyverse)
library(here)


agg_gw <- read_csv(here("raw/groundwater/agg_gw_stats.csv"))

agg_gw %>% 
  rename(hr = `Hydrologic Region`) %>% 
  pivot_longer(cols = as.character(seq(2005, 2015, by = 1)), 
               names_to = "year", 
               values_to = "val") %>% 
  pivot_wider(id_cols = c("hr", "year"), names_from = metric, values_from = val) %>% 
  mutate(p_kwh = grid_cost_million/ grid_electricity_gwh) %>% 
  mutate(hr = ifelse(str_detect(hr, "Fran"), "San Francisco Bay", hr)) -> hr_pumping_stats

write_csv(hr_pumping_stats, here("data/intermediate/hr_pumping_stats.csv"))


#options("scipen"=10)
#numbers <- seq(1000, 290000, by = 1000)
#electricity_data <- read_csv("data/raw_data/electricity/ferc1_electicity_0.csv")
#for(num in numbers) {
#  filename <- paste0("data/raw_data/electricity/ferc1_electicity_", num, ".csv")
#  print(filename)
#  new_data <- read_csv(filename, show_col_types = FALSE)
#  electricity_data <- bind_rows(electricity_data, new_data)
#}
#
#utility_names <- read_xlsx("data/raw_data/electricity/FERC_CID_Listing_1-1-2024.xlsx", 
#                           skip = 2) %>% filter(State == "CA") %>% 
#  mutate(utility_name = str_to_lower(`Organization Name`)) %>% 
#  mutate(utility_name = str_sub(utility_name, 1, 10))
#
#
#electricity_data %>% 
#  mutate(utility_name_ferc1 = str_to_lower(utility_name_ferc1)) %>% 
#  mutate(new_name = str_sub(utility_name_ferc1, 1, 10)) %>% 
#  filter(new_name %in% utility_names$utility_name) -> ca_elec_data
#
#unique(ca_elec_data$rate_schedule_type)
