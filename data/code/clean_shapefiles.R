library(tidyverse)
library(here)
library(sf)

# Clean DAUCO and User shapefiles
# CITE: Hagerty (2024)

sf_use_s2(FALSE)

dauco_shapes <- st_read(here("raw/hagerty/shapefiles"), layer = "dauco_final")
dauco_shapes %>% 
  select(dauco_id, geometry, county_code = COUNTY_COD, county = COUNTY_NAM) %>% 
  mutate(dauco_num = dauco_id) %>% 
  mutate(dauco_id = str_pad(dauco_id, width = 5, side = "left", pad = "0")) -> dauco_shapes

saveRDS(dauco_shapes, here("data/intermediate/shapefiles/dauco_shapes.rds"))

user_shapes <- st_read(here("raw/hagerty/shapefiles/users_final.shp")) %>%   
                    mutate(geometry = st_make_valid(geometry)) 
saveRDS(user_shapes, here("data/intermediate/shapefiles/user_shapes.rds"))


# Clean Hyrdologic Region shapefiles and produce indexed xw

hr_shapes <- st_read(here("raw/shapefiles/i03_Hydrologic_Regions")) %>% 
  mutate(geometry = st_transform(geometry, st_crs(dauco_shapes$geometry))) %>% 
  rename(hr = HR_NAME) %>% select(-OBJECTID) %>% 
  group_by(hr) %>% filter(Shape__Are == max(Shape__Are)) %>% 
  ungroup %>% 
  arrange(hr) %>% 
  mutate(hr_index = seq(1, n(), by = 1)) 

hr_shapes %>% 
  st_drop_geometry %>% 
  select(hr, hr_index)  -> hr_index_xw

saveRDS(hr_shapes, here("data/intermediate/shapefiles/hr_shapes.rds"))
write_csv(hr_index_xw, here("data/intermediate/xws/hr_index_xw.csv"))

# Clean Groundwater Basin shapefiles 


gw_basins_raw <- st_read(here("raw/groundwater/i08_B118_CA_GroundwaterBasins"))
gw_basins_raw %>% 
  # use aggregate basins because data is scarce 
  # (sub basins are administrative not hydrological)
  select(basin_id = Basin_Subb, basin_name = Basin_Su_1, 
         agg_basin_id = Basin_Numb, agg_basin_name = Basin_Name, 
         acreage = Area_Acres, geometry) %>% 
  st_transform(crs(dauco_shapes)) %>% 
  group_by(basin_id, basin_name, agg_basin_id, agg_basin_name) %>% 
  summarize(acreage = sum(acreage),
            geometry = st_combine(geometry))  -> gw_basins

saveRDS(gw_basins, here("data/intermediate/shapefiles/gw_basins.rds"))

gw_basins_raw %>% 
  select(agg_basin_id = Basin_Numb, agg_basin_name = Basin_Name, 
         acreage = Area_Acres, geometry) %>% 
  st_transform(crs(dauco_shapes)) %>% 
  group_by(agg_basin_id, agg_basin_name) %>% 
  summarize(agg_basin_acreage = sum(acreage),
            geometry = st_combine(geometry))  -> gw_agg_basins

saveRDS(gw_agg_basins, here("data/intermediate/shapefiles/gw_agg_basins.rds"))

# clean urban utility shapefiles 

pws_shapes <- st_read(here("raw/shapefiles/California_Drinking_Water_System_Area_Boundaries")) %>% 
  mutate(pws_id = parse_number(SABL_PWSID)) %>% mutate(geometry = st_transform(geometry, st_crs(dauco_shapes$geometry)))
user_shapes <- st_read(here("raw/hagerty/shapefiles/users_final.shp"))



pws_shapes %>% 
  st_drop_geometry() %>% 
  select(SABL_PWSID, WATER_SY_1) %>% 
  group_by(pws_id) %>% 
  mutate(n()) -> what

pws_shapes %>% 
  group_by(pws_id) %>% 
  summarize(geometry = st_combine(geometry)) %>% 
  #left_join(unique_pws, by = "pws_id") %>% 
  #filter(match == 1) %>% 
  st_make_valid -> clean_pws

user_shapes %>% 
  select(pws_id = pwsid) %>% 
  mutate(pws_id = as.numeric(pws_id)) %>% 
  filter(!is.na(pws_id)) %>% 
  mutate(match = as.numeric(!(pws_id %in% clean_pws$pws_id))) %>% 
  filter(match == 1) -> clean_users

pws_final <- bind_rows(clean_pws, clean_users)

saveRDS(pws_final, here("data/intermediate/shapefiles/pws_final.rds"))

pws_final %>% 
  st_intersection(hr_shapes) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_drop_geometry() %>% 
  group_by(pws_id) %>% 
  filter(area == max(area)) %>% 
  select(pws_id, hr) -> pws_hr_xw

write_csv(pws_hr_xw, here("data/intermediate/xws/pws_hr_xw.csv"))

pws_final %>% 
  mutate(centroid = st_centroid(geometry)) %>% 
  st_drop_geometry() -> pws_centroid_points  

pws_final %>%
  st_drop_geometry %>% 
  bind_cols(st_coordinates(pws_centroid_points$centroid)) %>% 
  st_as_sf(coords = c("X", "Y")) %>%  
  mutate(geometry = st_set_crs(geometry, st_crs(dauco_shapes$geometry))) %>% 
  st_join(dauco_shapes) %>% 
  st_drop_geometry %>% 
  select(pws_id, dauco_id) -> pws_dauco_xw

write_csv(pws_dauco_xw, here("data/intermediate/xws/pws_dauco_xw.csv"))



