library(tidyverse)
library(here)
library(sf)


# Clean DAUCO shapefiles
# CITE: Hagerty (2024)

sf_use_s2(FALSE)

dauco_shapes <- st_read(here("raw/hagerty/shapefiles"), layer = "dauco_final")
dauco_shapes %>% 
  select(dauco_id, geometry, county_code = COUNTY_COD, county = COUNTY_NAM) %>% 
  mutate(dauco_num = dauco_id) %>% 
  mutate(dauco_id = str_pad(dauco_id, width = 5, side = "left", pad = "0")) -> dauco_shapes

saveRDS(dauco_shapes, here("data/intermediate/shapefiles/dauco_shapes.rds"))

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

