#### DWR DATA
gw_levels_raw <- read_csv(here("raw/groundwater/periodic_gwl_bulkdatadownload/measurements.csv"))
gw_stations_raw <- read_csv(here("raw/groundwater/periodic_gwl_bulkdatadownload/stations.csv"))
gw_basins<- readRDS(here("data/intermediate/shapefiles/gw_basins.rds"))
gw_agg_basins<- readRDS(here("data/intermediate/shapefiles/gw_agg_basins.rds"))


gw_stations_raw %>% 
  select(site_code, latitude, longitude) %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(st_crs(dauco_shapes)) -> gw_stations


gw_stations %>% 
  st_join(dauco_shapes, by = "geometry") %>% 
  st_drop_geometry() -> site_dauco_xw

## we are going to use basins which may overlap many DAUCos 
## to make hte most of point well measurements. 
## for well points that don't match basins, we'll match
## them to DAUCos. 

gw_stations %>% 
  st_join(gw_basins, by = "geometry") %>% 
  left_join(st_drop_geometry(gw_agg_basins), by = c("agg_basin_id", "agg_basin_name")) %>% 
  st_drop_geometry()-> site_basin_xw

site_basin_xw %>% 
  filter(is.na(basin_id)) %>% 
  select(site_code) %>% unique %>% 
  pull(site_code) -> unmatched_site_codes


dauco_shapes %>% 
  st_join(gw_stations) %>% 
  group_by(dauco_id) %>% 
  summarize(count = n()) -> dauco_station_count

site_basin_xw %>% 
  filter(is.na(basin_name)) %>% 
  pull(site_code) -> no_basin_site_codes

site_dauco_xw %>% 
  filter(site_code %in% no_basin_site_codes) %>% 
  # there are some sites that are in Oregon on the border
  filter(!is.na(dauco_id)) -> no_basin_dauco_xw

dauco_shapes %>% 
  mutate(dauco_area = st_area(geometry)) %>% 
  st_intersection(gw_basins, by = "geometry") %>%  
  mutate(area = st_area(geometry)) %>% 
  group_by(dauco_id) %>% 
  mutate(basin_weight = area/sum(area)) %>% 
  select(dauco_id, basin_id, dauco_area, acreage, area, basin_weight) %>% 
  mutate(basin_overlap = area/dauco_area) %>% 
  st_drop_geometry() -> dauco_basin_xw


dauco_basin_xw %>% 
  group_by(dauco_id) %>% 
  summarize(dauco_basin_area = sum(basin_weight*acreage)/sum(basin_weight)) %>% 
  mutate(gamma = 5/dauco_basin_area) -> dauco_gammas

dauco_basin_xw %>% 
  group_by(dauco_id) %>% 
  summarize(potential_basin_overlap = sum(basin_overlap)) -> pot_basin_overlap

dauco_shapes %>% 
  st_intersection(gw_agg_basins, by = "geometry") %>%  
  mutate(agg_area = st_area(geometry)) %>% 
  group_by(dauco_id) %>% 
  mutate(agg_basin_weight = agg_area/sum(agg_area)) %>% 
  select(dauco_id, agg_basin_id, agg_basin_acreage, agg_area, agg_basin_weight) %>% 
  st_drop_geometry() -> dauco_agg_basin_xw

gw_levels_raw %>% 
  mutate(year = format(msmt_date,"%Y"), 
         month = format(msmt_date, "%m")) %>% 
  select(site_code, year, gse_gwe) %>% 
  filter(!is.na(gse_gwe)) %>% 
  select(site_code, year) %>% 
  unique %>% 
  group_by(site_code) %>% 
  summarize(num_years = n(), 
            min_year = min(year), 
            max_year = max(year)) -> site_counts



#gw_levels_raw %>% 
#  mutate(year = format(msmt_date,"%Y"), 
#         month = format(msmt_date, "%m")) %>% 
#  #filter(year %in% c(2011, 2012, 2013, 2014, 2015))
#  filter(year %in% seq(2002, 2020, by = 1)) %>% 
#  filter(year != 2017) %>% 
#  left_join(site_dauco_xw) %>% 
#  left_join(dauco_basin_xw, by = "dauco_id") %>% 
#  left_join(dauco_agg_basin_xw, by = c("dauco_id")) -> gw_levels 
#  
#rm(gw_levels)
#gw_levels %>% 
#  group_by(dauco_id, year) %>% 
#  summarize(unweighted_depth = mean(gse_gwe, na.rm = TRUE), 
#            basin_weighted_depth = sum(basin_weight*gse_gwe, na.rm = TRUE)/sum(basin_weight, na.rm=TRUE), 
#            agg_basin_weighted_depth = sum(agg_basin_weight*gse_gwe, na.rm = TRUE)/sum(agg_basin_weight, na.rm=TRUE)) %>% 
#  select(dauco_id, year, depth = unweighted_depth) %>% 
#  filter(!is.nan(depth)) %>% 
#  filter(!is.na(dauco_id))  -> fav_dauco_depths

gw_levels_raw %>% 
  mutate(year = format(msmt_date,"%Y"), 
         month = format(msmt_date, "%m")) %>% 
  #filter(year %in% c(2011, 2012, 2013, 2014, 2015))
  filter(year %in% seq(2002, 2020, by = 1)) %>% 
  filter(year != 2017) %>% 
  left_join(site_dauco_xw) %>% 
  group_by(dauco_id, year) %>% 
  summarize(depth = mean(gse_gwe, na.rm = TRUE)) %>% 
  select(dauco_id, year, depth) %>% 
  filter(!is.nan(depth)) %>% 
  filter(!is.na(dauco_id)) %>% 
  group_by(year) %>%
  mutate(percentile_rank = rank(depth, na.last = "keep") / sum(!is.na(depth))) %>% 
  group_by(dauco_id) %>% 
  mutate(avg_rank = mean(percentile_rank)) %>% 
  pivot_wider(id_cols = c("dauco_id", "avg_rank"), 
              names_from = year, 
              values_from = depth) %>% 
  pivot_longer(cols = c(as.character(seq(2002, 2016, by = 1)), as.character(seq(2018, 2020, by = 1))), 
               names_to = "year", 
               values_to = "depth") %>% 
  group_by(dauco_id) %>% 
  mutate(mean_depth = mean(depth, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(quant_depth = quantile(depth, avg_rank, na.rm = TRUE)) %>% 
  rowwise %>% 
  mutate(lambda = (depth - quant_depth)/(mean_depth - quant_depth)) %>% 
  mutate(lambda = ifelse(lambda < 0, 0, lambda)) %>% 
  mutate(lambda = ifelse(lambda > 1, 1, lambda)) %>% 
  group_by(dauco_id) %>% 
  mutate(lambda = mean(lambda, na.rm = TRUE)) %>% 
  mutate(depth = ifelse(is.na(depth), lambda*mean_depth + (1-lambda)*quant_depth, depth)) -> fav_dauco_depths


gw_levels_raw %>% 
  mutate(year = format(msmt_date,"%Y"), 
         month = format(msmt_date, "%m")) %>% 
  #filter(year %in% c(2011, 2012, 2013, 2014, 2015)) %>% 
  filter(year %in% seq(2002, 2020, by = 1)) %>% 
  filter(year != 2017) %>% 
  left_join(site_basin_xw, by = "site_code") %>% 
  full_join(dauco_basin_xw, by = "basin_id") %>% 
  full_join(dauco_agg_basin_xw, by = c("dauco_id", "agg_basin_id", "agg_basin_acreage")) %>% 
  select(-acreage.x) %>% 
  rename(acreage = acreage.y) -> basin_levels

rm(gw_levels_raw)

basin_levels %>% 
  filter(!is.na(basin_id)) %>% 
  group_by(dauco_id, basin_id, year, basin_overlap, basin_weight) %>% 
  summarize(depth = mean(gse_gwe, na.rm = TRUE)) %>% 
  filter(!is.nan(depth))  -> basin_depths

basin_depths %>% 
  group_by(dauco_id, year) %>% 
  summarize(basin_depth = sum(depth*basin_weight)/sum(basin_weight)) -> dauco_basin_depths

basin_levels %>% 
  filter(!is.na(agg_basin_id)) %>% 
  group_by(dauco_id, agg_basin_id, year, agg_basin_weight) %>% 
  summarize(depth = mean(gse_gwe, na.rm = TRUE)) %>% 
  filter(!is.nan(depth))  -> agg_basin_depths

rm(basin_levels)

agg_basin_depths %>% 
  group_by(dauco_id, year) %>% 
  summarize(agg_basin_depth = sum(depth*agg_basin_weight)/sum(agg_basin_weight)) -> dauco_agg_basin_depths

fav_dauco_depths %>% 
  full_join(dauco_basin_depths) %>% 
  full_join(dauco_agg_basin_depths) %>% 
  mutate(depth = ifelse(is.na(depth), basin_depth, depth)) -> incomplete_dauco_depths

incomplete_dauco_depths %>%
  #filter(dauco_id != 25715) %>% 
  filter(dauco_id != 27914) %>% 
  group_by(year) %>%
  mutate(percentile_rank = rank(depth, na.last = "keep") / sum(!is.na(depth))) %>% 
  group_by(dauco_id) %>% 
  mutate(avg_rank = mean(percentile_rank)) %>% 
  pivot_wider(id_cols = c("dauco_id", "avg_rank"), 
              names_from = year, 
              values_from = depth) %>% 
  pivot_longer(cols = c(as.character(seq(2002, 2016, by = 1)), as.character(seq(2018, 2020, by = 1))), 
               names_to = "year", 
               values_to = "depth") %>% 
  group_by(dauco_id) %>% 
  mutate(mean_depth = mean(depth, na.rm = TRUE)) %>% 
  group_by(year) %>% 
  mutate(quant_depth = quantile(depth, avg_rank, na.rm = TRUE)) %>% 
  rowwise %>% 
  mutate(lambda = (depth - quant_depth)/(mean_depth - quant_depth)) %>% 
  mutate(lambda = ifelse(lambda < 0, 0, lambda)) %>% 
  mutate(lambda = ifelse(lambda > 1, 1, lambda)) %>% 
  group_by(dauco_id) %>% 
  mutate(lambda = mean(lambda, na.rm = TRUE)) %>% 
  mutate(depth = ifelse(is.na(depth), lambda*mean_depth + (1-lambda)*quant_depth, depth)) -> clean_dauco_depths

clean_dauco_depths %>% 
  group_by(year) %>% 
  summarize(median = quantile(depth, 0.5),
            p25 = quantile(depth, 0.25),
            p75 = quantile(depth, 0.75),
            p90 = quantile(depth, 0.90))  %>% 
  ungroup %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_longer(cols = c("median", "p25", "p75", "p90"), names_to = "metric", values_to = "depth") %>% 
  ggplot(aes(x = year, y = depth, color = metric)) + 
  geom_point() +
  geom_smooth(method = "loess", span = 0.75)

unique(clean_dauco_depths$dauco_id)

clean_dauco_depths %>% 
  pivot_wider(id_cols = dauco_id, names_from = year, values_from = depth) %>% 
  mutate(depth_change = `2015` - `2011`) -> depth_changes

dauco_shapes %>% 
  left_join(depth_changes) %>% 
  ggplot(aes(fill = depth_change)) + 
  geom_sf(linewidth = .1)  + 
  scale_fill_gradientn(colours=rev(magma(6)), name = "Acreage") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank()) 


clean_dauco_depths %>% 
  select(dauco_id, year, depth) %>% 
  left_join(dauco_gammas) %>% 
  mutate(gamma = ifelse(is.na(gamma), 
                        mean(dauco_gammas$gamma, na.rm = TRUE), 
                        gamma)) -> dauco_depths

write_csv(dauco_depths, here("data/intermediate/dauco_depths.csv"))
