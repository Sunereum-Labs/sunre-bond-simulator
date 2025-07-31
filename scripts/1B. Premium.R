# library
library(tidyverse); library(sf); library(tigris); sf::sf_use_s2(TRUE); library(plotly)

# load data
solar_lookup <- read_csv("../data/solar_farm_lookup.csv")
gsod_attributes <- readRDS("../data/GSOD_attributes.rds")
gsod_peril <- readRDS("../data/lfs/GSOD_peril.rds")
hail_swaths <- readRDS("../data/hail_swaths.rds")
fire_events <- readRDS("../data/fire_events.rds")
solar_noaa_county_mapping <- readRDS("../data/noaa_county_mapping.rds")
hurricane_swaths <- readRDS("../data/hurricane_swaths.rds")

us_counties <- counties() %>% 
  st_transform(crs = 4326)
us_states <- states() %>% 
  st_transform(crs = 4326)

# GSOD 
gsod_attributes <- gsod_attributes %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
  st_join(select(us_counties, NAME), join = st_intersects) %>% 
  rename("STN_NAME" = NAME.x,
         "COUNTY" = NAME.y)

gsod_peril_county <- gsod_peril %>% 
  left_join(
    select(
      tibble(
        gsod_attributes), 
      STNID, STATE, COUNTY), by = c("STNID")) %>% 
  semi_join(distinct(solar_lookup, p_state, p_county), 
            by = c("STATE" = "p_state", "COUNTY" = "p_county")) %>% 
  group_by(STATE, COUNTY) %>% 
  summarise(# GUST triggers
            nGUST = sum(!is.na(GUST)),
            GUST_34 = sum(GUST >= 34, na.rm = TRUE)/nGUST,
            GUST_40 = sum(GUST >= 40, na.rm = TRUE)/nGUST,
            GUST_45 = sum(GUST >= 45, na.rm = TRUE)/nGUST,
            GUST_50 = sum(GUST >= 50, na.rm = TRUE)/nGUST,
            GUST_60 = sum(GUST >= 60, na.rm = TRUE)/nGUST,
            GUST_64 = sum(GUST >= 64, na.rm = TRUE)/nGUST) %>% 
  ungroup()

gsod_peril_state <- gsod_peril_county %>% 
  group_by(STATE) %>% 
  summarise(across(GUST_34:GUST_64, ~sum(nGUST*.)/sum(nGUST))) %>% 
  ungroup()

# hail
hail_swath_counties <- hail_swaths %>% 
  st_transform(crs = 4326) %>% 
  st_join(
    select(us_counties, STATEFP, NAME), 
    join = st_intersects) %>% 
  tibble() %>% 
  select(swath_id, MIN_DATETIME, MAGNITUDE, NAME, STATEFP) %>% 
  # ensure we only have maximum 1 hail event per location per day
  group_by(MIN_DATETIME, NAME, STATEFP) %>% 
  summarise(MAGNITUDE = max(MAGNITUDE)) %>% 
  group_by(NAME, STATEFP) %>% 
  summarise(HAIL_1.0 = sum(MAGNITUDE>=1)/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            HAIL_1.5 = sum(MAGNITUDE>=1.5)/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            HAIL_2.0 = sum(MAGNITUDE>=2)/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            HAIL_2.5 = sum(MAGNITUDE>=2.5)/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day"))) %>% 
  ungroup()

hail_swath_state <- hail_swath_counties %>% 
  group_by(STATEFP) %>% 
  summarise(n = n(),
           across(.cols = HAIL_1.0:HAIL_2.5, ~mean(., na.rm = TRUE))) %>% 
  ungroup()

# fire
fire_counties <- fire_events %>% 
  mutate(STATE = tolower(STATE),
         CZ_NAME = tolower(CZ_NAME)) %>% 
  left_join(solar_noaa_county_mapping, by = c("STATE" = "state", "CZ_NAME" = "matching")) %>% 
  group_by(STATE, county) %>% 
  summarise(FIRE = n()/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day"))) %>% 
  ungroup()

fire_state <- fire_counties %>% 
  group_by(STATE) %>% 
  summarise(n = n(),
            FIRE = mean(FIRE, na.rm = TRUE)) %>% 
  ungroup()

# hurricane
hurricane_swaths[!s2::s2_is_valid(hurricane_swaths),] %>% 
  st_transform(crs = 3857) %>% 
  .[1,] %>% 
  ggplot() +
  geom_sf()

sf::sf_use_s2(FALSE)
hurricane_swath_counties <- hurricane_swaths %>% 
  filter(year(end_datetime) >= 1955) %>% 
  st_transform(crs = 4326) %>% 
  st_join(
    select(us_counties, STATEFP, NAME), 
    join = st_intersects) %>% 
  tibble() %>% 
  select(swath_id, begin_datetime, end_datetime, NAME, STATEFP) %>% 
  group_by(NAME, STATEFP) %>% 
  summarise(HURRICANE = n()/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day"))) %>% 
  ungroup()

hurricane_swath_states <- hurricane_swath_counties %>% 
  group_by(STATEFP) %>% 
  summarise(HURRICANE = mean(HURRICANE, na.rm = TRUE)) %>% 
  ungroup()

# maps
hail_2.0 <- us_counties %>% 
  left_join(hail_swath_counties, by = c("NAME", "STATEFP")) %>% 
  st_transform(crs = 4326) %>% 
  select(HAIL_2.0, NAME, STATEFP) %>% 
  filter(!(STATEFP %in% c("02", "60", "69", "66", "15", "72", "78"))) %>% 
  ggplot() +
  geom_sf(aes(fill = HAIL_2.0*100), size = 0.01) +
  labs(fill = "Freq. of 2ins Hail (%)") +
  theme_minimal()

hurricane_50 <- us_counties %>% 
  left_join(hurricane_swath_counties, by = c("NAME", "STATEFP")) %>% 
  st_transform(crs = 4326) %>% 
  select(HURRICANE, NAME, STATEFP) %>% 
  filter(!(STATEFP %in% c("02", "60", "69", "66", "15", "72", "78"))) %>% 
  ggplot() +
  geom_sf(aes(fill = HURRICANE*100), size = 0.01) +
  labs(fill = "Freq. of 50kt Hurricane (%)") +
  theme_minimal()

fire <- us_counties %>% 
  tibble() %>% 
  left_join(
    tibble(
      select(us_states, NAME, STATEFP)), 
    by = "STATEFP") %>% 
  mutate(STATE = tolower(NAME.y),
         county = tolower(NAME.x)) %>% 
  left_join(fire_counties, by = c("STATE", "county")) %>% 
  st_as_sf(crs = 4326) %>% 
  select(FIRE, county, STATE) %>% 
  filter(!(STATE %in% c("alaska", "american samoa", 
                          "commonwealth of the northern mariana islands", 
                          "guam", "hawaii", "puerto rico", 
                        "united states virgin islands"))) %>% 
  ggplot() +
  geom_sf(aes(fill = FIRE*100), size = 0.01) +
  labs(fill = "Freq. of Wildfire (%)") +
  theme_minimal()

wind_34 <- us_counties %>% 
  tibble() %>% 
  left_join(
    tibble(
      select(us_states, STUSPS, STATEFP)), 
    by = "STATEFP") %>% 
  left_join(gsod_peril_county, by = c("STUSPS"="STATE", "NAME"="COUNTY")) %>% 
  st_as_sf(crs = 4326) %>% 
  select(GUST_34, NAME, STUSPS) %>% 
  filter(!(STUSPS %in% c("AK", "AS", "MP", "GU", "HI", "PR", "VI"))) %>% 
  ggplot() +
  geom_sf(aes(fill = GUST_34*100), size = 0.01) +
  labs(fill = "Freq. of 34kt Winds (%)") +
  theme_minimal()

ggsave(filename = "../premium/hail.jpg", hail_2.0, width = 4200, heigh = 2100, units = "px")
ggsave(filename = "../premium/hurricane.jpg", hurricane_50, width = 4200, heigh = 2100, units = "px")
ggsave(filename = "../premium/wind.jpg", wind_34, width = 4200, heigh = 2100, units = "px")
ggsave(filename = "../premium/fire.jpg", fire, width = 4200, heigh = 2100, units = "px")

# merge on premium components
solar_lookup_premium <- solar_lookup %>% 
  left_join(
    select(
      tibble(us_states), STATEFP, NAME, STUSPS),
    by = c("p_state"="STUSPS")) %>% 
  left_join(hail_swath_counties, by = c("p_county"="NAME", "STATEFP")) %>% 
  left_join(select(hail_swath_state, -n), by = "STATEFP") %>% 
  mutate(HAIL_1.0 = coalesce(HAIL_1.0.x, HAIL_1.0.y),
         HAIL_1.5 = coalesce(HAIL_1.5.x, HAIL_1.5.y),
         HAIL_2.0 = coalesce(HAIL_2.0.x, HAIL_2.0.y),
         HAIL_2.5 = coalesce(HAIL_2.5.x, HAIL_2.5.y)) %>% 
  mutate(join_col1 = tolower(NAME),
         join_col2 = tolower(p_county)) %>% 
  left_join(fire_counties, by = c("join_col1"="STATE", "join_col2"="county")) %>% 
  left_join(fire_state, by = c("join_col1"="STATE")) %>%
  mutate(FIRE = coalesce(FIRE.x, FIRE.y)) %>% 
  left_join(hurricane_swath_counties, by = c("p_county"="NAME", "STATEFP")) %>% 
  left_join(hurricane_swath_states, by = c("STATEFP")) %>% 
  mutate(HURRICANE = coalesce(HURRICANE.x, HURRICANE.y)) %>% 
  left_join(
    select(gsod_peril_county, -nGUST),
    by = c("p_county"="COUNTY", "p_state"="STATE")) %>% 
  left_join(gsod_peril_state, by = c("p_state"="STATE")) %>% 
  mutate(GUST_34 = coalesce(GUST_34.x, GUST_34.y),
         GUST_40 = coalesce(GUST_40.x, GUST_40.y),
         GUST_45 = coalesce(GUST_45.x, GUST_45.y),
         GUST_50 = coalesce(GUST_50.x, GUST_50.y),
         GUST_60 = coalesce(GUST_60.x, GUST_60.y),
         GUST_64 = coalesce(GUST_64.x, GUST_64.y)) %>% 
  select(colnames(solar_lookup), HAIL_1.0:HAIL_2.5, FIRE, HURRICANE, GUST_34:GUST_64) %>% 
  mutate(across(.cols = HAIL_1.0:GUST_64, ~replace_na(., 0)))

# save data
write_csv(solar_lookup_premium, file = "../premium/solar_lookup_with_premium.csv")
  