# ---- Packages & Data ----
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


# ---- Exposure Severity & Premium Multiple ----
# hail
coeffs <- c(
  c = log(0.8),
  inverter_oem_1 = log(1),
  inverter_oem_2 = log(1.07),
  inverter_oem_3 = log(0.95),
  inverter_oem_4 = log(0.9),
  racking_roof = log(2.2),
  racking_fixed = log(2.0),
  racking_single = log(1),
  racking_dual = log(0.8),
  racking_oem_1 = log(1),
  racking_oem_2 = log(0.6),
  racking_oem_3 = log(1.2),
  max_tilt_angle_minus_30 = log(0.97),
  module_oem_1 = log(1),
  module_oem_2 = log(1.03),
  module_oem_3 = log(1.15),
  module_oem_4 = log(1.45),
  acrage = log(1),
  age = log(1.01),
  voltage = log(0.995),
  magnitude = log(1.5) # for deviations away from 1"
)

# sample plant
test <- tibble(var = names(coeffs),
               coeff = coeffs,
               high = c(1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 30-30, 0, 0, 0, 1, 1, 1, 300/10, 2.5-1),
               low = c(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 75-30, 1, 0, 0, 0, 1, 20, 1500/10, 1-1))

test %>% 
  summarise(high = exp(sum(coeff * high)),
            low = exp(sum(coeff * low)))

hail_coeffs <- coeffs

# storm
coeffs <- c(
  c = log(0.1),
  inverter_oem_1 = log(1),
  inverter_oem_2 = log(1.07),
  inverter_oem_3 = log(0.95),
  inverter_oem_4 = log(0.9),
  racking_roof = log(2.2),
  racking_fixed = log(1),
  racking_single = log(1.1),
  racking_dual = log(1.2),
  racking_oem_1 = log(1),
  racking_oem_2 = log(0.9),
  racking_oem_3 = log(1.05),
  max_tilt_angle_minus_30 = log(0.995),
  module_oem_1 = log(1),
  module_oem_2 = log(1.03),
  module_oem_3 = log(1.07),
  module_oem_4 = log(1.15),
  acrage = log(1),
  age = log(1.01),
  voltage = log(0.995),
  magnitude = log(1.02) # above 34kt
)

# sample plant
test <- tibble(var = names(coeffs),
               coeff = coeffs,
               high = c(1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 30-30, 0, 0, 0, 1, 1, 1, 300/10, 0),
               low = c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 75-30, 1, 0, 0, 0, 1, 20, 1500/10, 20))

test %>% 
  summarise(high = exp(sum(coeff * high)),
            low = exp(sum(coeff * low)))

storm_coeffs <- coeffs

# hurricane
coeffs <- c(
  c = log(0.2),
  inverter_oem_1 = log(1),
  inverter_oem_2 = log(1.07),
  inverter_oem_3 = log(0.95),
  inverter_oem_4 = log(0.9),
  racking_roof = log(2.2),
  racking_fixed = log(1),
  racking_single = log(1.1),
  racking_dual = log(1.2),
  racking_oem_1 = log(1),
  racking_oem_2 = log(0.9),
  racking_oem_3 = log(1.05),
  max_tilt_angle_minus_30 = log(0.995),
  module_oem_1 = log(1),
  module_oem_2 = log(1.03),
  module_oem_3 = log(1.07),
  module_oem_4 = log(1.15),
  acrage = log(1),
  age = log(1.01),
  voltage = log(0.995),
  magnitude = log(1)
)

# sample plant
test <- tibble(var = names(coeffs),
               coeff = coeffs,
               high = c(1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 30-30, 0, 0, 0, 1, 1, 1, 300/10, 1),
               low = c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 75-30, 1, 0, 0, 0, 1, 20, 1500/10, 1))

test %>% 
  summarise(high = exp(sum(coeff * high)),
            low = exp(sum(coeff * low)))

hurricane_coeffs <- coeffs

# fire
coeffs <- c(
  c = log(0.7),
  inverter_oem_1 = log(1),
  inverter_oem_2 = log(1.07),
  inverter_oem_3 = log(0.95),
  inverter_oem_4 = log(0.9),
  racking_roof = log(2.2),
  racking_fixed = log(1),
  racking_single = log(1.1),
  racking_dual = log(1.2),
  racking_oem_1 = log(1),
  racking_oem_2 = log(0.9),
  racking_oem_3 = log(1.05),
  max_tilt_angle_minus_30 = log(0.995),
  module_oem_1 = log(1),
  module_oem_2 = log(1.03),
  module_oem_3 = log(1.07),
  module_oem_4 = log(1.15),
  acrage = log(1),
  age = log(1.01),
  voltage = log(0.995),
  magnitude = log(1)
)

# sample plant
test <- tibble(var = names(coeffs),
               coeff = coeffs,
               high = c(1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 30-30, 0, 0, 0, 1, 1, 1, 300/10, 1),
               low = c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 75-30, 1, 0, 0, 0, 1, 20, 1500/10, 1))

test %>% 
  summarise(high = exp(sum(coeff * high)),
            low = exp(sum(coeff * low)))

fire_coeffs <- coeffs

# simulate parameters for solar look-up
solar_lookup <- solar_lookup %>% 
  mutate(tiv = 1000000*1.2 * p_cap_ac,
         inverter_oem = sample(c(1, 2, 3, 4), size = n(), replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)),
         racking_type = sample(c("fixed", "single", "dual"), size = n(), replace = TRUE, prob = c(0.3, 0.5, 0.2)),
         racking_oem = sample(c(1, 2, 3), size = n(), replace = TRUE, prob = c(0.4, 0.55, 0.05)),
         max_title_angle = sample(c(30:75),  size = n(), replace = TRUE),
         module_oem = sample(c(1, 2, 3, 4), size = n(), replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.3)),
         acrage = 5 * p_cap_ac,
         age = 2025 - p_year,
         voltage = case_when(age > 10 ~ 600,
                             age > 5 ~ sample(c(600, 1000), size = n(), replace = TRUE, prob = c(0.4, 0.6))[row_number()],
                             TRUE ~ sample(c(600, 1000, 1500), size = n(), replace = TRUE, prob = c(0.05, 0.4, 0.55))[row_number()]
         )
  )

# calculate expected claim severity multiplier on base premium (% of TIV)
solar_lookup <- solar_lookup %>% 
  mutate(hail_claim_severity_multiple = exp(
      case_when(inverter_oem == 1 ~ hail_coeffs["inverter_oem_1"],
                inverter_oem == 2 ~ hail_coeffs["inverter_oem_2"],
                inverter_oem == 3 ~ hail_coeffs["inverter_oem_3"],
                inverter_oem == 4 ~ hail_coeffs["inverter_oem_4"]) +
      case_when(racking_type == "fixed" ~ hail_coeffs["racking_fixed"],
                racking_type == "single" ~ hail_coeffs["racking_single"],
                racking_type == "dual" ~ hail_coeffs["racking_dual"]) +
      case_when(racking_oem == 1 ~ hail_coeffs["racking_oem_1"],
                racking_oem == 2 ~ hail_coeffs["racking_oem_2"],
                racking_oem == 3 ~ hail_coeffs["racking_oem_3"]) +
      (max_title_angle - 30) * hail_coeffs["max_tilt_angle_minus_30"] +
      case_when(module_oem == 1 ~ hail_coeffs["module_oem_1"],
                module_oem == 2 ~ hail_coeffs["module_oem_2"],
                module_oem == 3 ~ hail_coeffs["module_oem_3"],
                module_oem == 4 ~ hail_coeffs["module_oem_4"]) +
      acrage * hail_coeffs["acrage"] +
      age * hail_coeffs["age"] +
      voltage/10 * hail_coeffs["voltage"]
      ),
      storm_claim_severity_multiple = exp(
        case_when(inverter_oem == 1 ~ storm_coeffs["inverter_oem_1"],
                  inverter_oem == 2 ~ storm_coeffs["inverter_oem_2"],
                  inverter_oem == 3 ~ storm_coeffs["inverter_oem_3"],
                  inverter_oem == 4 ~ storm_coeffs["inverter_oem_4"]) +
          case_when(racking_type == "fixed" ~ storm_coeffs["racking_fixed"],
                    racking_type == "single" ~ storm_coeffs["racking_single"],
                    racking_type == "dual" ~ storm_coeffs["racking_dual"]) +
          case_when(racking_oem == 1 ~ storm_coeffs["racking_oem_1"],
                    racking_oem == 2 ~ storm_coeffs["racking_oem_2"],
                    racking_oem == 3 ~ storm_coeffs["racking_oem_3"]) +
          (max_title_angle - 30) * storm_coeffs["max_tilt_angle_minus_30"] +
          case_when(module_oem == 1 ~ storm_coeffs["module_oem_1"],
                    module_oem == 2 ~ storm_coeffs["module_oem_2"],
                    module_oem == 3 ~ storm_coeffs["module_oem_3"],
                    module_oem == 4 ~ storm_coeffs["module_oem_4"]) +
          acrage * storm_coeffs["acrage"] +
          age * storm_coeffs["age"] +
          voltage/10 * storm_coeffs["voltage"]
      ),
      hurricane_claim_severity_multiple = exp(
        case_when(inverter_oem == 1 ~ hurricane_coeffs["inverter_oem_1"],
                  inverter_oem == 2 ~ hurricane_coeffs["inverter_oem_2"],
                  inverter_oem == 3 ~ hurricane_coeffs["inverter_oem_3"],
                  inverter_oem == 4 ~ hurricane_coeffs["inverter_oem_4"]) +
          case_when(racking_type == "fixed" ~ hurricane_coeffs["racking_fixed"],
                    racking_type == "single" ~ hurricane_coeffs["racking_single"],
                    racking_type == "dual" ~ hurricane_coeffs["racking_dual"]) +
          case_when(racking_oem == 1 ~ hurricane_coeffs["racking_oem_1"],
                    racking_oem == 2 ~ hurricane_coeffs["racking_oem_2"],
                    racking_oem == 3 ~ hurricane_coeffs["racking_oem_3"]) +
          (max_title_angle - 30) * hurricane_coeffs["max_tilt_angle_minus_30"] +
          case_when(module_oem == 1 ~ hurricane_coeffs["module_oem_1"],
                    module_oem == 2 ~ hurricane_coeffs["module_oem_2"],
                    module_oem == 3 ~ hurricane_coeffs["module_oem_3"],
                    module_oem == 4 ~ hurricane_coeffs["module_oem_4"],) +
          acrage * hurricane_coeffs["acrage"] +
          age * hurricane_coeffs["age"] +
          voltage/10 * hurricane_coeffs["voltage"]
      ),
      fire_claim_severity_multiple = exp(
        case_when(inverter_oem == 1 ~ fire_coeffs["inverter_oem_1"],
                  inverter_oem == 2 ~ fire_coeffs["inverter_oem_2"],
                  inverter_oem == 3 ~ fire_coeffs["inverter_oem_3"],
                  inverter_oem == 4 ~ fire_coeffs["inverter_oem_4"]) +
          case_when(racking_type == "fixed" ~ fire_coeffs["racking_fixed"],
                    racking_type == "single" ~ fire_coeffs["racking_single"],
                    racking_type == "dual" ~ fire_coeffs["racking_dual"]) +
          case_when(racking_oem == 1 ~ fire_coeffs["racking_oem_1"],
                    racking_oem == 2 ~ fire_coeffs["racking_oem_2"],
                    racking_oem == 3 ~ fire_coeffs["racking_oem_3"]) +
          (max_title_angle - 30) * fire_coeffs["max_tilt_angle_minus_30"] +
          case_when(module_oem == 1 ~ fire_coeffs["module_oem_1"],
                    module_oem == 2 ~ fire_coeffs["module_oem_2"],
                    module_oem == 3 ~ fire_coeffs["module_oem_3"],
                    module_oem == 4 ~ fire_coeffs["module_oem_4"]) +
          acrage * fire_coeffs["acrage"] +
          age * fire_coeffs["age"] +
          voltage/10 * fire_coeffs["voltage"]
      )
  )


# ---- Event Frequency & Base Premium ----

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
            GUST_64 = sum(GUST >= 64, na.rm = TRUE)/nGUST,
            storm_base_premium = exp(storm_coeffs["c"]) * 
              sum(exp(storm_coeffs["magnitude"] * (GUST-34)) * I(GUST >= 34), na.rm = TRUE)/sum(GUST >= 34, na.rm = TRUE)) %>% 
  ungroup()

gsod_peril_state <- gsod_peril_county %>% 
  group_by(STATE) %>% 
  summarise(across(GUST_34:GUST_64, ~sum(nGUST*.)/sum(nGUST)),
            storm_base_premium = mean(storm_base_premium, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(storm_base_premium = replace_na(storm_base_premium, 0))

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
  summarise(MAGNITUDE = max(MAGNITUDE, na.rm = TRUE)) %>% 
  group_by(NAME, STATEFP) %>% 
  summarise(HAIL_1.0 = sum(MAGNITUDE>=1)/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            HAIL_1.5 = sum(MAGNITUDE>=1.5)/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            HAIL_2.0 = sum(MAGNITUDE>=2)/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            HAIL_2.5 = sum(MAGNITUDE>=2.5)/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            hail_base_premium = exp(hail_coeffs["c"]) * 
              sum(exp(hail_coeffs["magnitude"] * (MAGNITUDE-1.5)) * I(MAGNITUDE >= 1), na.rm = TRUE)
            /sum(MAGNITUDE >= 1, na.rm = TRUE)) %>% 
  ungroup()

hail_swath_state <- hail_swath_counties %>% 
  group_by(STATEFP) %>% 
  summarise(n = n(),
           across(.cols = HAIL_1.0:hail_base_premium, ~mean(., na.rm = TRUE))) %>% 
  ungroup()

# fire
fire_counties <- fire_events %>% 
  mutate(STATE = tolower(STATE),
         CZ_NAME = tolower(CZ_NAME)) %>% 
  left_join(solar_noaa_county_mapping, by = c("STATE" = "state", "CZ_NAME" = "matching")) %>% 
  group_by(STATE, county) %>% 
  summarise(FIRE = n()/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            fire_base_premium = exp(fire_coeffs["c"] + fire_coeffs["magnitude"])) %>% 
  ungroup()

fire_state <- fire_counties %>% 
  group_by(STATE) %>% 
  summarise(n = n(),
            FIRE = mean(FIRE, na.rm = TRUE),
            fire_base_premium = mean(fire_base_premium)) %>% 
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
  summarise(HURRICANE = n()/length(seq(ymd("1955-01-01"), ymd("2024-12-12"), by = "1 day")),
            hurricane_base_premium = exp(hurricane_coeffs["c"] + hurricane_coeffs["magnitude"])) %>% 
  ungroup()

hurricane_swath_states <- hurricane_swath_counties %>% 
  group_by(STATEFP) %>% 
  summarise(HURRICANE = mean(HURRICANE, na.rm = TRUE),
            hurricane_base_premium = mean(hurricane_base_premium, na.rm = TRUE)) %>% 
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


# ---- Merge and Visualisation ----
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
         HAIL_2.5 = coalesce(HAIL_2.5.x, HAIL_2.5.y),
         hail_expected_claim = hail_base_premium.x * hail_claim_severity_multiple,
         hail_expected_claim = case_when(is.na(hail_expected_claim) | hail_expected_claim == 0 ~
                                           hail_base_premium.y * hail_claim_severity_multiple,
                                         TRUE ~ hail_expected_claim),
         hail_technical_premium = hail_expected_claim * HAIL_1.0.x,
         hail_technical_premium = case_when(is.na(hail_technical_premium) | hail_technical_premium == 0 ~
                                           hail_expected_claim * HAIL_1.0.y,
                                       TRUE ~ hail_technical_premium)) %>% 
  mutate(join_col1 = tolower(NAME),
         join_col2 = tolower(p_county)) %>% 
  left_join(fire_counties, by = c("join_col1"="STATE", "join_col2"="county")) %>% 
  left_join(fire_state, by = c("join_col1"="STATE")) %>%
  mutate(FIRE = coalesce(FIRE.x, FIRE.y),
         fire_base_premium = fire_base_premium.x * FIRE.x,
         fire_expected_claim = fire_base_premium.x * fire_claim_severity_multiple,
         fire_expected_claim = case_when(is.na(fire_expected_claim) | fire_expected_claim == 0 ~
                                           fire_base_premium.y * fire_claim_severity_multiple,
                                         TRUE ~ fire_expected_claim),
         fire_technical_premium = fire_expected_claim * FIRE.x,
         fire_technical_premium = case_when(is.na(fire_technical_premium) | fire_technical_premium == 0 ~
                                              fire_expected_claim * FIRE.y,
                                            TRUE ~ fire_technical_premium)) %>% 
  left_join(hurricane_swath_counties, by = c("p_county"="NAME", "STATEFP")) %>% 
  left_join(hurricane_swath_states, by = c("STATEFP")) %>% 
  mutate(HURRICANE = coalesce(HURRICANE.x, HURRICANE.y),
         hurricane_expected_claim = hurricane_base_premium.x * hurricane_claim_severity_multiple,
         hurricane_expected_claim = case_when(is.na(hurricane_expected_claim) | hurricane_expected_claim == 0 ~
                                           hurricane_base_premium.y * hurricane_claim_severity_multiple,
                                         TRUE ~ hurricane_expected_claim),
         hurricane_technical_premium = hurricane_expected_claim * HURRICANE.x,
         hurricane_technical_premium = case_when(is.na(hurricane_technical_premium) | hurricane_technical_premium == 0 ~
                                              hurricane_expected_claim * HURRICANE.y,
                                            TRUE ~ hurricane_technical_premium)) %>% 
  left_join(
    select(gsod_peril_county, -nGUST),
    by = c("p_county"="COUNTY", "p_state"="STATE")) %>% 
  left_join(gsod_peril_state, by = c("p_state"="STATE")) %>% 
  mutate(GUST_34 = coalesce(GUST_34.x, GUST_34.y),
         GUST_40 = coalesce(GUST_40.x, GUST_40.y),
         GUST_45 = coalesce(GUST_45.x, GUST_45.y),
         GUST_50 = coalesce(GUST_50.x, GUST_50.y),
         GUST_60 = coalesce(GUST_60.x, GUST_60.y),
         GUST_64 = coalesce(GUST_64.x, GUST_64.y),
         storm_expected_claim = storm_base_premium.x * storm_claim_severity_multiple,
         storm_expected_claim = case_when(is.na(storm_expected_claim) | storm_expected_claim == 0 ~
                                           storm_base_premium.y * storm_claim_severity_multiple,
                                         TRUE ~ storm_expected_claim),
         storm_technical_premium = storm_expected_claim * GUST_34.x,
         storm_technical_premium = case_when(is.na(storm_technical_premium) | storm_technical_premium == 0 ~
                                              storm_expected_claim * GUST_34.y,
                                            TRUE ~ storm_technical_premium)) %>% 
  select(colnames(solar_lookup), HAIL_1.0:HAIL_2.5, FIRE, HURRICANE, GUST_34:GUST_64,
         hail_expected_claim, fire_expected_claim, hurricane_expected_claim, storm_expected_claim,
         hail_technical_premium, fire_technical_premium, hurricane_technical_premium, storm_technical_premium) %>% 
  mutate(across(.cols = HAIL_1.0:storm_technical_premium, ~replace_na(., 0)))


# claim severity
solar_lookup_premium %>% 
  select(hail_expected_claim:storm_expected_claim) %>% 
  pivot_longer(cols = hail_expected_claim:storm_expected_claim, values_to = "perc_of_tiv", names_to = "peril") %>% 
  mutate(perc_of_tiv = perc_of_tiv * 100) %>% 
  ggplot() +
  geom_histogram(aes(x = perc_of_tiv)) +
  facet_wrap(~peril, nrow = 2, scales = "free") +
  xlim(0, 100) +
  theme_minimal()

# technical premium
solar_lookup_premium %>% 
  select(hail_technical_premium:storm_technical_premium) %>% 
  pivot_longer(cols = hail_technical_premium:storm_technical_premium, values_to = "perc_of_tiv", names_to = "peril") %>% 
  mutate(perc_of_tiv = perc_of_tiv * 100) %>% 
  ggplot() +
  geom_histogram(aes(x = perc_of_tiv)) +
  facet_wrap(~peril, nrow = 2, scales = "free") +
  theme_minimal()

solar_lookup_premium %>% 
  select(hail_technical_premium:storm_technical_premium) %>% 
  mutate(technical_premium = hail_technical_premium + fire_technical_premium + storm_technical_premium + hurricane_technical_premium,
         technical_premium = technical_premium * 100) %>% 
  ggplot() +
  geom_histogram(aes(x = technical_premium), bins = 60) +
  theme_minimal()


# ---- Commercial Premium Rates ----
# commercial rates
commercial_rates <- c(
  # Southwest
  "AZ" = 0.4, "NM" = 0.4, "NV" = 0.4, "UT" = 0.4,
  
  # California (separate due to unique rain pattern)
  "CA" = 0.45,
  
  # Pacific Northwest
  "WA" = 0.35, "OR" = 0.35,
  
  # Midwest
  "IL" = 0.4, "IN" = 0.4, "IA" = 0.4, "OH" = 0.4,
  "MI" = 0.4, "WI" = 0.4, "MN" = 0.4, "MO" = 0.4,
  "ND" = 0.4, "SD" = 0.4, "NE" = 0.4, "KS" = 0.4,
  
  # Northeast
  "NY" = 0.5, "PA" = 0.5, "MA" = 0.5, "NJ" = 0.5,
  "CT" = 0.5, "RI" = 0.5, "VT" = 0.5, "NH" = 0.5,
  "ME" = 0.5,
  
  # Gulf Coast / Southeast (humid subtropical)
  "FL" = 0.4, "LA" = 0.4, "MS" = 0.4,
  "AL" = 0.4, "GA" = 0.4, "SC" = 0.4,
  "NC" = 0.4, "TX" = 0.55,
  
  # Appalachians / Interior Southeast
  "TN" = 0.35, "KY" = 0.35, "WV" = 0.35,
  "VA" = 0.35, "AR" = 0.35,
  
  # Hawaii
  "HI" = 0.4,
  
  # Alaska
  "AK" = 0.3,
  
  # Mountain/Intermountain West (drier)
  "CO" = 0.35, "MT" = 0.35, "ID" = 0.35, "WY" = 0.35,
  
  # Plains crossover
  "OK" = 0.4,  # could also go "Plains"
  
  # Mid-Atlantic
  "DE" = 0.4, "MD" = 0.4, "DC" = 0.4  # if using DC
  
  # Not included: territories like PR, GU, VI
)

solar_lookup_premium <- solar_lookup_premium %>% 
  mutate(commercial_premium = commercial_rates[p_state])

# save data
write_csv(solar_lookup_premium, file = "../premium/solar_lookup_with_premium.csv")
write_csv(solar_lookup_premium, file = "../risk_engine/input/solar_lookup_with_premium.csv")
saveRDS(list(hail_coeffs, 
               storm_coeffs,
               hurricane_coeffs,
               fire_coeffs), file = "../premium/severity_coefficients.rds")
  