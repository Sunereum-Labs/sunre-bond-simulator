sim_sf <- solar_lookup_concat$case_id

hail_coefficients <- severity_coefficients[[1]]
storm_coefficients <- severity_coefficients[[2]]
hurricane_coefficients <- severity_coefficients[[3]]
fire_coefficients <- severity_coefficients[[4]]

# GUST
gust <- solar_gsod_lookup %>% 
  filter(case_id %in% sim_sf &
           PERIL == "GUST") %>% 
  filter(YEAR >= min(wy) & YEAR <= max(wy)) %>% 
  # tie-breaker for multiple COMPLETE stations located the same distance away
  distinct(case_id, distance, YEAR, PERIL, .keep_all = TRUE)

# GSOD claims {only has GUST atm}
gsod_events <- gsod_peril %>% 
  mutate(YEAR = year(YEARMODA)) %>% 
  semi_join(gust, by = c("STNID", "YEAR")) %>% 
  # 34kt-50kt GUST trigger
  group_by(STNID, YEARMODA, YEAR) %>% 
  summarise(GUST = sum(GUST >= 34),
            GUST_MAGNITUDE = max(GUST)) %>% 
  ungroup()

time_index = seq(from = ymd(paste0(min(wy), "-01-01")), 
                 to = ymd(paste0(max(wy), "-12-31")),
                 b = "1 day")
sim_gsod <- tibble(case_id = rep(sim_sf, each = length(time_index)),
                   YEARMODA = rep(time_index, times = length(sim_sf)),
                   YEAR = year(YEARMODA)) %>% 
  left_join(gust, by = c("case_id", "YEAR")) %>% 
  left_join(gsod_events, by = c("STNID", "YEARMODA", "YEAR")) %>% 
  select(-STNID, -distance, -PERIL)

# wildfire
state_code_to_name <- function(code) {
  match_idx <- match(toupper(code), state.abb)
  state.name[match_idx]
}

fire <- solar_lookup_concat %>% 
  select(case_id, p_state, p_county) %>% 
  mutate(noaa_state = tolower(state_code_to_name(p_state)),
         noaa_county = tolower(p_county)) %>% 
  left_join(solar_noaa_county_mapping, by = c("noaa_state"="state", "noaa_county"="county"),
            relationship = "many-to-many") %>% 
  select(case_id, noaa_state, matching) %>% 
  filter(!is.na(matching))

# summarise fire events by day
sim_fire_events <- fire_events %>%
  mutate(YEARMODA = ymd(substr(BEGIN_DATE_TIME_PARSED, 1, 10)),
         STATE = tolower(STATE),
         CZ_NAME = tolower(CZ_NAME)) %>% 
  filter(YEAR %in% wy) %>% 
  semi_join(fire, by = c("STATE"="noaa_state", "CZ_NAME"="matching")) %>% 
  group_by(YEARMODA, STATE, CZ_NAME) %>% 
  summarise(fire = pmin(1, n())) %>% 
  ungroup()

# re-summarise daily fire events by case_id
sim_fire_events_join <- fire %>% 
  left_join(sim_fire_events, by = c("noaa_state"="STATE", "matching"="CZ_NAME"),
            relationship = "many-to-many") %>% 
  group_by(case_id, YEARMODA) %>% 
  summarise(fire = pmin(1, n())) %>% 
  ungroup()

# fire claims by case_id
sim_fire <- tibble(case_id = rep(sim_sf, each = length(time_index)),
                   YEARMODA = rep(time_index, times = length(sim_sf))) %>% 
  left_join(sim_fire_events_join, by = c("case_id", "YEARMODA")) %>% 
  mutate(fire = replace_na(fire, 0),) %>% 
  # ensure that only 1 claim is made per fire, defined as consecutive fire days
  group_by(case_id) %>% 
  mutate(fire_new = case_when(fire == 0 ~ 0,
                              fire == 1 & lag(fire) != 1 ~ 1,
                              fire == 1 & lag(fire) == 1 ~ 0),
         FIRE_MAGNITUDE = 1) %>% 
  ungroup() %>% 
  arrange(YEARMODA, case_id) %>% 
  mutate(YEAR = year(YEARMODA)) %>% 
  select(-fire) %>% 
  rename("fire" = fire_new)

# hail
hail <- hail_swaths %>% 
  # trigger on hail size 2ins
  filter(MAGNITUDE >= 1) %>% 
  st_transform(crs = 3857) %>% 
  mutate(geometry = st_buffer(geometry, dist = 10000)) # add 10km buffer to hail swaths

solar_sf <- solar_lookup_concat %>% 
  select(case_id, xlong, ylat) %>% 
  st_as_sf(coords = c("xlong", "ylat"), crs = 4326) %>% 
  st_transform(crs = 3857)

hail_events <- st_intersects(solar_sf, hail, sparse = TRUE)
sim_hail <- tibble(); colnames(sim_hail) <- c("case_id", colnames(hail))
for (i in 1:nrow(solar_sf)) {
  temp <- hail[hail_events[[i]],]
  temp$case_id = solar_sf$case_id[i]
  sim_hail <- sim_hail %>% 
    rbind(temp)
}
sim_hail <- sim_hail %>% 
  mutate(YEARMODA = ymd(substr(MAX_DATETIME, 1, 10))) %>% 
  as_tibble() %>% 
  group_by(case_id, YEARMODA) %>% 
  summarise(hail = pmin(1, n()),
            HAIL_MAGNITUDE = max(MAGNITUDE)) %>% 
  ungroup()

sim_noaa <- sim_fire %>% 
  left_join(sim_hail, by = c("case_id", "YEARMODA")) %>% 
  mutate(hail = replace_na(hail, 0),
         HAIL_MAGNITUDE = replace_na(HAIL_MAGNITUDE, 0))

# hurricane
hurricane <- hurricane_swaths %>% 
  filter(year(end_datetime) >= min(wy) & year(end_datetime) <= max(wy)) %>% 
  st_transform(crs = 3857)

hurricane_events <- st_intersects(solar_sf, hurricane, sparse = TRUE)
sim_hurricane <- tibble(); colnames(sim_hurricane) <- c("case_id", colnames(hurricane))
for (i in 1:nrow(solar_sf)) {
  temp <- hurricane[hurricane_events[[i]],]
  temp$case_id = solar_sf$case_id[i]
  sim_hurricane <- sim_hurricane %>% 
    rbind(temp)
}

sim_hurricane <- sim_hurricane %>% 
  mutate(YEARMODA = ymd(substr(end_datetime, 1, 10))) %>% 
  as_tibble() %>% 
  group_by(case_id, YEARMODA) %>% 
  summarise(hurricane = pmin(1, n()),
            HURRICANE_MAGNITUDE = 1) %>% 
  ungroup()

# create simulation file
sim_claims <- sim_noaa %>% 
  left_join(sim_hurricane, by = c("case_id", "YEARMODA")) %>% 
  left_join(sim_gsod, by = c("case_id", "YEARMODA", "YEAR")) %>% 
  # impute NAs for missing hurricane values
  mutate(hurricane = replace_na(hurricane, 0),
         HURRICANE_MAGNITUDE = replace_na(HURRICANE_MAGNITUDE, 0),
         GUST = replace_na(GUST, 0),
         GUST_MAGNITUDE = replace_na(GUST_MAGNITUDE, 0))

# merge on claim severity data
simulation <- sim_claims %>% 
  left_join(
    select(solar_lookup_concat, case_id, tiv, hail_claim_severity_multiple:fire_claim_severity_multiple),
    by = "case_id"
  ) %>% 
  # get magnitude and constant from 1B.Premium.R
  mutate(hail_claim_severity = hail_claim_severity_multiple * exp(hail_coefficients["c"] +
           pmax(HAIL_MAGNITUDE-1, 0) * hail_coefficients["magnitude"]),
         storm_claim_severity = storm_claim_severity_multiple * exp(storm_coefficients["c"] +
           pmax(GUST_MAGNITUDE-34, 0) * storm_coefficients["magnitude"]),
         hurricane_claim_severity = hurricane_claim_severity_multiple * exp(hurricane_coefficients["c"] +
           HURRICANE_MAGNITUDE * hurricane_coefficients["magnitude"]),
         fire_claim_severity = fire_claim_severity_multiple * exp(fire_coefficients["c"] +
           FIRE_MAGNITUDE * fire_coefficients["magnitude"]),
         # draw claims from a gamma distribution, leaving scale fixed at 0.0625
         gamma_scale = 0.0625,
         hail_claim = rgamma(n = n(), shape = hail_claim_severity/gamma_scale, scale = gamma_scale),
         storm_claim = rgamma(n = n(), shape = storm_claim_severity/gamma_scale, scale = gamma_scale),
         hurricane_claim = rgamma(n = n(), shape = hurricane_claim_severity/gamma_scale, scale = gamma_scale),
         fire_claim = rgamma(n = n(), shape = fire_claim_severity/gamma_scale, scale = gamma_scale),
         hail_claim = pmin(hail_claim * hail, 1),
         storm_claim = pmin(storm_claim * GUST, 1),
         hurricane_claim = pmin(hurricane_claim * hurricane, 1),
         fire_claim = pmin(fire_claim * fire, 1),
         total_insured_claim = pmax(pmin(hail_claim + storm_claim + hurricane_claim + fire_claim, 1) - deductible, 0),
         hail_insured_claim = case_when(total_insured_claim > 0 & hail > 0 ~ 
                                          hail_claim/(hail_claim + storm_claim + hurricane_claim + fire_claim) * total_insured_claim,
                                        TRUE ~ 0),
         storm_insured_claim = case_when(total_insured_claim > 0 & GUST > 0 ~ 
                                          storm_claim/(hail_claim + storm_claim + hurricane_claim + fire_claim) * total_insured_claim,
                                        TRUE ~ 0),
         hurricane_insured_claim = case_when(total_insured_claim > 0 & hurricane > 0 ~ 
                                          hurricane_claim/(hail_claim + storm_claim + hurricane_claim + fire_claim) * total_insured_claim,
                                        TRUE ~ 0),
         fire_insured_claim = case_when(total_insured_claim > 0 & fire > 0 ~ 
                                          fire_claim/(hail_claim + storm_claim + hurricane_claim + fire_claim) * total_insured_claim,
                                        TRUE ~ 0))

# check claim means by plant
#simulation %>% 
#  select(case_id, hail_claim:fire_claim) %>% 
#  group_by(case_id) %>% 
#  summarise(across(hail_claim:fire_claim, ~mean(.))) %>% 
#  ungroup() %>% 
#  pivot_longer(cols = hail_claim:fire_claim, names_to = "peril", values_to = "claim") %>% 
#  ggplot() +
#  geom_boxplot(aes(x = peril, y = claim, group = peril)) +
#  theme_minimal()


  