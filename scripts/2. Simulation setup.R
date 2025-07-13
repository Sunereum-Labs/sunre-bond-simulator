# generate missing fields
if (states[1] == "") {
    states = solar_lookup %>% 
      filter(p_state != "") %>% 
      .$p_state %>% 
      unique()
}

# randomly select n solar farms
sim_sf <- solar_lookup %>% 
  filter(p_state %in% states &
           p_cap_ac >= sf_ac_min & p_cap_ac <= sf_ac_max) %>% 
  .$case_id %>% 
  sample(size = n_assets, replace = FALSE)

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
  summarise(GUST = sum(GUST >= 34 & GUST < 50)) %>% 
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

fire <- solar_lookup %>% 
  filter(case_id %in% sim_sf) %>% 
  select(case_id, p_state, p_county) %>% 
  mutate(noaa_state = tolower(state_code_to_name(p_state)),
         noaa_county = tolower(p_county)) %>% 
  left_join(solar_noaa_county_mapping, by = c("noaa_state"="state", "noaa_county"="county")) %>% 
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
  left_join(sim_fire_events, by = c("noaa_state"="STATE", "matching"="CZ_NAME")) %>% 
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
                              fire == 1 & lag(fire) == 1 ~ 0)) %>% 
  ungroup() %>% 
  arrange(YEARMODA, case_id) %>% 
  mutate(YEAR = year(YEARMODA)) %>% 
  select(-fire) %>% 
  rename("fire" = fire_new)
  
# hail
hail <- hail_swaths %>% 
  # trigger on hail size 1.75ins
  filter(MAGNITUDE >= 1.75) %>% 
  st_transform(crs = 3857) %>% 
  mutate(geometry = st_buffer(geometry, dist = 10000)) # add 10km buffer to hail swaths

solar_sf <- solar_lookup %>% 
  filter(case_id %in% sim_sf) %>% 
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
  summarise(hail = pmin(1, n())) %>% 
  ungroup()

sim_noaa <- sim_fire %>% 
  left_join(sim_hail, by = c("case_id", "YEARMODA")) %>% 
  mutate(hail = replace_na(hail, 0))

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
  summarise(hurricane = pmin(1, n())) %>% 
  ungroup()

# create simulation file
sim_claims <- sim_noaa %>% 
  left_join(sim_hurricane, by = c("case_id", "YEARMODA")) %>% 
  left_join(sim_gsod, by = c("case_id", "YEARMODA", "YEAR")) %>% 
  # impute NAs for missing hurricane values
  mutate(hurricane = replace_na(hurricane, 0),
         GUST = replace_na(GUST, 0))

simulation <- sim_claims %>% 
  group_by(YEARMODA, YEAR) %>% 
  summarise(across(.cols = c("GUST", "hail", "fire", "hurricane"), sum),
            claims = sum(GUST, hail, fire, hurricane)) %>% 
  ungroup()

# merge on financial data
wy_index <- setNames(rep(1:length(fy), ceiling(length(wy)/length(fy)))[1:length(wy)], wy)
fy_index <- setNames(1:length(fy), fy)

# prices and historical returns
sim_prices <- prices %>%
  filter(year(date) %in% fy) %>% 
  mutate(doy = yday(date),
         year_index = fy_index[as.character(year(date))])

# premiums = expected value: sum all claims pay uniformly everyday
sim_premium <- simulation %>% 
  select(-YEAR, -YEARMODA) %>% 
  unlist() %>% 
  sum()/nrow(simulation)
  
simulation <- simulation %>% 
  mutate(doy = yday(YEARMODA),
         year_index = wy_index[as.character(year(YEARMODA))]) %>% 
  left_join(select(sim_prices, -date), by = c("doy", "year_index")) %>% 
  # fill leap years
  fill(sp500_ret, eth_usd, rf_ret, .direction = "down") %>%
  # generate staking yield and financial returns portfolio
  mutate(R = rlnorm(nrow(.), meanlog = R_mu, sdlog = R_sigma),
         S = rpois(nrow(.), S_lambda),
         G = rbeta(nrow(.), G_alpha, G_beta),
         Y = R/365 - S * G,
         eta = runif(nrow(.), 0, 1),
         I = eta * rf_ret + (1 - eta) * sp500_ret) %>% 
  mutate(P = sim_premium)

# Portfolio distance/concentration metric
SDI <- solar_lookup %>% 
  filter(case_id %in% sim_sf) %>%
  summarise(SDI = compute_DI_SDI(lat = ylat, lon = xlong))
