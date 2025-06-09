# generate missing fields
if (weather_nodes[1] == "") {
  if (states[1] == "") {
    states = weather_station_lookup %>% 
      filter(STATE != "") %>% 
      .$STATE %>% 
      unique()
  }
  m_weather_nodes = weather_station_completeness_index %>% 
    filter(STATE %in% states & YEAR %in% wy & !!sym(peril) == "COMPLETE") %>% 
    select(STNID, YEAR) %>% 
    group_by(YEAR) %>% 
    slice_sample(n = n_nodes, replace = TRUE) %>% 
    ungroup()
  
} else {
  #m_weather_nodes = tibble(STNID = weather_nodes, YEAR = wy)
  #n_nodes = length
}

# event type
trigger_index = if_else(event == 2, 2, 3)

# truncate GSOD data for simulation
simulation <- gsod_peril %>% 
  mutate(YEAR = year(YEARMODA)) %>% 
  # filter on selected weather-station years by a semi-join
  semi_join(m_weather_nodes, by = c("STNID", "YEAR")) %>%
  select(STNID, YEARMODA, YEAR, !!sym(peril)) %>% 
  # bootstrap NA values
  group_by(STNID) %>% 
  mutate(!!sym(peril) := if_else(is.na(!!sym(peril)),
                                 sample(!is.na(!!sym(peril)), 1, replace = TRUE),
                                 !!sym(peril))) %>% 
  ungroup() %>% 
  # solve claims
  mutate(claim = if_else(!!sym(peril) >= 
                           filter(triggers, var == !!peril)[1, trigger_index][[1]], 1, 0)) %>% 
  group_by(YEARMODA, YEAR) %>% 
  summarise(claims = sum(claim)) %>% 
  ungroup() %>% 
  arrange(YEARMODA)

rm(trigger_index)


# merge on financial data
wy_index <- setNames(rep(1:length(fy), ceiling(length(wy)/length(fy)))[1:length(wy)], wy)
fy_index <- setNames(1:length(fy), fy)

# prices and historical returns
sim_prices <- prices %>%
  filter(year(date) %in% fy) %>% 
  mutate(doy = yday(date),
         year_index = fy_index[as.character(year(date))])

# premiums
sim_premium <- weather_station_completeness_index %>%
  semi_join(m_weather_nodes, by = c("STNID", "YEAR")) %>% 
  left_join(
    select(P, STATE, !!sym(paste0(peril, "_", event))),
           by = "STATE") %>%
  mutate(P = pmax(!!sym(paste0(peril, "_", event)), 1/(365*event)) * (1+p_ret)) %>% 
  group_by(YEAR) %>% 
  summarise(P = sum(P)) %>% 
  ungroup() %>% 
  select(YEAR, P)
  
simulation <- simulation %>% 
  mutate(doy = yday(YEARMODA),
         year_index = wy_index[as.character(year(YEARMODA))]) %>% 
  left_join(select(sim_prices, -date), by = c("doy", "year_index")) %>% 
  fill(sp500_ret, eth_usd, rf_ret, .direction = "down") %>%
  # generate staking yield and financial returns portfolio
  mutate(R = rlnorm(nrow(.), meanlog = R_mu, sdlog = R_sigma),
         S = rpois(nrow(.), S_lambda),
         G = rbeta(nrow(.), G_alpha, G_beta),
         Y = R/365 - S * G,
         eta = runif(nrow(.), 0, 1),
         I = eta * rf_ret + (1 - eta) * sp500_ret) %>% 
  left_join(sim_premium, by = "YEAR") %>% 
  mutate(P = P)

# Portfolio distance/concentration metric
SDI <- m_weather_nodes %>% 
  left_join(weather_station_lookup, by = "STNID") %>% 
  group_by(YEAR) %>% 
  summarise(SDI = compute_DI_SDI(lat = LATITUDE, lon = LONGITUDE))
