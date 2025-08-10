# load required packages
library(tidyverse)
library(geosphere)
library(sf)
library(plotly)
library(jsonlite)
library(tigris)
library(rnaturalearth)


# load data and meta data
solar_lookup <- read_csv("./premium/solar_lookup_with_premium.csv")
severity_coefficients <- readRDS("./premium/severity_coefficients.rds")

solar_gsod_lookup <- read_csv("./data/lfs/solar_stnid_lookup.csv")
gsod_peril <- readRDS("./data/lfs/GSOD_peril.rds")

hail_swaths <- readRDS("./data/hail_swaths.rds")
fire_events <- readRDS("./data/fire_events.rds")
solar_noaa_county_mapping <- readRDS("./data/noaa_county_mapping.rds")

hurricane_swaths <- readRDS("./data/hurricane_swaths.rds")

risk_engine <- function(
    seed = 100,
    iter = 1000,
    wy = 1955:2023,
    deductible = 0.15,
    target_loss_ratio = 0.4,
    portfolio_sampling = 1,
    states = c("NY", "AZ", "CA", "TX", "FL", "NC", "SC", "OH", "CO", "GA", "OR", "NV", "UT",
               "MN", "GA", "VA", "PA", "NJ", "MD", "CN", "MA"),
    n_assets = 500,
    sf_ac_min = 0,
    sf_ac_max = 10) {
  
  set.seed(seed)
  
  if (portfolio_sampling == 1) {
    solar_lookup_concat <- solar_lookup %>% 
      filter(p_state %in% states & p_cap_ac <= sf_ac_max & p_cap_ac >= sf_ac_min)
    
    if (n_assets > nrow(solar_lookup)) {
      return("Not enough assets to sample that meet filter criteria")
    }
    
    solar_lookup_concat <- solar_lookup_concat %>% 
      .[sample(1:nrow(.), size = n_assets, replace = FALSE),]
    
    write_csv(solar_lookup_concat, "./risk_engine/input/input.csv")
  } else {
    solar_lookup_concat <- read_csv("./risk_engine/input/input.csv")
  }
  
  # ---- 
  #Dashboard 1: portfolio characteristics
  
  # total tiv
  p <- solar_lookup_concat %>% 
    summarise(portfolio = "",
              tiv = sum(tiv)/1e6) %>% 
    ggplot() +
    geom_bar(aes(x = portfolio, y = tiv), stat = "identity") +
    geom_hline(yintercept = 1.2 * sum(solar_lookup_concat$tiv)/1e6, linetype = 2) +
    theme_minimal() +
    xlab("Aggregate Total Insured Value") +
    ylab("USD'millions")
    
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash1_p1.json", auto_unbox = TRUE, pretty = FALSE)
  
  # state
  p <- solar_lookup_concat %>% 
    group_by(p_state) %>% 
    summarise(tiv = sum(tiv)/1e3) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = p_state, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.2 * sum(solar_lookup_concat$tiv)/1e3, linetype = 2) +
    theme_minimal() +
    xlab("State") +
    ylab("USD'thousands")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash1_p2.json", auto_unbox = TRUE, pretty = FALSE)
  
  # county
  p <- solar_lookup_concat %>% 
    group_by(p_county) %>% 
    summarise(tiv = sum(tiv)/1e3) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = p_county, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.05 * sum(solar_lookup_concat$tiv)/1e3, linetype = 2) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 90,          # degrees
        hjust = 1,           # horizontal justification
        vjust = 1            # vertical justification
      )
    ) +
    xlab("County") +
    ylab("USD'thousands")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash1_p3.json", auto_unbox = TRUE, pretty = FALSE)
  
  # inverter oem
  p <- solar_lookup_concat %>% 
    group_by(inverter_oem) %>% 
    summarise(tiv = sum(tiv)/1e6) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = inverter_oem, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.75 * sum(solar_lookup_concat$tiv)/1e6, linetype = 2) +
    theme_minimal() +
    xlab("Inverter OEM") +
    ylab("USD'millions")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash1_p4.json", auto_unbox = TRUE, pretty = FALSE)
  
  # racking oem
  p <- solar_lookup_concat %>% 
    group_by(racking_oem) %>% 
    summarise(tiv = sum(tiv)/1e6) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = racking_oem, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.75 * sum(solar_lookup_concat$tiv)/1e6, linetype = 2) +
    theme_minimal() +
    xlab("Racking OEM") +
    ylab("USD'millions")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash1_p5.json", auto_unbox = TRUE, pretty = FALSE)
  
  # racking type
  p <- solar_lookup_concat %>% 
    group_by(racking_type) %>% 
    summarise(tiv = sum(tiv)/1e6) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = racking_type, y = tiv), stat = "identity") +
    theme_minimal() +
    xlab("Racking Type") +
    ylab("USD'millions")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash1_p6.json", auto_unbox = TRUE, pretty = FALSE)
  
  # module oem
  p <- solar_lookup_concat %>% 
    group_by(module_oem) %>% 
    summarise(tiv = sum(tiv)/1e6) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = module_oem, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.75 * sum(solar_lookup_concat$tiv)/1e6, linetype = 2) +
    theme_minimal() +
    xlab("Module OEM") +
    ylab("USD'millions")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash1_p7.json", auto_unbox = TRUE, pretty = FALSE)
  
  # age, max tilt angle and voltage
  p <- solar_lookup_concat %>% 
    select(age, max_title_angle, voltage) %>%
    rename("Age" = age, "Maximum Tilt Angle" = max_title_angle, "Voltage" = voltage) %>% 
    pivot_longer(cols = Age:Voltage, names_to = "variable", values_to = "value") %>% 
    ggplot() +
    geom_boxplot(aes(x = variable, y = value, group = variable)) +
    facet_wrap(~variable, scales = "free", nrow = 1) +
    theme_minimal() +
    xlab("") +
    ylab("")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash1_p8.json", auto_unbox = TRUE, pretty = FALSE)
  
  # ----
  # create portfolio and simulate the first iteration of claims
  source("./scripts/3A. Risk engine setup.R")
  
  # ----
  # Dashboard 2: Loss
  us_states <- states() %>% 
    st_transform(crs = 4326)
  
  premium <- simulation %>% # missing 5 sites with no claims
    mutate(total_loss = total_insured_claim * tiv,
           hail_loss = hail_insured_claim * tiv,
           storm_loss = storm_insured_claim * tiv,
           hurricane_loss = hurricane_insured_claim * tiv,
           fire_loss = fire_insured_claim * tiv) %>% 
    group_by(case_id) %>% 
    summarise(across(total_loss:fire_loss, ~sum(.))) %>% 
    ungroup() %>% 
    mutate(simulation_premium = total_loss/iter/length(wy),
           across(hail_loss:fire_loss, ~./iter/length(wy))) %>% 
    left_join(select(solar_lookup, case_id, p_state, p_county, xlong, ylat, 
                     tiv, commercial_premium,
                     racking_type, racking_oem, module_oem, voltage, max_title_angle,
                     HAIL_1.0), 
              by = "case_id") %>% 
    mutate(commercial_premium = commercial_premium * tiv/100,
           hail_rate = hail_loss/tiv*100,
           storm_rate = storm_loss/tiv*100,
           hurricane_rate = hurricane_loss/tiv*100,
           fire_rate = fire_loss/tiv*100) %>% 
    select(-total_loss)
  
  # loss ratio by state
  p <- us_states %>% 
    left_join(
      summarise(
        group_by(premium, p_state),
        loss_ratio = sum(simulation_premium)/sum(commercial_premium)),
      by = c("STUSPS"="p_state")) %>% 
    filter(!(STUSPS %in% c("AK", "AS", "MP", "GU", "HI", "PR", "VI"))) %>% 
    ggplot() +
    geom_sf(aes(fill = loss_ratio), color = "white") +
    scale_fill_gradient2(low = "green",
                         high = "red",
                         na.value = "grey",
                         midpoint = target_loss_ratio) +
    
    theme_minimal()
  
  # premium loss by state
  p <- premium %>% 
    group_by(p_state) %>% 
    summarise(across(hail_loss:fire_loss, ~sum(.))) %>% 
    ungroup() %>% 
    pivot_longer(cols = hail_loss:fire_loss, names_to = "peril", values_to = "average_annual_loss") %>% 
    ggplot() +
    geom_bar(aes(x = p_state, y = average_annual_loss, fill = peril), stat = "identity", position = "stack") +
    theme_minimal()
  
  # loss rate by state
  p <- premium %>% 
    group_by(p_state) %>% 
    summarise(across(hail_loss:fire_loss, ~sum(.)),
              tiv = sum(tiv)) %>% 
    ungroup() %>% 
    mutate(across(hail_loss:fire_loss, ~./tiv*100)) %>% 
    pivot_longer(cols = hail_loss:fire_loss, names_to = "peril", values_to = "loss_rate") %>% 
    ggplot() +
    geom_bar(aes(x = p_state, y = loss_rate, fill = peril), stat = "identity", position = "stack") +
    theme_minimal()
  
  # distribution of hail rate
  p <- premium %>% ggplot + geom_histogram(aes(x = hail_rate), bins = 60) + theme_minimal()
  
  
  # ----
  # Dashboard 2: Sites
  us_map <- ne_states(country = "united states of america", returnclass = "sf") %>% 
    st_transform(4326)
  
  p <- ggplot() +
    geom_sf(data = us_map, fill = "gray", color = "white") +
    geom_point(data = mutate(premium, loss_rate = (simulation_premium+0.0001)/commercial_premium),
               aes(x = xlong, y = ylat, 
                   text = paste0(
                     "<b>Case ID:</b> ", case_id, "<br>",
                     "<b>State:</b> ", p_state, "<br>",
                     "<b>County:</b> ", p_county, "<br>",
                     "<b>tiv:</b> ", tiv, "<br>",
                     "<b>Hail rate:</b> ", hail_rate, "<br>",
                     "<b>Storm rate:</b> ", storm_rate, "<br>",
                     "<b>Hurricane rate:</b> ", hurricane_rate, "<br>",
                     "<b>Racking type:</b> ", racking_type, "<br>",
                     "<b>Racking OEM:</b> ", racking_oem, "<br>",
                     "<b>Max tilt:</b> ", max_title_angle, "<br>",
                     "<b>Module OEM:</b> ", module_oem, "<br>",
                     "<b>Voltage:</b> ", voltage, "<br>",
                     "<b>Hail Freq:</b>", HAIL_1.0
                   ),
               color = loss_rate), 
               size = 0.3) +
    scale_color_gradient2(low = "blue",
                         mid = "black",
                         high = "red",
                         midpoint = 1) +
    coord_fixed(1.3) +
    theme_minimal() +
    xlim(-125, -66) +
    ylim(25, 50)
    
  p <- premium %>% 
    group_by(p_state, racking_type) %>% 
    summarise(loss_rate = sum(simulation_premium)/sum(commercial_premium)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = p_state, y = loss_rate, fill = racking_type), stat = "identity", position = "dodge") +
    theme_minimal()
    
  
  
  
  
  
  
}