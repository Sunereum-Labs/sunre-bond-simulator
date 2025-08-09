# load required packages
library(tidyverse)
library(geosphere)
library(sf)


# load data and meta data
solar_lookup <- read_csv("../premium/solar_lookup_with_premium.csv")
severity_coefficients <- readRDS("../premium/severity_coefficients.rds")

solar_gsod_lookup <- read_csv("../data/lfs/solar_stnid_lookup.csv")
gsod_peril <- readRDS("../data/lfs/GSOD_peril.rds")

hail_swaths <- readRDS("../data/hail_swaths.rds")
fire_events <- readRDS("../data/fire_events.rds")
solar_noaa_county_mapping <- readRDS("../data/noaa_county_mapping.rds")

hurricane_swaths <- readRDS("../data/hurricane_swaths.rds")

risk_engine <- function(
    seed = 100,
    iter = 3000,
    wy = 1955:2023,
    deductible = 0.05,
    target_loss_ratio = 0.4,
    portfolio_sampling = 1,
    states = c("NY", "AZ", "CA", "TX", "FL", "NC", "SC", "OH", "CO", "GA", "OR"),
    n_assets = 300,
    sf_ac_min = 0,
    sf_ac_max = 20) {
  
  set.seed(seed)
  
  if (portfolio_sampling == 1) {
    solar_lookup_concat <- solar_lookup %>% 
      filter(p_state %in% states & p_cap_ac <= sf_ac_max & p_cap_ac >= sf_ac_min)
    
    if (n_assets > nrow(solar_lookup)) {
      return("Not enough assets to sample that meet filter criteria")
    }
    
    solar_lookup_concat <- solar_lookup_concat %>% 
      .[sample(1:nrow(.), size = n_assets, replace = FALSE),]
  } else {
    solar_lookup_concat <- read_csv("./risk_engine/input/input.csv")
  }
  
  # ---- 
  #pull portfolio attributes
  p_tiv <- solar_lookup_concat %>% 
    summarise(portfolio = "total tiv",
              tiv = sum(tiv)) %>% 
    ggplot() +
    geom_bar(aes(x = portfolio, y = tiv), stat = "identity") +
    geom_hline(yintercept = 1.2 * sum(solar_lookup_concat$tiv), linetype = 2) +
    theme_minimal()
  
  p_state <- solar_lookup_concat %>% 
    group_by(p_state) %>% 
    summarise(tiv = sum(tiv)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = p_state, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.2 * sum(solar_lookup_concat$tiv), linetype = 2) +
    theme_minimal()
  
  p_state <- solar_lookup_concat %>% 
    group_by(p_state) %>% 
    summarise(tiv = sum(tiv)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = p_state, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.2 * sum(solar_lookup_concat$tiv), linetype = 2) +
    theme_minimal()
  
  p_county <- solar_lookup_concat %>% 
    group_by(p_county) %>% 
    summarise(tiv = sum(tiv)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = p_county, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.05 * sum(solar_lookup_concat$tiv), linetype = 2) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 90,          # degrees
        hjust = 1,           # horizontal justification
        vjust = 1            # vertical justification
      )
    )
  
  p_inverter_oem <- solar_lookup_concat %>% 
    group_by(inverter_oem) %>% 
    summarise(tiv = sum(tiv)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = inverter_oem, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.75 * sum(solar_lookup_concat$tiv), linetype = 2) +
    theme_minimal()
  
  p_racking_oem <- solar_lookup_concat %>% 
    group_by(racking_oem) %>% 
    summarise(tiv = sum(tiv)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = racking_oem, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.75 * sum(solar_lookup_concat$tiv), linetype = 2) +
    theme_minimal()
  
  p_module_oem <- solar_lookup_concat %>% 
    group_by(module_oem) %>% 
    summarise(tiv = sum(tiv)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = module_oem, y = tiv), stat = "identity") +
    geom_hline(yintercept = 0.75 * sum(solar_lookup_concat$tiv), linetype = 2) +
    theme_minimal()
  
  # ----
  # create portfolio and simulate the first iteration of claims
  source("./scripts/3A. Risk engine setup.R")
  
  
  
  
  
}