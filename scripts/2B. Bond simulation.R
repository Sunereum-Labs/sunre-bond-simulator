# load required packages
library(tidyverse)
library(geosphere)
library(sf)


# load data and meta data
prices <- readRDS("./data/financial_data.rds")
solar_lookup <- read_csv("./premium/solar_lookup_with_premium.csv")

solar_gsod_lookup <- read_csv("./data/lfs/solar_stnid_lookup.csv")
gsod_peril <- readRDS("./data/lfs/GSOD_peril.rds")

hail_swaths <- readRDS("./data/hail_swaths.rds")
fire_events <- readRDS("./data/fire_events.rds")
solar_noaa_county_mapping <- readRDS("./data/noaa_county_mapping.rds")

hurricane_swaths <- readRDS("./data/hurricane_swaths.rds")

# portfolio spread function S, DI, SDI
compute_DI_SDI <- function(lat, lon, metric = "SDI") {
  if (length(lat) != length(lon)) stop("Latitude and longitude vectors must be the same length.")
  n <- length(lat)
  if (n < 2) stop("At least two coordinates are needed.")
  
  #  coordinate matrix
  coords <- cbind(lon, lat)
  
  # Haversine, Euclidean distance for all pairwise distances in km
  dist_matrix <- distm(coords, fun = distHaversine)/1000
  
  # extract lower triangle (i < j) of distance matrix
  dist_vals <- dist_matrix[lower.tri(dist_matrix)]
  
  # compute DI
  sum_dists = sum(dist_vals)
  sum_sq_dists = sum(dist_vals^2)
  DI = (sum_dists^2) / sum_sq_dists
  
  # compute average pairwise distance
  S = mean(dist_vals)
  
  # SDI
  SDI = DI * S
  
  return(SDI)
}

sunrefi_sim <- function(
    seed = 100,
    iter = 20,
    wy = 1955:2023,
    CAR = 0.5,
    loss_ratio = 0.5,
    L1_ratio = 0.5,
    states = c("NY", "AZ"),
    n_assets = 20,
    sf_ac_min = 0,
    sf_ac_max = 20,
    fy = 2023,
    eth_float = 1,
    R_mu = log(0.06),
    R_sigma = sqrt(log(1+0.03^2/0.06^2)),
    S_lambda = 0.002,
    G_alpha = 2,
    G_beta  = 5) {
  
  # seed for reproducibility
  set.seed(seed)
  
  for (m in 1:iter) {
    
    cat("Starting iteration ", m)
    
    # data setup
    source("./scripts/2A. Bond simulation setup.R", local = TRUE)
    
    # output fields
    output_L2_return <- c()
    output_L1_return <- c()
    
    output_L2_insolvent <- c()
    output_L1_insolvent <- c()
    
    output_premium <- c()
    output_claims <- c()
    output_wind <- c()
    output_hail <- c()
    output_hurricane <- c()
    output_fire <- c()
    
    # calculate alpha {premium share} endogenously
    alpha = L1_ratio
    
    for (j in 1:length(wy)) {
      
      # create simulation file on the current weather year
      j_simulation <- simulation %>% 
        filter(YEAR == wy[j])
      
      # simulation
      L2 = c(n_bonds * CAR * (1 - L1_ratio))
      L2_insolvency = c(0)
      
      L1 <- c(n_bonds * CAR * L1_ratio)
      L1_insolvency = c(0)
      
      for (i in 1:nrow(j_simulation)) {
        
        L2[i+1] = L2[i] * (1 + j_simulation$I[i]) + j_simulation$P[i] * (1 - alpha) - j_simulation$claims[i]
        L1[i+1] = L1[i] * (1 + j_simulation$Y[i]) + 
          j_simulation$P[i] * alpha * (I(eth_float == 1)*j_simulation$eth_usd[1] / j_simulation$eth_usd[i] + I(eth_float == 0)*1)  + 
          min(L2[i+1], 0) * ((j_simulation$eth_usd[1]/j_simulation$eth_usd[i] - 1) * 
                                        I(eth_float == 1)*1 + 1)
        
        L2_insolvency[i+1] = I(L2[i+1] < 0)*1
        L2[i+1] = (1 - L2_insolvency[i+1]) * L2[i+1] + L2_insolvency[i+1] * 0
        
        L1_insolvency[i+1] = I(L1[i+1] < 0)*1
      }
      
      output_L2_return[j] = 
        if (L1_ratio < 1 & CAR > 0) {
          L2[length(L2)]/(L2[1]) - 1
        } else {
            0
        }
      output_L1_return[j] = 
        if (L1_ratio > 0 & CAR > 0) {
          L1[length(L1)]/(L1[1]) - 1
        } else {
          0
        }
      
      output_L2_insolvent[j] = 
        if (L1_ratio < 1 & CAR > 0) {
          I(sum(L2_insolvency) > 0)*1
        } else {
          0
        }
      output_L1_insolvent[j] = 
        if (L1_ratio > 0 & CAR > 0) {
          I(sum(L1_insolvency) > 0)*1
        } else {
          0
        }
      
      output_premium[j] = sum(j_simulation$P)
      output_claims[j] = sum(j_simulation$claims)
      output_wind[j] = sum(j_simulation$GUST)
      output_fire[j] = sum(j_simulation$fire)
      output_hail[j] = sum(j_simulation$hail)
      output_hurricane[j] = sum(j_simulation$hurricane)
      
      # sample year timeseries
      if (sum(j_simulation$claims) >= 1) {
        plt_example <<- tibble(Year = wy[j],
                               L1 = L1,
                               L2 = L2,
                               claims = c(0, j_simulation$claims)) %>% 
          mutate(index = row_number(),
                 claims = ifelse(claims > 0, claims, NA)) %>% 
          pivot_longer(cols = c(L1, L2), names_to = "Pool", values_to = "value") %>% 
          ggplot() +
          theme_minimal() +
          geom_line(aes(x = index, y = value, color = Pool)) +
          geom_point(aes(x = index, y = claims)) +
          labs(x = "Day of year", y = "$", title = paste0("Iteration ", m, ", Weather Year ", wy[j])) +
          theme(legend.position = "bottom")
      }
    }
    
    temp <- tibble(year = wy,
                   iter = m,
                   SDI = SDI$SDI,
                   L2_return = output_L2_return,
                   L1_return = output_L1_return,
                   L2_insolvent = output_L2_insolvent,
                   L1_insolvent = output_L1_insolvent,
                   yearly_premium = output_premium,
                   claims = output_claims,
                   wind = output_wind,
                   fire = output_fire,
                   hail = output_hail,
                   hurricane = output_hurricane)
    
    if (m == 1) {
      output <<- temp
    } else {
      output <<- output %>% 
        rbind(temp)
    }
  }
  
  # returns distribution
  plt_returns <<- output %>% 
    select(year, iter, L1_return, L2_return) %>% 
    pivot_longer(cols = c(L1_return, L2_return), names_to = "Pool", values_to = "return") %>% 
    group_by(Pool) %>% 
    mutate(return_mean = mean(return)) %>% 
    ungroup() %>% 
    ggplot() +
    theme_minimal() +
    geom_histogram(aes(x = return*100)) +
    labs(x = "Mean Yearly Return (%)", y = "Count", title = "Porfolio returns distribution") +
    theme(legend.position = "none") +
    facet_wrap(~Pool, nrow = 2, scales = "free")
  
  # returns x year
  plt_returns_year <<- output %>% 
    select(year, iter, L1_return, L2_return) %>% 
    pivot_longer(cols = c(L1_return, L2_return), names_to = "Pool", values_to = "return") %>% 
    ggplot() +
    theme_minimal() +
    geom_boxplot(aes(x = as.factor(year), y = return*100, color = Pool)) +
    labs(x = "Weather Year", y = "Return (%)", title = "Porfolio returns per iteration year") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~Pool, nrow = 2)  + 
    ylim(-100, 100)
    
  # solvency distribution
  plt_insolvency <<- output %>% 
    select(year, iter, L1_insolvent, L2_insolvent) %>% 
    pivot_longer(cols = c(L1_insolvent, L2_insolvent), names_to = "Pool", values_to = "insolvency") %>% 
    group_by(iter, Pool) %>% 
    summarise(solvency = 1 - sum(insolvency)/n()) %>% 
    ungroup() %>% 
    ggplot() +
    theme_minimal() +
    geom_histogram(aes(x = solvency*100, fill = Pool), position = "dodge") +
    labs(x = "Percentage of solvent years (%)", y = "Number of iterations", 
         title = "Distribution of solvency") +
    theme(legend.position = "bottom")
  
  # insolvency x year
  plt_insolvency_year <<- output %>% 
    select(year, iter, L1_insolvent, L2_insolvent) %>% 
    pivot_longer(cols = c(L1_insolvent, L2_insolvent), names_to = "Pool", values_to = "insolvency") %>% 
    group_by(year, Pool) %>% 
    summarise(insolvency = sum(insolvency)/n()) %>% 
    group_by(Pool) %>% 
    mutate(insolvency_mean = mean(insolvency)) %>% 
    ungroup() %>% 
    ggplot() +
    theme_minimal() +
    geom_bar(aes(x = year, y = insolvency*100, fill = Pool), stat = "identity", position = "dodge") +
    geom_abline(aes(slope = 0, intercept = insolvency_mean*100, color = Pool)) +
    labs(x = "Weather Year", y = "Percentage of iterations (%)", title = "Insolvency rates per iteration year") +
    theme(legend.position = "bottom")
  
  # insolvency x sdi
  plt_insolvency_sdi <<- output %>%
    select(year, SDI, L1_insolvent, L2_insolvent) %>% 
    pivot_longer(cols = c(L1_insolvent, L2_insolvent), names_to = "Pool", values_to = "insolvency") %>% 
    group_by(SDI, Pool) %>% 
    summarise(solvency = 1 - sum(insolvency)/n()) %>% 
    ungroup() %>% 
    ggplot() +
    theme_minimal() +
    geom_point(aes(x = SDI, y = solvency*100)) +
    labs(x = "Portfolio diversification (standardised SDI)", y = "Percentage of solvent years (%)", 
         title = "Diversification vs solvency") +
    theme(legend.position = "none")
  
  # claims distribution
  plt_claims <<- output %>% 
    select(claims) %>% 
    arrange(desc(claims)) %>% 
    mutate(idx = row_number()) %>% 
    ggplot() +
    geom_bar(aes(x = idx, y = claims), stat = "identity") +
    theme_minimal() +
    labs(x = "Iteration year", y = "Number of claims",
         title = "Distribution of claims")
    
  # claims x year
  plt_claims_year <<- output %>% 
    select(year, iter, claims) %>% 
    ggplot() +
    theme_minimal() +
    geom_boxplot(aes(x = as.factor(year), y = claims)) +
    labs(x = "Weather Year", y = "Number of claims", title = "Claim rates per iteration year") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # peril x year
  plt_peril_year <<- output %>% 
    select(year, iter, wind:hurricane) %>% 
    pivot_longer(cols = wind:hurricane, names_to = "peril", values_to = "claims") %>% 
    ggplot() +
    theme_minimal() +
    geom_boxplot(aes(x = as.factor(year), y = claims, group = as.factor(year))) +
    facet_wrap(~peril, scales = "free") +
    labs(x = "Weather Year", y = "Number of claims", title = "Peril claims per iteration year") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}
