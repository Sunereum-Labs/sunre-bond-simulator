# load required packages
library(tidyverse)
library(geosphere)


# load data and meta data
gsod_peril <- readRDS("GSOD_peril_1943-2023.rds")
weather_station_lookup <- read_csv("weather_station_lookup.csv")
weather_station_completeness_index <- read_csv("weather_station_completeness.csv")
triggers <- read_csv("contract_triggers.csv")
P <- read_csv("state_premiums.csv")
prices <- readRDS("financial_data.rds")

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

refiant_sim <- function(
    seed = 100,
    iter = 100,
    peril = "MXSPD",
    p_ret = 0.05,
    wy = 1943:2023,
    event = 2,
    CAR = 0.2,
    L1_ratio = 0.7,
    weather_nodes = "",
    states = "",
    n_nodes = 100,
    fy = 2022,
    eth_float = 1,
    alpha = 0.1,
    R_mu = log(0.06),
    R_sigma = sqrt(log(1+0.03^2/0.06^2)),
    S_lambda = 0.002,
    G_alpha = 2,
    G_beta  = 5) {
  
  # seed for reproducibility
  set.seed(seed)
  
  # remove 1986 and 1987 from WYs which are missing from the data
  wy = c(wy[wy < 1986], wy[wy > 1987])
  
  for (m in 1:iter) {
    
    cat("Starting iteration ", m)
    
    # data setup
    source("2. Simulation setup.R", local = TRUE)
    
    # output fields
    output_L2_return <- c()
    output_L1_return <- c()
    
    output_L2_insolvent <- c()
    output_L1_insolvent <- c()
    
    output_claims <- c()
    
    for (j in 1:length(wy)) {
      
      # create simulation file on the current weather year
      j_simulation <- simulation %>% 
        filter(YEAR == wy[j])
      
      # simulation
      L2 = c(n_nodes * CAR * (1 - L1_ratio))
      L2_insolvency = c(0)
      
      L1 <- c(n_nodes * CAR * L1_ratio)
      L1_insolvency = c(0)
      
      for (i in 1:nrow(j_simulation)) {
        
        L2[i+1] = L2[i] * (1 + j_simulation$I[i]) + j_simulation$P[i] * (1 - alpha) - j_simulation$claims[i]
        L1[i+1] = L1[i] * (1 + j_simulation$Y[i]) + j_simulation$P[i] * 
          alpha  + min(L2[i+1], 0) * ((j_simulation$eth_usd[1]/j_simulation$eth_usd[i] - 1) * 
                                        I(eth_float == 1)*1 + 1)
        
        L2_insolvency[i+1] = I(L2[i+1] < 0)*1
        L2[i+1] = (1 - L2_insolvency[i+1]) * L2[i+1] + L2_insolvency[i+1] * 0
        
        L1_insolvency[i+1] = I(L1[i+1] < 0)*1
      }
      
      output_L2_return[j] = L2[length(L2)]/L2[1] - 1
      output_L1_return[j] = L1[length(L1)]/L1[1] - 1
      
      output_L2_insolvent[j] = I(sum(L2_insolvency) > 0)*1
      output_L1_insolvent[j] = I(sum(L1_insolvency) > 0)*1
      
      output_claims[j] = sum(j_simulation$claims)
      
    }
    
    temp <- tibble(year = wy,
                   iter = m,
                   SDI = SDI$SDI,
                   L2_return = output_L2_return,
                   L1_return = output_L1_return,
                   L2_insolvent = output_L2_insolvent,
                   L1_insolvent = output_L1_insolvent,
                   claims = output_claims)
    
    if (m == 1) {
      output <<- temp
    } else {
      output <<- output %>% 
        rbind(temp)
    }
  }
  
  # sample year timeseries
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
  
  # returns
  plt_returns <<- output %>% 
    select(year, iter, L1_return, L2_return) %>% 
    pivot_longer(cols = c(L1_return, L2_return), names_to = "Pool", values_to = "return") %>% 
    group_by(Pool) %>% 
    mutate(return_mean = mean(return)) %>% 
    ungroup() %>% 
    ggplot() +
    theme_minimal() +
    geom_histogram(aes(x = return*100)) +
    labs(x = "Return (%)", y = "Count", title = "Porfolio returns distribution") +
    theme(legend.position = "none") +
    facet_wrap(~Pool, nrow = 2)
  
  plt_returns_y <<- output %>% 
    select(year, iter, L1_return, L2_return) %>% 
    pivot_longer(cols = c(L1_return, L2_return), names_to = "Pool", values_to = "return") %>% 
    ggplot() +
    theme_minimal() +
    geom_boxplot(aes(x = as.factor(year), y = return*100, color = Pool)) +
    labs(x = "Weather Year", y = "Return (%)", title = "Porfolio returns per iteration year") +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~Pool, nrow = 2)
  
  plt_returns_sdi <<- output %>% 
    select(year, iter, L1_return, L2_return, SDI) %>% 
    mutate(total_return = L1_return * L1_ratio + L2_return * (1-L1_ratio),
           standardised_SDI = (SDI-min(SDI))/(max(SDI)-min(SDI))) %>% 
    ggplot() +
    theme_minimal() +
    geom_point(aes(x = standardised_SDI, y = total_return*100)) +
    labs(x = "Portfolio diversification (standardised SDI)", y = "Total protocol return (%)", title = "Protocol return vs Diversification") +
    theme(legend.position = "none")
    
  # insolvency
  plt_insolvency_y <<- output %>% 
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
    labs(x = "Weather Year", y = "Percentage of iterations (%)", title = "Fund insolvency rates per iteration year") +
    theme(legend.position = "bottom")
  
  # claims
  plt_claims <<- output %>% 
    select(year, iter, claims) %>% 
    ggplot() +
    theme_minimal() +
    geom_boxplot(aes(x = as.factor(year), y = claims)) +
    labs(x = "Weather Year", y = "Number of claims", title = "Claim rates per iteration year") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
}
