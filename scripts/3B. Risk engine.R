# load required packages
library(tidyverse)
library(geosphere)
library(sf)
library(plotly)
library(jsonlite)
library(tigris)
library(rnaturalearth)

# colours
sun_blue = "#2563EB"
sun_green = "#16A34A"
sun_purple = "#9333EA"
sun_red = "#E53935"
sun_orange = "#FB8C00"

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
    geom_bar(aes(x = portfolio, y = tiv), stat = "identity", fill = sun_red) +
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
    geom_bar(aes(x = p_state, y = tiv), stat = "identity", fill = sun_red) +
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
    geom_bar(aes(x = p_county, y = tiv), stat = "identity", fill = sun_red) +
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
    geom_bar(aes(x = inverter_oem, y = tiv), stat = "identity", fill = sun_red) +
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
    geom_bar(aes(x = racking_oem, y = tiv), stat = "identity", fill = sun_red) +
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
    geom_bar(aes(x = racking_type, y = tiv), stat = "identity", fill = sun_red) +
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
    geom_bar(aes(x = module_oem, y = tiv), stat = "identity", fill = sun_red) +
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
    geom_boxplot(aes(x = variable, y = value, group = variable), color = sun_red) +
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
    left_join(select(solar_lookup, p_name, case_id, p_year, p_state, p_county, xlong, ylat, p_cap_ac, 
                     tiv, commercial_premium,
                     inverter_oem, racking_type, racking_oem, module_oem, max_title_angle, voltage, acrage), 
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
        `Average Loss Ratio` = sum(simulation_premium)/sum(commercial_premium)),
      by = c("STUSPS"="p_state")) %>% 
    filter(!(STUSPS %in% c("AK", "AS", "MP", "GU", "HI", "PR", "VI"))) %>%
    ggplot() +
    geom_sf(aes(fill = `Average Loss Ratio`), color = "white") +
    scale_fill_gradient2(low = sun_green,
                         high = sun_red,
                         na.value = "grey",
                         midpoint = target_loss_ratio) +
    
    theme_minimal()
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p1.json", auto_unbox = TRUE, pretty = FALSE)
  
  # premium loss by state
  p <- premium %>% 
    group_by(p_state) %>% 
    summarise(across(hail_loss:fire_loss, ~sum(.)/1e3)) %>% 
    rename("Hail" = hail_loss, "Storm" = storm_loss, "Hurricane" = hurricane_loss, "Fire" = fire_loss) %>% 
    ungroup() %>% 
    pivot_longer(cols = Hail:Fire, names_to = "Peril", values_to = "average_annual_loss") %>% 
    ggplot() +
    geom_bar(aes(x = p_state, y = average_annual_loss, fill = Peril), stat = "identity", position = "stack") +
    theme_minimal() +
    scale_fill_manual(values = c("Hail" = sun_blue, "Storm" = sun_purple, "Hurricane" = sun_green, "Fire" = sun_orange)) +
    xlab("State") +
    ylab("USD'thousands") +
    ggtitle("Average Annual Portfolio Losses")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p2.json", auto_unbox = TRUE, pretty = FALSE)
  
  # loss rate by state
  p <- premium %>% 
    group_by(p_state) %>% 
    summarise(across(hail_loss:fire_loss, ~sum(.)),
              tiv = sum(tiv)) %>% 
    ungroup() %>% 
    mutate(across(hail_loss:fire_loss, ~./tiv*100)) %>% 
    rename("Hail" = hail_loss, "Storm" = storm_loss, "Hurricane" = hurricane_loss, "Fire" = fire_loss) %>% 
    pivot_longer(cols = Hail:Fire, names_to = "Peril", values_to = "loss_rate") %>% 
    ggplot() +
    geom_bar(aes(x = p_state, y = loss_rate, fill = Peril), stat = "identity", position = "stack") +
    theme_minimal() +
    scale_fill_manual(values = c("Hail" = sun_blue, "Storm" = sun_purple, "Hurricane" = sun_green, "Fire" = sun_orange)) +
    xlab("State") +
    ylab("% of TIV") +
    ggtitle("Average Technical Premium")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p3.json", auto_unbox = TRUE, pretty = FALSE)
  
  # distrution of annual portfolio loss
  loss <- simulation %>% 
    group_by(YEAR, iter) %>% 
    summarise(total_loss = sum(total_insured_claim * tiv)/1e6) %>% 
    ungroup()
  
  p <- loss %>% 
    ggplot() +
    geom_histogram(aes(x = total_loss), fill = sun_red) +
    geom_vline(xintercept = sum(premium$commercial_premium)/1e6/target_loss_ratio, linetype = 2) +
    annotate("text", x = sum(premium$commercial_premium)/1e6/target_loss_ratio, y = 7500,
             label = "Target loss ratio", angle = 270, vjust = -0.5) +
    geom_vline(xintercept = quantile(loss$total_loss, 0.9), linetype = 3) +
    annotate("text", x = quantile(loss$total_loss, 0.9), y = 7500,
             label = "1-in-10 year loss", angle = 270, vjust = -0.5) +
    geom_vline(xintercept = quantile(loss$total_loss, 0.98), linetype = 3) +
    annotate("text", x = quantile(loss$total_loss, 0.98), y = 7500,
             label = "1-in-50 year loss", angle = 270, vjust = -0.5) +
    geom_vline(xintercept = quantile(loss$total_loss, 0.99), linetype = 3) +
    annotate("text", x = quantile(loss$total_loss, 0.99), y = 7500,
             label = "1-in-100 year loss", angle = 270, vjust = -0.5) +
    geom_vline(xintercept = quantile(loss$total_loss, 0.998), linetype = 3) +
    annotate("text", x = quantile(loss$total_loss, 0.998), y = 7500,
             label = "1-in-500 year loss", angle = 270, vjust = -0.5) +
    theme_minimal() +
    ylab("Count of Simulation Years") +
    xlab("USD'millions") +
    ggtitle("Annual Portfolio Loss")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p4.json", auto_unbox = TRUE, pretty = FALSE)
  
  # distribution of hail rate
  p <- premium %>% 
    ggplot + 
    geom_histogram(aes(x = hail_rate), bins = 60, fill = sun_blue) + 
    theme_minimal() +
    xlab("Technical Hail Premium") +
    ylab("Count") +
    ggtitle("Asset Distribution of Technical Hail Premium")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p5.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    ggplot + 
    geom_histogram(aes(x = storm_rate), bins = 60, fill = sun_purple) + 
    theme_minimal() +
    xlab("Technical Hail Premium") +
    ylab("Count") +
    ggtitle("Asset Distribution of Technical Hail Premium")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p6.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    ggplot + 
    geom_histogram(aes(x = hurricane_rate), bins = 60, fill = sun_green) + 
    theme_minimal() +
    xlab("Technical Hail Premium") +
    ylab("Count") +
    ggtitle("Asset Distribution of Technical Hail Premium")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p6.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    ggplot + 
    geom_histogram(aes(x = fire_rate), bins = 60, fill = sun_orange) + 
    theme_minimal() +
    xlab("Technical Hail Premium") +
    ylab("Count") +
    ggtitle("Asset Distribution of Technical Hail Premium")
  
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p7.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    group_by(racking_type) %>% 
    summarise(loss_ratio = sum(simulation_premium)/sum(commercial_premium)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = racking_type, y = loss_ratio), stat = "identity", position = "dodge", fill = sun_red) +
    theme_minimal() +
    xlab("Racking Type") +
    ylab("Average Loss Ratio")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p8.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    group_by(racking_oem) %>% 
    summarise(loss_ratio = sum(simulation_premium)/sum(commercial_premium)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = racking_oem, y = loss_ratio), stat = "identity", position = "dodge", fill = sun_red) +
    theme_minimal() +
    xlab("Racking OEM") +
    ylab("Average Loss Ratio")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p9.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    group_by(inverter_oem) %>% 
    summarise(loss_ratio = sum(simulation_premium)/sum(commercial_premium)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = inverter_oem, y = loss_ratio), stat = "identity", position = "dodge", fill = sun_red) +
    theme_minimal() +
    xlab("Inverter OEM") +
    ylab("Average Loss Ratio")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p10.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    group_by(module_oem) %>% 
    summarise(loss_ratio = sum(simulation_premium)/sum(commercial_premium)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_bar(aes(x = module_oem, y = loss_ratio), stat = "identity", position = "dodge", fill = sun_red) +
    theme_minimal() +
    xlab("Module OEM") +
    ylab("Average Loss Ratio")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p11.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    mutate(loss_ratio = simulation_premium/commercial_premium) %>% 
    ggplot() +
    geom_jitter(aes(x = max_title_angle, y = loss_ratio), color = sun_red) +
    theme_minimal() +
    xlab("Max Tilt Angle") +
    ylab("Asset Loss Ratio")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p12.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    mutate(loss_ratio = simulation_premium/commercial_premium,
           age = 2025 - p_year) %>% 
    ggplot() +
    geom_jitter(aes(x = age, y = loss_ratio), color = sun_red) +
    theme_minimal() +
    xlab("Asset Age") +
    ylab("Asset Loss Ratio")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p13.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    mutate(loss_ratio = simulation_premium/commercial_premium) %>% 
    ggplot() +
    geom_jitter(aes(x = voltage, y = loss_ratio), color = sun_red) +
    theme_minimal() +
    xlab("Voltage") +
    ylab("Asset Loss Ratio")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p14.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- premium %>% 
    mutate(loss_ratio = simulation_premium/commercial_premium) %>% 
    ggplot() +
    geom_jitter(aes(x = p_cap_ac, y = loss_ratio), color = sun_red) +
    theme_minimal() +
    xlab("Size (MWac)") +
    ylab("Asset Loss Ratio")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash2_p15.json", auto_unbox = TRUE, pretty = FALSE)
  
  
  # ----
  # Dashboard 3: Sites

  p <- us_states %>% 
    filter(!(STUSPS %in% c("AK", "AS", "MP", "GU", "HI", "PR", "VI"))) %>%
    ggplot() +
    geom_sf(fill = "gray", color = "white") +
    geom_point(data = mutate(premium, 
                             loss_rate = (simulation_premium)/commercial_premium,
                             censored_loss_rate = pmin(loss_rate, 1)),
               aes(x = xlong, y = ylat, 
                   text = paste0(
                     "<b>Asset:</b> ", p_name, "<br>",
                     "<b>State:</b> ", p_state, "<br>",
                     "<b>County:</b> ", p_county, "<br>",
                     "<b>TIV:</b> ", tiv, "<br>",
                     "<b>Commercial prem:</b> ", commercial_premium, "<br>",
                     "<b>Technical prem:</b> ", round(simulation_premium, 0), "<br>",
                     "<b>Hail rate:</b> ", hail_rate, "<br>",
                     "<b>Storm rate:</b> ", storm_rate, "<br>",
                     "<b>Hurricane rate:</b> ", hurricane_rate, "<br>",
                     "<b>Fire rate:</b> ", fire_rate, "<br>",
                     "<b>Inverter OEM:</b> ", inverter_oem, "<br>",
                     "<b>Racking type:</b> ", racking_type, "<br>",
                     "<b>Racking OEM:</b> ", racking_oem, "<br>",
                     "<b>Max tilt:</b> ", max_title_angle, "<br>",
                     "<b>Module OEM:</b> ", module_oem, "<br>",
                     "<b>Voltage:</b> ", voltage, "<br>"
                   ),
               color = censored_loss_rate), 
               size = 0.8,
               show.legend = FALSE) +
    scale_color_gradient2(low = sun_green,
                          mid = "black",
                          high = sun_red,
                          midpoint = target_loss_ratio) +
    coord_fixed(1.3) +
    theme_minimal()
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash3_p1.json", auto_unbox = TRUE, pretty = FALSE)
  
  
  # ----
  # Dashboard 4: CAT Risk
  
  us_counties <- counties() %>% 
    st_transform(crs = 4326)
  
  # frequency
  p <- simulation %>% 
    group_by(YEAR, iter) %>% 
    summarise(hail = sum(hail_insured_claim > 0)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_histogram(aes(x = hail), fill = sun_blue) +
    theme_minimal() +
    xlab("Number of Claims") +
    ylab("Count of Simulation Years") +
    ggtitle("Annual Insured Hail Claims")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash4_p5.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- simulation %>% 
    group_by(YEAR, iter) %>% 
    summarise(hurricane = sum(hurricane_insured_claim > 0)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_histogram(aes(x = hurricane), fill = sun_green) +
    theme_minimal() +
    xlab("Number of Claims") +
    ylab("Count of Simulation Years") +
    ggtitle("Annual Insured Hurricane Claims")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash4_p6.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- simulation %>% 
    group_by(YEAR, iter) %>% 
    summarise(storm = sum(storm_insured_claim > 0)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_histogram(aes(x = storm), fill = sun_purple) +
    theme_minimal() +
    xlab("Number of Claims") +
    ylab("Count of Simulation Years") +
    ggtitle("Annual Insured Storm Claims")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash4_p7.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- simulation %>% 
    group_by(YEAR, iter) %>% 
    summarise(fire = sum(fire_insured_claim > 0)) %>% 
    ungroup() %>% 
    ggplot() +
    geom_histogram(aes(x = fire), fill = sun_orange) +
    theme_minimal() +
    xlab("Number of Claims") +
    ylab("Count of Simulation Years") +
    ggtitle("Annual Insured Fire Claims")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash4_p8.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- simulation %>% 
    filter(hail_insured_claim > 0) %>% 
    ggplot() +
    geom_histogram(aes(x = hail_insured_claim), fill = sun_blue, bins = 60) +
    theme_minimal() +
    xlab("Size of Claims (% of TIV)") +
    ylab("Count of Claims") +
    ggtitle("Distribution of Hail Claims")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash4_p9.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- simulation %>% 
    filter(hurricane_insured_claim > 0) %>% 
    ggplot() +
    geom_histogram(aes(x = hurricane_insured_claim), fill = sun_green, bins = 60) +
    theme_minimal() +
    xlab("Size of Claims (% of TIV)") +
    ylab("Count of Claims") +
    ggtitle("Distribution of Hurricane Claims")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash4_p10.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- simulation %>% 
    filter(storm_insured_claim > 0) %>% 
    ggplot() +
    geom_histogram(aes(x = storm_insured_claim), fill = sun_purple, bins = 60) +
    theme_minimal() +
    xlab("Size of Claims (% of TIV)") +
    ylab("Count of Claims") +
    ggtitle("Distribution of Storm Claims")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash4_p11.json", auto_unbox = TRUE, pretty = FALSE)
  
  p <- simulation %>% 
    filter(fire_insured_claim > 0) %>% 
    ggplot() +
    geom_histogram(aes(x = fire_insured_claim), fill = sun_orange, bins = 60) +
    theme_minimal() +
    xlab("Size of Claims (% of TIV)") +
    ylab("Count of Claims") +
    ggtitle("Distribution of Fire Claims")
  
  p <- ggplotly(p)
  p <- plotly_build(p)$x
  payload <- list(data=p$data, layout=p$layout, config=p$config)
  write_json(payload, "./risk_engine/plots/dash4_p12.json", auto_unbox = TRUE, pretty = FALSE)
  
  
  
  
  
}