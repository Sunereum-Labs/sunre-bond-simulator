# ---- Packages ----
library(tidyverse); library(maps); library(plotly); library(geosphere); library(sf); library(rnaturalearth)

# ---- GSOD attributes ----
# impose lat & lon filters that preseve the mainland and HI, remove PR and obviously offshore/remote nodes
gsod_attributes <- readRDS("../data/GSOD_attributes.rds")
weather_station_lookup <- gsod_attributes %>% 
  filter(LONGITUDE > -160 & LONGITUDE < -66 & LATITUDE > 18 & LATITUDE < 50 &
           ELEVATION >= 0 &
           !(STATE %in% c("", "PR"))) %>% 
  distinct(STNID, .keep_all = TRUE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
rm(gsod_attributes); gc()

# ---- Group off-shore weather stations ----
us_map <- ne_states(country = "united states of america", returnclass = "sf") %>% 
  st_transform(4326)

station_state_boundary <- st_within(weather_station_lookup,
                                    us_map, sparse = TRUE)

offshore = c()
for (i in 1:nrow(weather_station_lookup)) {
  offshore[i] = is_empty(station_state_boundary[[i]])
}

weather_station_lookup$OFFSHORE <- offshore

plt <- ggplot() +
  geom_sf(data = us_map, fill = "gray80") +
  geom_sf(data = weather_station_lookup, aes(text = paste0(
    "<b>Name:</b> ", NAME, "<br>"
  ), color = OFFSHORE), size = 1) +
  theme_minimal()

ggplotly(plt)
rm(list = c("station_state_boundary", "offshore", "plt")); gc()

onshore_only = filter(weather_station_lookup, OFFSHORE == "FALSE")$STNID


# ---- Load GSOD peril data ----
gsod_peril2 <- readRDS("../data/GSOD_peril_1996-2023.rds")
gsod_peril2 <- gsod_peril2 %>% 
  filter(STNID %in% weather_station_lookup$STNID); gc()
gsod_peril2 <- gsod_peril2 %>% 
  distinct(STNID, YEARMODA, .keep_all = TRUE); gc()

gsod_peril1 <- readRDS("../data/GSOD_peril_1943-1995.rds")
gsod_peril1 <- gsod_peril1 %>% 
  filter(STNID %in% weather_station_lookup$STNID); gc()
gsod_peril1 <- gsod_peril1 %>% 
  distinct(STNID, YEARMODA, .keep_all = TRUE); gc()

# ----- Load US solar farm lookup ----
# downloaded from https://energy.usgs.gov/uspvdb/data/
us_solar <- read_csv("../data/uspvdb_v3_0_20250430.csv")

# ---- GSOD peril data investigation ----

# -- GUST NAs vs MXSP --

# Plot 1: let's see how the number of yearly NAs for GUST and MXSPD cluster
plt_data1 <- gsod_peril1 %>% 
  mutate(YEAR = year(YEARMODA)) %>% 
  group_by(STNID, YEAR) %>% 
  summarise(GUST = sum(is.na(GUST)),
            MXSPD = sum(is.na(MXSPD))) %>% 
  ungroup()

plt_data2 <- gsod_peril2 %>%
  mutate(YEAR = year(YEARMODA)) %>% 
  group_by(STNID, YEAR) %>% 
  summarise(GUST = sum(is.na(GUST)),
            MXSPD = sum(is.na(MXSPD))) %>% 
  ungroup()

plt_data1 %>% 
  rbind(plt_data2) %>% 
  ggplot() +
  geom_point(aes(x = MXSPD, y = GUST), alpha = 0.5) +
  ggtitle("Number of daily NAs per year by STNID") +
  theme_minimal()

# Plot 2: test hypothesis that GUST should always fall equal or below MXSPD
plt_data1 <- gsod_peril1 %>% 
  select(GUST, MXSPD) %>% 
  filter(!is.na(GUST) & !is.na(MXSPD))

plt_data2 <- gsod_peril2 %>% 
  select(GUST, MXSPD) %>% 
  filter(!is.na(GUST) & !is.na(MXSPD))

plt_data1 %>% 
  rbind(plt_data2) %>% 
  slice_sample(n = 1000000, replace = FALSE) %>% 
  ggplot() +
  geom_point(aes(x = MXSPD, y = GUST), alpha = 0.5) +
  geom_abline(aes(intercept = 0, slope = 1)) +
  theme_minimal() +
  ggtitle("GUST vs MXSPD")

# Plot 3: distribution of MXSPD when GUST is NA -- this is always when MXSPD < 25
plt_data1 <- gsod_peril1 %>% 
  select(GUST, MXSPD) %>% 
  filter(is.na(GUST) & !is.na(MXSPD))

plt_data2 <- gsod_peril2 %>% 
  select(GUST, MXSPD) %>% 
  filter(is.na(GUST) & !is.na(MXSPD))

plt_data1 %>% 
  rbind(plt_data2) %>% 
  ggplot() +
  geom_histogram(aes(x = MXSPD)) +
  ggtitle("Distribution of MXSPD when GUST is NA") +
  theme_minimal()

# -- PRCP NAs vs indicator flags --

# Plot 4: number of NAs per month
plt_data1 <- gsod_peril1 %>%
  mutate(YEAR = year(YEARMODA), MONTH = as.factor(month(YEARMODA))) %>% 
  group_by(STNID, YEAR, MONTH) %>% 
  summarise(PRCP = sum(is.na(PRCP))) %>% 
  ungroup()

plt_data2 <- gsod_peril2 %>%
  mutate(YEAR = year(YEARMODA), MONTH = as.factor(month(YEARMODA))) %>% 
  group_by(STNID, YEAR, MONTH) %>% 
  summarise(PRCP = sum(is.na(PRCP))) %>% 
  ungroup()

plt_data1 %>% 
  rbind(plt_data2) %>% 
  ggplot() +
  geom_boxplot(aes(x = MONTH, y = PRCP)) +
  ggtitle("Distribution of monthly PRCP daily NAs") +
  theme_minimal()

# Plot 5: PRCP NA percentages when; A) I_rf == NA, B) I_{all} == 0, C) I_{all} == 1, D) I_rf == 1 & I_{else} != 1, E) I_rf == 0 & I_else == 1
plt_data1 <- gsod_peril1 %>% 
  summarise(A = sum(is.na(PRCP) & is.na(I_RAIN_DRIZZLE)),
            B = sum(is.na(PRCP) & I_RAIN_DRIZZLE == 1 & I_SNOW_ICE == 1 & I_HAIL == 1),
            C = sum(is.na(PRCP) & I_RAIN_DRIZZLE == 0 & I_SNOW_ICE == 0 & I_HAIL == 0),
            D = sum(is.na(PRCP) & I_RAIN_DRIZZLE == 1 & I_SNOW_ICE != 1 & I_HAIL != 1),
            E = sum(is.na(PRCP) & I_RAIN_DRIZZLE == 0 & I_SNOW_ICE == 1 & I_HAIL == 1))

plt_data2 <- gsod_peril2 %>% 
  summarise(A = sum(is.na(PRCP) & is.na(I_RAIN_DRIZZLE)),
            B = sum(is.na(PRCP) & I_RAIN_DRIZZLE == 1 & I_SNOW_ICE == 1 & I_HAIL == 1),
            C = sum(is.na(PRCP) & I_RAIN_DRIZZLE == 0 & I_SNOW_ICE == 0 & I_HAIL == 0),
            D = sum(is.na(PRCP) & I_RAIN_DRIZZLE == 1 & I_SNOW_ICE != 1 & I_HAIL != 1),
            E = sum(is.na(PRCP) & I_RAIN_DRIZZLE == 0 & I_SNOW_ICE == 1 & I_HAIL == 1))
            
plt_data1 %>% 
  rbind(plt_data2) %>% 
  summarise(across(A:E, ~sum(.))) %>% 
  pivot_longer(cols = A:E, names_to = "case", values_to = "NAs") %>% 
  ggplot() +
  geom_bar(aes(x = case, y = NAs), stat = "identity") +
  theme_minimal() +
  scale_y_log10() +
  ggtitle("Number of PRCP NAs by rain, snow, hail indicator flag cases")

# -- DEWP NAs vs TEMP & RH --

# Plot 6: DEWP NAs vs TEMP and RH NAs
plt_data1 <- gsod_peril1 %>% 
  summarise(A = sum(is.na(DEWP) & !is.na(RH) & is.na(TEMP)),
            B = sum(is.na(DEWP) & is.na(RH) & !is.na(TEMP)),
            C = sum(is.na(DEWP) &  !is.na(RH) & !is.na(TEMP)),
            D = sum(is.na(DEWP) &  is.na(RH) & is.na(TEMP)))

plt_data2 <- gsod_peril2 %>% 
  summarise(A = sum(is.na(DEWP) & !is.na(RH) & is.na(TEMP)),
            B = sum(is.na(DEWP) & is.na(RH) & !is.na(TEMP)),
            C = sum(is.na(DEWP) &  !is.na(RH) & !is.na(TEMP)),
            D = sum(is.na(DEWP) &  is.na(RH) & is.na(TEMP)))

plt_data1 %>% 
  rbind(plt_data2) %>% 
  summarise(across(A:D, ~sum(.))) %>% 
  pivot_longer(cols = A:D, names_to = "case", values_to = "NAs") %>% 
  ggplot() +
  geom_bar(aes(x = case, y = NAs), stat = "identity") +
  theme_minimal() +
  ggtitle("Number of DEWP NAs by RH and TEMP NAs")

# Plot 7: distrution of TEMP for DEWP NAs
plt_data1 <- gsod_peril1 %>% 
  filter(is.na(DEWP))

plt_data2 <- gsod_peril2 %>%
  filter(is.na(DEWP))

plt_data1 %>% 
  rbind(plt_data2) %>% 
  ggplot() +
  geom_histogram(aes(x = TEMP)) +
  ggtitle("Distribution of TEMP when DEWP is NA") +
  theme_minimal()

# -- Large values of PRCP ---
# We shouldn't have values above 43", exceptin HI given this is the highest in mainland US history 

plt_data1 <- gsod_peril1 %>%
  filter(STNID %in% onshore_only) %>% 
  filter(PRCP >= 43)

plt_data2 <- gsod_peril2 %>%
  filter(STNID %in% onshore_only) %>% 
  filter(PRCP >= 43)

# Plot 8: histogram of PRCP >= 43" by rain flag
plt_data1 %>% 
  rbind(plt_data2) %>% 
  ggplot() +
  geom_histogram(aes(x = PRCP)) +
  ggtitle("Distribution of improbably large PRCP by RAIN_DRIZZLE flag") +
  scale_x_log10() +
  facet_wrap(~I_RAIN_DRIZZLE, nrow = 2) +
  theme_minimal()

# Plot 9: histogram of PRCP >= 43" by snow flag
plt_data1 %>% 
  rbind(plt_data2) %>% 
  ggplot() +
  geom_histogram(aes(x = PRCP)) +
  ggtitle("Distribution of improbably large PRCP by SNOW flag") +
  scale_x_log10() +
  facet_wrap(~I_SNOW_ICE, nrow = 2) +
  theme_minimal()

# Plot 10: histogram of PRCP >= 43" by hail flag
plt_data1 %>% 
  rbind(plt_data2) %>% 
  ggplot() +
  geom_histogram(aes(x = PRCP)) +
  ggtitle("Distribution of improbably large PRCP by HAIL flag") +
  scale_x_log10() +
  facet_wrap(~I_HAIL, nrow = 2) +
  theme_minimal()

# Plot 11: time series scatter plot of PRCP >= 43"
plt_data1 %>% 
  rbind(plt_data2) %>% 
  ggplot() +
  geom_point(aes(x = YEARMODA, y = PRCP), alpha = 0.1)

# Plot 12: suppose it's possible that some data is stored as tenths or hundredths of inches
plt_data3 <- gsod_peril1 %>%
  filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
  group_by(STNID) %>% 
  summarise(med = median(PRCP),
            upper = quantile(PRCP, 0.75),
            n = n())

plt_data4 <- gsod_peril2 %>%
  filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
  group_by(STNID) %>% 
  summarise(med = median(PRCP),
            upper = quantile(PRCP, 0.75),
            n = n())

# these values are an order of magnitude too high
plt_data3 %>% 
  rbind(plt_data4) %>% 
  arrange(desc(med)) %>% 
  ungroup() %>% 
  filter(n > 1000) %>% 
  filter(med >= quantile(med, 0.75)) %>% 
  ggplot() +
  geom_point(aes(y = med, x = fct_reorder(STNID, med, .desc = TRUE))) +
  geom_errorbar(aes(ymin = med, ymax = upper, x = fct_reorder(STNID, med, .desc = TRUE))) +
  scale_y_log10() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Median and upper quantile of PRCP for top 25% of weather stations with >1000 record rainy days")

# Plot 13: let's just plot a boxplot of all the weather station medians
# - most values are above 4" which is definitely too high
plt_data3 %>% 
  rbind(plt_data4) %>% 
  filter(n > 1000) %>% 
  ggplot() +
  geom_boxplot(aes(y = med)) +
  ggtitle("Median PRCP distribution") +
  theme_minimal()

# compute global maxes out of function across both data frames for prcp_adjust, 
#to eliminate checks, if rainfall is recorded at any time (>=42 + 1" mainland, 
# >= (49.69 ~ 0.5" = 50") in HI) & flag == CHECK, then rebase
global_max <- gsod_peril1 %>%
  rbind(gsod_peril2) %>% 
  filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
  group_by(STNID, STATE) %>% 
  summarise(global_max = max(PRCP)) %>% 
  ungroup()

# let's write a function for PRCP data correction by year
prcp_adjust <- function(df, id_col, date_col, state_col) {
  
  # find medians and percentage obs with 2 tailing decimals
  df2 <- df %>% 
    filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
    mutate(yr = year(!!sym(date_col))) %>% 
    group_by(!!sym(id_col), yr, !!sym(state_col)) %>% 
    summarise(med = median(PRCP),
              max = max(PRCP),
              d2 = sum(!(PRCP %% 0.1 == 0), na.rm = TRUE)/n()) %>% 
    ungroup()
  
  # regional rainfall rainy day medians from NOAA NCEI and USDA/NRCS
  regions = c("SW", "California", "Pacific NW", "Midwest", "NE",
              "Gulf Coast", "Appalachians", "Hawaii", "Alaska", "Rockies")
  upper = c(0.15, 0.3, 0.35, 0.4, 0.45, 0.6, 0.5, 1, 0.3, 0.25)
  upper <- setNames(upper, regions)
  state_regions <- c(
    # Southwest
    "AZ" = "SW", "NM" = "SW", "NV" = "SW", "UT" = "SW",
    
    # California (separate due to unique rain pattern)
    "CA" = "California",
    
    # Pacific Northwest
    "WA" = "Pacific NW", "OR" = "Pacific NW",
    
    # Midwest
    "IL" = "Midwest", "IN" = "Midwest", "IA" = "Midwest", "OH" = "Midwest",
    "MI" = "Midwest", "WI" = "Midwest", "MN" = "Midwest", "MO" = "Midwest",
    "ND" = "Midwest", "SD" = "Midwest", "NE" = "Midwest", "KS" = "Midwest",
    
    # Northeast
    "NY" = "NE", "PA" = "NE", "MA" = "NE", "NJ" = "NE",
    "CT" = "NE", "RI" = "NE", "VT" = "NE", "NH" = "NE",
    "ME" = "NE",
    
    # Gulf Coast / Southeast (humid subtropical)
    "FL" = "Gulf Coast", "LA" = "Gulf Coast", "MS" = "Gulf Coast",
    "AL" = "Gulf Coast", "GA" = "Gulf Coast", "SC" = "Gulf Coast",
    "NC" = "Gulf Coast", "TX" = "Gulf Coast",
    
    # Appalachians / Interior Southeast
    "TN" = "Appalachians", "KY" = "Appalachians", "WV" = "Appalachians",
    "VA" = "Appalachians", "AR" = "Appalachians",
    
    # Hawaii
    "HI" = "Hawaii",
    
    # Alaska
    "AK" = "Alaska",
    
    # Mountain/Intermountain West (drier)
    "CO" = "Rockies", "MT" = "Rockies", "ID" = "Rockies", "WY" = "Rockies",
    
    # Plains crossover
    "OK" = "Midwest",  # could also go "Plains"
    
    # Mid-Atlantic
    "DE" = "NE", "MD" = "NE", "DC" = "NE"  # if using DC
    
    # Not included: territories like PR, GU, VI
  )
  
  df2 <- df2 %>% 
    mutate(region = state_regions[!!sym(state_col)],
           regional_med = upper[region],
           med_factor = med/regional_med,
           flag = case_when(med >= 4 ~ "REBASE",
                            med_factor >= 10 ~ "REBASE",
                            med_factor >= 3 & med >= 1 ~ "REBASE",
                            max >= 43 & !!sym(state_col) != "HI" ~ "REBASE",
                            max >= 50 & !!sym(state_col) == "HI" ~ "REBASE",
                            med_factor <= 1 ~ "LEAVE",
                            TRUE ~ "CHECK")) %>% 
    select(!!sym(id_col), !!sym(state_col), yr, med, regional_med, med_factor, d2, flag)
  
  df2 <- df2 %>% 
    left_join(global_max, by = c(id_col, state_col)) %>%
    mutate(flag2 = case_when(flag %in% c("REBASE", "LEAVE") ~ flag,
                             flag == "CHECK" & global_max >= 43 & !!sym(state_col) != "HI" ~ "REBASE",
                             flag == "CHECK" & global_max >= 50 & !!sym(state_col) == "HI" ~ "REBASE",
                             TRUE ~ flag)) %>% 
    select(-flag) %>% 
    rename("flag" = flag2)
  
  return(df2)
}

prcp_adjusted1 <- prcp_adjust(df = gsod_peril1, id_col = "STNID",
                              date_col = "YEARMODA", state_col = "STATE")

prcp_adjusted2 <- prcp_adjust(df = gsod_peril2, id_col = "STNID",
                              date_col = "YEARMODA", state_col = "STATE")

# Plot 14: now let's REBASE or LEAVE PRCP on weather stations, then plot stations to be CHECK'ed
plt_data1 <- gsod_peril1 %>% 
  filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
  mutate(yr = year(YEARMODA)) %>% 
  left_join(select(prcp_adjusted1,
                   STNID, STATE, yr, flag), by = c("STNID", "STATE", "yr")) %>% 
  mutate(PRCP = if_else(flag == "REBASE", PRCP/10, PRCP),
         id_col = if_else(flag == "CHECK", STNID, "STATE")) %>% 
  select(id_col, STATE, PRCP)

plt_data2 <- gsod_peril2 %>% 
  filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
  mutate(yr = year(YEARMODA)) %>% 
  left_join(select(prcp_adjusted2,
                   STNID, STATE, yr, flag), by = c("STNID", "STATE", "yr")) %>% 
  mutate(PRCP = if_else(flag == "REBASE", PRCP/10, PRCP),
         id_col = if_else(flag == "CHECK", STNID, "STATE")) %>% 
  select(id_col, STATE, PRCP)


# toggle each state plot, note the weather stations that are not being left here
check_states <- unique(c(filter(plt_data1, id_col != "STATE")$STATE, 
                         filter(plt_data2, id_col != "STATE")$STATE))
to_leave = c("722406-12927", "725837-99999", "723894-03181")
state = check_states[37]
plt_data1 %>% 
  rbind(plt_data2) %>% 
  filter(STATE == state) %>%
  ggplot() +
  geom_boxplot(aes(x = reorder(id_col, -PRCP, FUN = median), y = PRCP)) +
  geom_hline(aes(yintercept = 0.3)) +
  geom_hline(aes(yintercept = 1)) +
  geom_hline(aes(yintercept = 2)) +
  geom_hline(aes(yintercept = 4)) +
  ylim(0, 10) +
  ggtitle("PRCP distribution for CHECK value weather stations vs state") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme_minimal()

# ask GPT to evaluate whether any of these areas should have higher than average intensity rainfall
weather_station_lookup %>% 
  filter(STATE == state) %>% 
  filter(STNID %in% unique(c(plt_data1$id_col, plt_data2$id_col))) %>% 
  .$NAME %>% 
  paste0(., collapse = ", ")

# let's now convert CHECKs to REBASE unless marked to be left as a leave
prcp_adjusted1 <- prcp_adjusted1 %>% 
  mutate(flag2 = case_when(flag == "CHECK" & (STNID %in% to_leave) ~ "LEAVE",
                        flag == "CHECK" & !(STNID %in% to_leave) ~ "REBASE",
                        TRUE ~ flag)) %>% 
  select(-flag) %>% 
  rename("flag" = flag2)

prcp_adjusted2 <- prcp_adjusted2 %>% 
  mutate(flag2 = case_when(flag == "CHECK" & (STNID %in% to_leave) ~ "LEAVE",
                           flag == "CHECK" & !(STNID %in% to_leave) ~ "REBASE",
                           TRUE ~ flag)) %>% 
  select(-flag) %>% 
  rename("flag" = flag2)

# looking at the state boxplots, the distributions still seem quite high
# let's now look at extremely high annual rainfall for those being "left"
prcp_leave <- prcp_adjusted1 %>% 
  rbind(prcp_adjusted2) %>% 
  filter(flag == "LEAVE")

prcp_check2 <- gsod_peril1 %>% 
  rbind(gsod_peril2) %>% 
  mutate(yr = year(YEARMODA)) %>% 
  semi_join(prcp_leave, by = c("STNID", "yr")) %>% 
  filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0)

# Vector of upper limit of annual rainfall and absolute maximums
regions = c("SW", "California", "Pacific NW", "Midwest", "NE",
            "Gulf Coast", "Appalachians", "Hawaii", "Alaska", "Rockies")
upper = c(15, 30, 60, 40, 50, 65, 65, 100, 30, 40)
upper <- setNames(upper, regions)
state_regions <- c(
  # Southwest
  "AZ" = "SW", "NM" = "SW", "NV" = "SW", "UT" = "SW",
  
  # California (separate due to unique rain pattern)
  "CA" = "California",
  
  # Pacific Northwest
  "WA" = "Pacific NW", "OR" = "Pacific NW",
  
  # Midwest
  "IL" = "Midwest", "IN" = "Midwest", "IA" = "Midwest", "OH" = "Midwest",
  "MI" = "Midwest", "WI" = "Midwest", "MN" = "Midwest", "MO" = "Midwest",
  "ND" = "Midwest", "SD" = "Midwest", "NE" = "Midwest", "KS" = "Midwest",
  
  # Northeast
  "NY" = "NE", "PA" = "NE", "MA" = "NE", "NJ" = "NE",
  "CT" = "NE", "RI" = "NE", "VT" = "NE", "NH" = "NE",
  "ME" = "NE",
  
  # Gulf Coast / Southeast (humid subtropical)
  "FL" = "Gulf Coast", "LA" = "Gulf Coast", "MS" = "Gulf Coast",
  "AL" = "Gulf Coast", "GA" = "Gulf Coast", "SC" = "Gulf Coast",
  "NC" = "Gulf Coast", "TX" = "Gulf Coast",
  
  # Appalachians / Interior Southeast
  "TN" = "Appalachians", "KY" = "Appalachians", "WV" = "Appalachians",
  "VA" = "Appalachians", "AR" = "Appalachians",
  
  # Hawaii
  "HI" = "Hawaii",
  
  # Alaska
  "AK" = "Alaska",
  
  # Mountain/Intermountain West (drier)
  "CO" = "Rockies", "MT" = "Rockies", "ID" = "Rockies", "WY" = "Rockies",
  
  # Plains crossover
  "OK" = "Midwest",  # could also go "Plains"
  
  # Mid-Atlantic
  "DE" = "NE", "MD" = "NE", "DC" = "NE"  # if using DC
  
  # Not included: territories like PR, GU, VI
)

# check values above the typical regional range (noting this is imperfect)
prcp_check2 <- prcp_check2 %>% 
  group_by(STNID, STATE, yr) %>% 
  summarise(annual = sum(PRCP)) %>% 
  mutate(region = state_regions[STATE],
         upper = upper[region],
         flag = case_when(annual > upper ~ "CHECK",
                          TRUE ~ "LEAVE")) %>% 
  left_join(weather_station_lookup, by = c("STNID", "STATE"))

# (manually check the data frame)
View(filter(prcp_check2, flag == "CHECK"))

to_leave2 = c("723894-03181_2017", "722404-13941_2019", "722406-12927_2012", "723626-03737_2010",
              "720358-53999_2022", "722173-53951_2023")

prcp_check2 <- prcp_check2 %>% 
  mutate(id_col = paste(STNID, "_", yr, sep = ""),
         flag2 = case_when(flag == "CHECK" & (id_col %in% to_leave2) ~ "LEAVE",
                           flag == "CHECK" & !(id_col %in% to_leave2) ~ "REBASE",
                           TRUE ~ flag)) %>% 
  select(-flag) %>% 
  rename("flag" = flag2)

# final PRCP rebasement list
prcp_adjusted <- prcp_adjusted1 %>% 
  rbind(prcp_adjusted2) %>% 
  # remove old LEAVE
  filter(flag != "LEAVE") %>% 
  select(STNID, STATE, yr, flag) %>% 
  # rbind new LEAVE
  rbind(select(prcp_check2,
               STNID, STATE, yr, flag))

rm(list = c("plt_data1", "plt_data2", "plt_data3", "plt_data4",
            "prcp_adjusted1", "prcp_adjusted2", "prcp_check2", "prcp_leave"))

# -- DEWP large values --
# view points above 30*
gsod_peril1 %>%
  select(STNID, YEARMODA, DEWP, TEMP, MAX) %>% 
  rbind(select(gsod_peril2,
               STNID, YEARMODA, DEWP, TEMP, MAX)) %>% 
  filter(DEWP > 30) %>% 
  arrange(desc(DEWP)) %>% 
  left_join(weather_station_lookup, by = "STNID") %>%
  filter(OFFSHORE == "FALSE") %>% 
  View()

# ---- GSOD peril data merge & data handling ----
gsod_peril1 <- gsod_peril1 %>% 
  select(-RH, -I_HAIL, -I_SNOW_ICE)

gsod_peril2 <- gsod_peril2 %>% 
  select(-RH, -I_HAIL, -I_SNOW_ICE)

rm(plt_data1); rm(plt_data2); rm(plt_data3); rm(plt_data4); gc()

gsod_peril <- gsod_peril1 %>% 
  rbind(gsod_peril2)
rm(list = c("gsod_peril1", "gsod_peril2")); gc()

# adjustments to peril fields based on the previous investigation
gsod_peril <- gsod_peril %>% 
  # remove offshore weather stations
  filter(STNIND %in% onshore_only)

# peril summary with offshore stations
gsod_peril %>% summary()

gsod_peril <- gsod_peril %>% 
  # Wind peril adjustments
  mutate(GUST = replace_na(GUST, MXSPD),
         GUST = pmax(GUST, MXSPD))

# peril summary without offshore stations
gsod_peril %>% filter(STNID %in% onshore_only) %>% summary()

# ---- Summary value investigation ----

plt <- plt_data %>% 
  group_by(STNID, NAME, ELEVATION, geometry) %>% 
  summarise(n = n()) %>% 
  ungroup() %>%
  ggplot() +
  geom_sf(data = us_map, fill = "gray80") +
  geom_sf(aes(text = paste0(
    "<b>Name:</b> ", NAME, "<br>",
    "<b>Elevation:</b> ", ELEVATION, "<br>",
    "<b>N:</b> ", n),
    color = n,
    geometry = geometry),
    size = 0.5) +
  scale_color_gradient2(low = "orange", mid ="black", high = "lightblue", midpoint = log10(50), trans = "log10") +
  theme_minimal()

ggplotly(plt)

# ---- NAs handling ----
# hypothesis -- stations recording a PRCP > 43ins are located offshore
plt_data1 <- gsod_peril %>% 
  select(STNID, STATE, YEARMODA, PRCP) %>% 
  filter(PRCP > 43) %>% 
  

plt_data2 <- weather_station_lookup %>% 
  filter(STNID %in% plt_data1$STNID)

gsod_peril <- gsod_peril %>% 
  mutate(GUST = replace_na(GUST, MXSPD),
         GUST = pmax(GUST, MXSPD))

# ---- NOAA hail data ----
file <- "StormEvents_details-ftp_v1.0_d1950_c20250401.csv"
hail <- read_csv(paste0("../data/", file)) %>% 
  filter(EVENT_TYPE == "Hail")

file <- "StormEvents_details-ftp_v1.0_d2020_c20240620.csv"
temp <- read_csv(paste0("../data/", file)) %>% 
  filter(EVENT_TYPE == "Hail")

hail <- hail %>% 
  rbind(temp)

years <- c(1951:2019, 2021:2023)
for (year in years) {
  file <- sprintf("StormEvents_details-ftp_v1.0_d%d_c20250520.csv", year)
  temp <- read_csv(paste0("../data/", file)) %>% 
    filter(EVENT_TYPE == "Hail")
  hail <- hail %>% 
    rbind(temp)
}

# ---- TBD ----

# now let's create an index for complete and incomplete data by weather station
weather_station_completeness_index <- gsod_peril %>% 
  mutate(YEAR = year(YEARMODA)) %>% 
  group_by(STNID, YEAR) %>% 
  summarise(across(-c(1:2), ~sum(!is.na(.)))) %>% 
  ungroup() %>% 
  mutate(across(colnames(.)[-c(1, 2)], ~if_else(. >= 345, "COMPLETE", "INCOMPLETE")))
  
# join on state and concatenate station list
weather_station_completeness_index <- weather_station_completeness_index %>% 
  left_join(select(weather_station_lookup, STNID, STATE), by = "STNID") %>% 
  filter(!is.na(STATE))

# now let's map (AC > 20MW) solar farms to the nearest COMPLETE weather station by year
# - solar farm latitude and longitude are calculated using AEA BUT are in degrees, so can compute distances directly
solar_farm_lookup <- us_solar %>% 
  filter(p_cap_ac > 20) %>% 
  filter(xlong > -160 & xlong < -66 & ylat > 18 & ylat < 50) %>% 
  select(case_id, p_name, p_year, p_state, p_county, ylat, xlong, p_cap_ac)

case_id = c(); STNID = c(); distance = c()
for (i in 1:nrow(solar_farm_lookup)) {
  
  dist_vctr <- distm(x = c(solar_farm_lookup$xlong[i], solar_farm_lookup$ylat[i]),
                    y = cbind(weather_station_lookup$LONGITUDE, weather_station_lookup$LATITUDE),
                    fun = distHaversine)/1000
  
  case_id = c(case_id, rep(solar_farm_lookup$case_id[i], length(dist_vctr)))
  STNID = c(STNID, weather_station_lookup$STNID)
  distance = c(distance, dist_vctr[1,])
}
  
solar_stnid_distance <- tibble(case_id, STNID, distance)
rm(list = c("case_id", "STNID", "distance"))

solar_stnid_lookup_table <- tibble()
for (p in c("GUST", "MXSPD", "PRCP", "MAX", "TEMP", "DEWP")) {
  
  p_lookup <- weather_station_completeness_index %>% 
    filter(!!sym(p) == "COMPLETE")

  for (j in unique(weather_station_completeness_index$YEAR)) {
    j_lookup <- solar_stnid_distance %>%
      semi_join(filter(p_lookup, YEAR == j), by = "STNID") %>% 
      group_by(case_id) %>% 
      filter(distance == min(distance)) %>% 
      mutate(YEAR = j, PERIL = p)
    
    solar_stnid_lookup_table <- solar_stnid_lookup_table %>% 
      rbind(j_lookup)
  }
}

# map data
us_map2 <- map_data("state")

state_codes <- tibble(
  state = tolower(state.name),
  code = state.abb,
  stringsAsFactors = FALSE)

state_centroids <- us_map2 %>%
  group_by(region) %>%
  summarise(long = mean(range(long)), lat = mean(range(lat))) %>%
  left_join(state_codes, by = c("region" = "state"))

# plot our weather station look-up on the map
plt <- ggplot() +
  geom_sf(data = us_map, fill = "gray95", color = "white") +
  geom_sf(data = weather_station_lookup, 
                           aes(text = paste0(
                                 "<b>Name:</b> ", NAME, "<br>",
                                 "<b>Station ID:</b> ", STNID, "<br>",
                                 "<b>State:</b> ", STATE, "<br>",
                                 "<b>Comm ID:</b> ", BEGIN, "<br>",
                                 "<b>Decomm ID:</b> ", END, "<br>",
                                 "<b>Elevation:</b> ", ELEVATION
                                 )),
             color = "black", size = 0.015, alpha = 0.5) +
  geom_point(data = solar_farm_lookup, 
             aes(x = xlong, y = ylat, 
                 text = paste0(
                   "<b>Case ID:</b> ", case_id, "<br>",
                   "<b>Name:</b> ", p_name, "<br>",
                   "<b>State:</b> ", p_state, "<br>",
                   "<b>County:</b> ", p_county, "<br>",
                   "<b>AC Capacity:</b> ", p_cap_ac, "<br>",
                   "<b>Comm year:</b> ", p_year
                 )),
             color = "orange", size = 0.3) +
  coord_fixed(1.3) +
  theme_minimal()

plt <- ggplotly(plt)

htmlwidgets::saveWidget(plt, "../gsod_map/index.html", selfcontained = FALSE)
write_csv(weather_station_lookup, "../data/weather_station_lookup.csv")
write_csv(solar_farm_lookup, "../data/solar_farm_lookup.csv")
write_csv(solar_stnid_lookup_table, "../data/solar_stnid_lookup.csv")


# find 1-in-2 and 1-in-5 year triggers for each peril variable
vars = colnames(gsod_peril)[-c(1:3)]

N_2 = c()
N_5 = c()
n = c()
for (i in 1:length(vars)) {
  
  N = gsod_peril %>% 
    # makesure we only include concatenated data
    filter(STNID %in% weather_station_lookup$STNID) %>% 
    select(!!vars[i]) %>% 
    .[[1]] %>% 
    sort()
  
  # 1-in-2 year events
  N_2[i] = N %>% 
    tail(floor(1/730 * length(.))) %>% 
    .[1]
  
  # 1-in-5 year events
  N_5[i] = N %>% 
    tail(floor(1/1825 * length(.))) %>% 
    .[1]
  
  n[i] = length(N)
}

triggers <- tibble(var = vars, `1n2` = N_2, `1n5` = N_5, n = n)
write_csv(triggers, "contract_triggers.csv")


# calculate expected premiums

# state expected premiums (calculating these as a check on the triggers)
states = unique(weather_station_lookup$STATE)
P <- tibble(STATE = states)
for (i in 1:length(vars)) {
  N_2 = triggers %>% 
    filter(var == vars[i]) %>% 
    .$`1n2`
  
  N_5 = triggers %>% 
    filter(var == vars[i]) %>% 
    .$`1n5`
  
  temp = gsod_peril %>% 
    select(STATE, !!sym(vars[i])) %>% 
    filter(!is.na(!!sym(vars[i]))) %>% 
    group_by(STATE) %>%
    summarise(!!paste0(vars[i], "_2") := sum(!!sym(vars[i]) >= N_2)/n(),
              !!paste0(vars[i], "_5") := sum(!!sym(vars[i]) >= N_5)/n())
  
  P <- P %>% 
    left_join(temp, by = "STATE")
}

write_csv(P, "state_premiums.csv")
