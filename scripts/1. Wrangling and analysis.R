# ---- Packages ----
library(tidyverse); library(maps); library(plotly); library(geosphere); library(sf); library(rnaturalearth); library(dbscan)

# ----- Load US solar farm lookup ----
# downloaded from https://energy.usgs.gov/uspvdb/data/
us_solar <- read_csv("../data/uspvdb_v3_0_20250430.csv")


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

#plt <- ggplot() +
#  geom_sf(data = us_map, fill = "gray80") +
#  geom_sf(data = weather_station_lookup, aes(text = paste0(
#    "<b>Name:</b> ", NAME, "<br>"
#  ), color = OFFSHORE), size = 1) +
#  theme_minimal()

#ggplotly(plt)
rm(list = c("station_state_boundary", "offshore", "plt")); gc()

onshore_only = filter(weather_station_lookup, OFFSHORE == "FALSE")$STNID


# ---- Load GSOD peril data ----
gsod_peril2 <- readRDS("../data/lfs/GSOD_peril_1996-2023.rds")
gsod_peril2 <- gsod_peril2 %>% 
  filter(STNID %in% weather_station_lookup$STNID); gc()
gsod_peril2 <- gsod_peril2 %>% 
  distinct(STNID, YEARMODA, .keep_all = TRUE); gc()

gsod_peril1 <- readRDS("../data/lfs/GSOD_peril_1943-1995.rds")
gsod_peril1 <- gsod_peril1 %>% 
  filter(STNID %in% weather_station_lookup$STNID); gc()
gsod_peril1 <- gsod_peril1 %>% 
  distinct(STNID, YEARMODA, .keep_all = TRUE); gc()

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

# Plot 7: distribution of TEMP for DEWP NAs
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

rm(plt_data1); rm(plt_data2); rm(plt_data3); rm(plt_data4); gc()

# ---- NAs handling ----
gsod_peril1 <- gsod_peril1 %>% 
  mutate(GUST = coalesce(GUST, MXSPD),
         GUST = pmax(GUST, MXSPD),
         PRCP = if_else(I_RAIN_DRIZZLE == 1, PRCP, 0))

gsod_peril2 <- gsod_peril2 %>% 
  mutate(GUST = coalesce(GUST, MXSPD),
         GUST = pmax(GUST, MXSPD),
         PRCP = if_else(I_RAIN_DRIZZLE == 1, PRCP, 0))


# ---- Algorithm: detecting large values of PRCP ----
# We shouldn't have values above 43", except in HI given this is the highest in mainland US history 
#plt_data1 <- gsod_peril1 %>%
#  filter(STNID %in% onshore_only) %>% 
#  filter(PRCP >= 43)

#plt_data2 <- gsod_peril2 %>%
#  filter(STNID %in% onshore_only) %>% 
#  filter(PRCP >= 43)

# Plot 8: histogram of PRCP >= 43" by rain flag
#plt_data1 %>% 
#  rbind(plt_data2) %>% 
#  ggplot() +
#  geom_histogram(aes(x = PRCP)) +
#  ggtitle("Distribution of improbably large PRCP by RAIN_DRIZZLE flag") +
# scale_x_log10() +
# facet_wrap(~I_RAIN_DRIZZLE, nrow = 2) +
# theme_minimal()

# Plot 9: histogram of PRCP >= 43" by snow flag
#plt_data1 %>% 
#  rbind(plt_data2) %>% 
# ggplot() +
# geom_histogram(aes(x = PRCP)) +
# ggtitle("Distribution of improbably large PRCP by SNOW flag") +
# scale_x_log10() +
# facet_wrap(~I_SNOW_ICE, nrow = 2) +
# theme_minimal()

# Plot 10: histogram of PRCP >= 43" by hail flag
#plt_data1 %>% 
# rbind(plt_data2) %>% 
# ggplot() +
# geom_histogram(aes(x = PRCP)) +
# ggtitle("Distribution of improbably large PRCP by HAIL flag") +
# scale_x_log10() +
# facet_wrap(~I_HAIL, nrow = 2) +
# theme_minimal()

# Plot 11: time series scatter plot of PRCP >= 43"
#plt_data1 %>% 
# rbind(plt_data2) %>% 
# ggplot() +
# geom_point(aes(x = YEARMODA, y = PRCP), alpha = 0.1) + 
# theme_minimal() +
# ggtilte("Time series scatter plot of PRCP >= 43")

# Plot 12: suppose it's possible that some data is stored as tenths or hundredths of inches
#plt_data3 <- gsod_peril1 %>%
# filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
# group_by(STNID) %>% 
# summarise(med = median(PRCP),
#           upper = quantile(PRCP, 0.75),
#           n = n())

#plt_data4 <- gsod_peril2 %>%
# filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
# group_by(STNID) %>% 
# summarise(med = median(PRCP),
#           upper = quantile(PRCP, 0.75),
#           n = n())

# these values are an order of magnitude too high
#plt_data3 %>% 
#  rbind(plt_data4) %>% 
# arrange(desc(med)) %>% 
# ungroup() %>% 
# filter(n > 1000) %>% 
# filter(med >= quantile(med, 0.75)) %>% 
# ggplot() +
# geom_point(aes(y = med, x = fct_reorder(STNID, med, .desc = TRUE))) +
# geom_errorbar(aes(ymin = med, ymax = upper, x = fct_reorder(STNID, med, .desc = TRUE))) +
# scale_y_log10() +
# theme_minimal() +
# theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
# ggtitle("Median and upper quantile of PRCP for top 25% of weather stations with >1000 record rainy days")

# Plot 13: let's just plot a boxplot of all the weather station medians
# - most values are above 4" which is definitely too high
#plt_data3 %>% 
#  rbind(plt_data4) %>% 
#  filter(n > 1000) %>% 
# ggplot() +
# geom_boxplot(aes(y = med)) +
# ggtitle("Median PRCP distribution") +
# theme_minimal()

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

# MANUAL CHECK: now let's REBASE or LEAVE PRCP on weather stations, then plot stations to be CHECK'ed
#plt_data1 <- gsod_peril1 %>% 
#  filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
#  mutate(yr = year(YEARMODA)) %>% 
#  left_join(select(prcp_adjusted1,
#                   STNID, STATE, yr, flag), by = c("STNID", "STATE", "yr")) %>% 
#  mutate(PRCP = if_else(flag == "REBASE", PRCP/10, PRCP),
#         id_col = if_else(flag == "CHECK", STNID, "STATE")) %>% 
#  select(id_col, STATE, PRCP)

#plt_data2 <- gsod_peril2 %>% 
#  filter(I_RAIN_DRIZZLE == 1 & !is.na(PRCP) & PRCP > 0) %>% 
#  mutate(yr = year(YEARMODA)) %>% 
#  left_join(select(prcp_adjusted2,
#                   STNID, STATE, yr, flag), by = c("STNID", "STATE", "yr")) %>% 
#  mutate(PRCP = if_else(flag == "REBASE", PRCP/10, PRCP),
#         id_col = if_else(flag == "CHECK", STNID, "STATE")) %>% 
#  select(id_col, STATE, PRCP)


# toggle each state plot, note the weather stations that are not being left here
#check_states <- unique(c(filter(plt_data1, id_col != "STATE")$STATE, 
#                         filter(plt_data2, id_col != "STATE")$STATE))
to_leave = c("722406-12927", "725837-99999", "723894-03181")
#state = check_states[37]
#plt_data1 %>% 
#  rbind(plt_data2) %>% 
#  filter(STATE == state) %>%
#  ggplot() +
#  geom_boxplot(aes(x = reorder(id_col, -PRCP, FUN = median), y = PRCP)) +
#  geom_hline(aes(yintercept = 0.3)) +
#  geom_hline(aes(yintercept = 1)) +
#  geom_hline(aes(yintercept = 2)) +
#  geom_hline(aes(yintercept = 4)) +
#  ylim(0, 10) +
#  ggtitle("PRCP distribution for CHECK value weather stations vs state") +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#  theme_minimal()

# ask GPT to evaluate whether any of these areas should have higher than average intensity rainfall
#weather_station_lookup %>% 
#  filter(STATE == state) %>% 
#  filter(STNID %in% unique(c(plt_data1$id_col, plt_data2$id_col))) %>% 
#  .$NAME %>% 
#  paste0(., collapse = ", ")

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
#View(filter(prcp_check2, flag == "CHECK"))

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

# ---- GSOD peril data merge & data handling ----
gsod_peril1 <- gsod_peril1 %>% 
  select(-RH, -I_HAIL, -I_SNOW_ICE, -I_RAIN_DRIZZLE)

gsod_peril2 <- gsod_peril2 %>% 
  select(-RH, -I_HAIL, -I_SNOW_ICE, -I_RAIN_DRIZZLE)

gsod_peril <- gsod_peril1 %>% 
  rbind(gsod_peril2)
rm(list = c("gsod_peril1", "gsod_peril2")); gc()

# remove offshore weather stations
gsod_peril <- gsod_peril %>% 
  # remove offshore weather stations
  filter(STNID %in% onshore_only)

# adjust PRCP
gsod_peril <- gsod_peril %>% 
  mutate(yr = year(YEARMODA)) %>% 
  left_join(prcp_adjusted, by = c("STNID", "STATE", "yr")) %>% 
  mutate(PRCP = case_when(flag == "REBASE" ~ PRCP/10,
                        TRUE ~ PRCP)) %>% 
  select(-flag, -yr, -STATE)

gsod_peril %>% summary()

saveRDS(gsod_peril, "../data/lfs/gsod_peril.rds")


# ---- GSOD/solar farm mapping ----

# now let's create an index for complete and incomplete data by weather station
weather_station_completeness_index <- gsod_peril %>% 
  mutate(YEAR = year(YEARMODA)) %>% 
  group_by(STNID, YEAR) %>% 
  # ignore YEARMODA
  summarise(across(-c(1), ~sum(!is.na(.)))) %>% 
  ungroup() %>% 
  mutate(across(colnames(.)[-c(1, 2)], ~if_else(. >= 345, "COMPLETE", "INCOMPLETE")))
  
# join on state and concatenate station list
weather_station_completeness_index <- weather_station_completeness_index %>% 
  left_join(select(as_tibble(weather_station_lookup), STNID, STATE), by = "STNID") %>% 
  filter(!is.na(STATE))

# now let's map solar farms to the nearest COMPLETE weather station by year
# - solar farm latitude and longitude are calculated using AEA BUT are in degrees, so can compute distances directly
solar_farm_lookup <- us_solar %>% 
  filter(xlong > -160 & xlong < -66 & ylat > 18 & ylat < 50) %>% 
  select(case_id, p_name, p_year, p_state, p_county, ylat, xlong, p_cap_ac)

case_id = c(); STNID = c(); distance = c()
for (i in 1:nrow(solar_farm_lookup)) {
  
  dist_vctr <- distm(x = c(solar_farm_lookup$xlong[i], solar_farm_lookup$ylat[i]),
                    y = st_coordinates(weather_station_lookup$geometry),
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

solar_stnid_lookup_table %>% 
  ggplot() +
  geom_boxplot(aes(y = distance, x = PERIL, group = PERIL)) +
  theme_minimal() +
  labs(title = "Distribution of solar farm/weather station nearest complete node distances (km)") +
  ylim(0, 250)

# map data
us_map <- ne_states(country = "united states of america", returnclass = "sf") %>% 
  st_transform(crs = 4326)

# plot our weather station look-up on the map
plt <- ggplot() +
  geom_sf(data = us_map, fill = "gray95", color = "white") +
  geom_sf(data = filter(weather_station_lookup, STNID %in% unique(gsod_peril$STNID)),
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
  theme_minimal() +
  xlim(-160, -66) +
  ylim(18, 50)

plt <- ggplotly(plt)

htmlwidgets::saveWidget(plt, "../gsod_map/index.html", selfcontained = FALSE)
write_csv(weather_station_lookup, "../data/weather_station_lookup.csv")
write_csv(solar_farm_lookup, "../data/solar_farm_lookup.csv")
write_csv(solar_stnid_lookup_table, "../data/solar_stnid_lookup.csv")


# find 1-in-2 and 1-in-5 year triggers for each peril variable
vars = colnames(gsod_peril)[-c(1:2)]

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
write_csv(triggers, "../data/contract_triggers.csv")


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


# ---- NOAA Storm Events ----
file <- "StormEvents_details-ftp_v1.0_d1950_c20250401.csv"
noaa <- read_csv(paste0("../data/", file))

file <- "StormEvents_details-ftp_v1.0_d2020_c20240620.csv"
temp <- read_csv(paste0("../data/", file))

noaa <- noaa %>% 
  rbind(temp)

years <- c(1951:2019, 2021:2023)
for (year in years) {
  file <- sprintf("StormEvents_details-ftp_v1.0_d%d_c20250520.csv", year)
  temp <- read_csv(paste0("../data/", file))
  noaa <- noaa %>% 
    rbind(temp)
}

noaa %>% 
  group_by(EVENT_TYPE) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  View()

tz_lookup <- c(
  # Mainland U.S. timezones with DST
  "AST-4" = "America/Halifax",         # Atlantic
  "EST-5" = "America/New_York",        # Eastern
  "CST-6" = "America/Chicago",         # Central
  "MST-7" = "America/Denver",          # Mountain (with DST)
  "PST-8" = "America/Los_Angeles",     # Pacific (with DST)
  
  # Fixed offset timezones (no DST)
  "MST"    = "America/Phoenix",        # Arizona (Mountain without DST)
  "HST-10" = "Pacific/Honolulu",       # Hawaii (no DST)
  "AKST-9" = "America/Anchorage",      # Alaska Standard (with DST)
  "GST10"  = "Pacific/Guam",           # Guam
  "SST-11" = "Pacific/Pago_Pago",      # Samoa Standard
  
  # Additional recognized NOAA zone labels
  "AST-4"  = "America/Puerto_Rico",    # Atlantic (Puerto Rico, USVI, no DST)
  "CHST-10" = "Pacific/Guam",          # Chamorro (Guam, CNMI, no DST)
  "UTC"    = "UTC",                    # Coordinated Universal Time
  "GMT"    = "Etc/GMT",                # Greenwich Mean Time
  
  # Legacy/alternate codes used in some NOAA records
  "AST" = "America/Puerto_Rico",       # Atlantic (Puerto Rico, USVI, no DST)
  "EDT" = "America/New_York",          # Eastern Daylight Time
  "CDT" = "America/Chicago",           # Central Daylight Time
  "MDT" = "America/Denver",            # Mountain Daylight Time
  "PDT" = "America/Los_Angeles",       # Pacific Daylight Time
  "AKDT" = "America/Anchorage",        # Alaska Daylight Time
  "SST" = "Pacific/Pago_Pago",         # Samoa Standard
  "EDT-4" = "America/New_York",        # Eastern Daylight Time
  "CDT-5" = "America/Chicago",         # Central Daylight Time
  "PDT-7" = "America/Los_Angeles",     # Pacific Daylight Time
  
  # Slightly ambiguous codes
  "CST" = "America/Chicago",           # Central Daylight Time
  "CSt" = "America/Chicago",           # Central Daylight Time
  "EST" = "America/New_York",          # Eastern
  "ESt" = "America/New_York",          # Eastern
  "PST" = "America/Los_Angeles",       # Pacific (with DST)
  "AST" = "America/Puerto_Rico",       # Atlantic (Puerto Rico, USVI, no DST)
  "HST" = "Pacific/Honolulu",          # Hawaii (no DST)
  "UNK" = "America/Chicago",           # UNK is likely Unknown, set this to Chicago
  "CSC" = "America/Chicago",           # CSC is likely a typo for CST
  "SCT" = "America/Chicago",           # SCT is likely a typo for CST
  
  # Fallbacks for generic offset-style codes
  "UTC-5" = "Etc/GMT+5",               # Note: sign is inverted in Etc/*
  "UTC-6" = "Etc/GMT+6",
  "UTC-7" = "Etc/GMT+7",
  "UTC-8" = "Etc/GMT+8",
  "UTC-9" = "Etc/GMT+9",
  "UTC-10"= "Etc/GMT+10"
)

noaa <- noaa %>% 
  # parse datetime
  mutate(BEGIN_DATE_TIME_PARSED = ymd_hms(
           paste0(substr(BEGIN_YEARMONTH, 1, 4), "-", substr(BEGIN_YEARMONTH, 5, 6), "-",
                  BEGIN_DAY, "-", substr(BEGIN_DATE_TIME, 11, 18))), # YEARMONTH, DAY, TIME is more robust 
         END_DATE_TIME_PARSED = ymd_hms(
           paste0(substr(END_YEARMONTH, 1, 4), "-", substr(END_YEARMONTH, 5, 6), "-",
                  END_DAY, "-", substr(END_DATE_TIME, 11, 18))),
         TZONE = tz_lookup[CZ_TIMEZONE],
         BEGIN_DATE_TIME_PARSED = force_tzs(time = BEGIN_DATE_TIME_PARSED, 
                                            tzones = TZONE,
                                            tzone_out = "UTC"),
         END_DATE_TIME_PARSED = force_tzs(time = END_DATE_TIME_PARSED, 
                                            tzones = TZONE,
                                            tzone_out = "UTC"))


# ---- Hail ----
# let's cluster the reports into swaths, 
hail_sf <- noaa %>% 
  filter(EVENT_TYPE == "Hail") %>% 
  filter(!is.na(BEGIN_LAT) & !is.na(BEGIN_LON)) %>% 
  st_as_sf(coords = c("BEGIN_LON","BEGIN_LAT"),
                    crs = 4326) %>%
  st_transform(crs = 3857)

# maximum and minimum datetime parsed, number of NAs
min(hail_sf$BEGIN_DATE_TIME_PARSED, na.rm = TRUE); max(hail_sf$BEGIN_DATE_TIME_PARSED, na.rm = TRUE)
hail_sf %>% filter(is.na(BEGIN_DATE_TIME_PARSED)) %>% nrow()

# maximum distance and temporal swath radius
spatial_eps = 40*1000 # 40km in m
temporal_eps = 2*60*60 # assume 20km/h = 2 hour in sec
time_space_scale = spatial_eps/temporal_eps

# coordinate object
X = cbind(st_coordinates(hail_sf), as.numeric(hail_sf$BEGIN_DATE_TIME_PARSED) * time_space_scale)

# remove missing coordinates (broken coordinates that don't have a geometry mapping in CRS 3857)
rows_to_remove = unique(which(is.na(X), arr.ind = TRUE)[,1])

X_2 <- X[-rows_to_remove,]
hail_sf_2 <- hail_sf[-rows_to_remove,]

# clustering using dbscan
swaths <- dbscan(x = X_2, eps = spatial_eps, minPts = 3)

# swath ids
hail_sf_2$swath_id <- ifelse(
  swaths$cluster == 0,
  paste0("solo_", seq_len(sum(swaths$cluster == 0))),
  paste0("swath_", swaths$cluster)
)

rand <- hail_sf_2 %>% 
  filter(substr(swath_id, 1, 5) == "swath") %>%
  .$swath_id %>% 
  unique() %>% 
  sample(size = 100)

#  swath polygons --> plot
swath_polys <- hail_sf_2 %>%
  filter(swath_id %in% rand) %>% 
  group_by(swath_id) %>%
  summarise(
    n_reports = n(),
    geometry = st_combine(geometry) |> st_convex_hull()
  ) %>%
  ungroup()

us_map <- ne_states(country = "united states of america", returnclass = "sf") %>% 
  st_transform(crs = 3857)

plt <- ggplot() +
  geom_sf(data = us_map, fill = "gray80") +
  geom_sf(data = swath_polys, aes(fill = n_reports), color = "black", alpha = 0.4) +
  geom_sf(data = filter(hail_sf_2, swath_id %in% rand), color = "red", size = 0.5) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "DBSCAN-Detected Hail Swaths", fill = "# Reports")

ggplotly(plt)

# distribution of swath times and distances
st_crs(hail_sf_2)

max_dist_haversine <- function(geometry) {
  coords <- st_coordinates(geometry)
  
  # Compute convex hull
  hull_pts <- coords[chull(coords), ]
  
  # Compute pairwise haversine distances (in meters)
  max(distm(hull_pts, fun = distHaversine))
}

plt_data <- hail_sf_2 %>% 
  filter(substr(swath_id, 1, 5) == "swath") %>% 
  st_transform(crs = 4326) %>% 
  group_by(swath_id) %>% 
  summarise(max_dist_m = max_dist_haversine(geometry),
            max_time_m = (max(as.numeric(BEGIN_DATE_TIME_PARSED)) - min(as.numeric(BEGIN_DATE_TIME_PARSED)))/60)

plt_data %>% 
  mutate(max_dist_km = max_dist_m/1000) %>% 
  select(-max_dist_m) %>% 
  pivot_longer(cols = c("max_dist_km", "max_time_m"), names_to = "var", values_to = "value") %>%
  ggplot() +
  geom_boxplot(aes(y = value, group = var, x = var)) +
  theme_minimal() +
  labs(title = "Max swath distances along time and spatial axis")

# create final file for export
hail_swaths1 <- hail_sf_2 %>%
  filter(substr(swath_id, 1, 5) == "swath") %>% 
  group_by(swath_id) %>%
  summarise(
    nodes = n(),
    MIN_DATETIME = min(BEGIN_DATE_TIME_PARSED),
    MAX_DATETIME = max(BEGIN_DATE_TIME_PARSED),
    MAGNITUDE = max(MAGNITUDE),
    geometry = st_combine(geometry) |> st_convex_hull()) %>%
  ungroup()

hail_swaths2 <- hail_sf_2 %>%
  filter(substr(swath_id, 1, 5) != "swath") %>%   
  mutate(
    nodes = 1,
    MIN_DATETIME = BEGIN_DATE_TIME_PARSED,
    MAX_DATETIME = BEGIN_DATE_TIME_PARSED) %>% 
  select(swath_id, nodes, MIN_DATETIME, MAX_DATETIME, MAGNITUDE, geometry)

hail_swaths <- hail_swaths1 %>% 
  rbind(hail_swaths2)

saveRDS(hail_swaths, file = "../data/hail_swaths.rds")
rm(list = c("hail_sf", "hail_sf_2", "hail_swaths1", "hail_swaths2", 
            "plt_data", "swath_polys", "swaths", "us_map", "X", "X_2"))

# ---- Wildfire ----
fire <- noaa %>% 
  filter(EVENT_TYPE == "Wildfire")

# we will merge by county to solar lookup, so we need to ensure county names are fungible
names_noaa <- fire %>% 
  select(STATE, CZ_NAME) %>% 
  mutate(state = tolower(STATE),
         county = tolower(CZ_NAME)) %>% 
  distinct(state, county)

state_code_to_name <- function(code) {
  match_idx <- match(toupper(code), state.abb)
  state.name[match_idx]
}

names_sf <- us_solar %>% 
  select(p_state, p_county) %>% 
  mutate(state = tolower(state_code_to_name(p_state)),
         county = tolower(p_county)) %>% 
  distinct(state, county)

missing <- names_sf %>% 
  anti_join(names_noaa, by = c("state", "county"))

state = c(); county = c(); matching = c()
for (i in 1:nrow(missing)) {
  temp = grep(missing$county[i], filter(names_noaa, 
                                             state == missing$state[i])$county, value = TRUE)
  matching = c(matching, temp)
  state = c(state, rep(missing$state[i], length(temp)))
  county = c(county, rep(missing$county[i], length(temp)))
}

grep_search <- tibble(state, county, matching)

# select rows to remove from grep_search
to_remove = c(1, 2, 10, 11, 12, 14, 15, 32, 33, 34, 35, 75, 79)

grep_search_final <- grep_search[-to_remove,]

# search mapping for export
noaa_county_mapping <- names_sf %>% 
  semi_join(names_noaa, by = c("state", "county")) %>% 
  mutate(matching = county) %>% 
  rbind(grep_search_final)

saveRDS(noaa_county_mapping, file = "../data/noaa_county_mapping.rds")
saveRDS(fire, file = "../data/fire_events.rds")

rm(list = c("grep_search", "grep_search_final", "missing", "names_noaa", "names_sf", "noaa"))


# ---- Hurricane ----
# Grab Atlantic and Pacific HURDAT2 data from https://www.nhc.noaa.gov/data/ and read it in
atlantic <- read_fwf("../data/hurdat2-1851-2024-040425.txt")
pacific <- read_fwf("../data/hurdat2-nepac-1949-2024-031725.txt")

hurricane <- atlantic %>% 
  rbind(pacific)

# requires a bit of cleaning - let's pull hurricane IDs from the left-most row, make sure there are no duplicate sets
hurricane %>% 
  filter(is.na(X2)) %>% 
  group_by(X1) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  View()

# clean hurricane
hurricane <- hurricane %>% 
  # get rid of header rows and assign swath ids to each hurricane
  mutate(ID = if_else(is.na(X2), X1, NA),
         is_header = !is.na(ID),
         swath_id = cumsum(is_header)) %>% 
  filter(is_header == FALSE) %>% 
  select(-ID, -is_header) %>% 
  # remove commas from values
  mutate(across(everything(), ~ gsub(",", "", .))) %>% 
  # create datetime
  mutate(date_time = ymd_hm(paste0(X1, " ", X2))) %>% 
  # parse latitude and system status
  mutate(status = substr(X4, 1, 2),
         lat = substr(X4, 4, 1000)) %>% 
  select(swath_id, date_time, status, lat, X5, everything()) %>% 
  select(-X1, -X2, -X4)

# grab colnames from metadata https://www.nhc.noaa.gov/data/hurdat/hurdat2-format-atl-1851-2021.pdf
colnames(hurricane) <- c("swath_id", "date_time", "status", "lat", "lon",
                         "record_identifier", "max_sustained_speed", "min_pressure",
                         "ne_radii_34kt", "se_radii_34kt", "sw_radii_34kt", "nw_radii_34kt",
                         "ne_radii_50kt", "se_radii_50kt", "sw_radii_50kt", "nw_radii_50kt",
                         "ne_radii_64kt", "se_radii_64kt", "sw_radii_64kt", "nw_radii_64kt",
                         "max_radii")

# unit changes
hurricane <- hurricane %>%
  # convert nautical miles to km
  mutate(across(min_pressure:max_radii, ~ if_else(. == "-999", NA, as.numeric(.)))) %>% 
  mutate(across(ne_radii_34kt:max_radii, ~ .*1.852)) %>% 
  # convert lat and lon to decimal degrees
  mutate(lat = case_when(substr(lat, nchar(lat), nchar(lat)) == "S" ~ as.numeric(substr(lat, 1, nchar(lat)-1)) * - 1,
                         TRUE ~ as.numeric(substr(lat, 1, nchar(lat)-1))),
         lon = case_when(substr(lon, nchar(lon), nchar(lon)) == "W" ~ as.numeric(substr(lon, 1, nchar(lon)-1)) * - 1,
                         TRUE ~ as.numeric(substr(lon, 1, nchar(lon)-1))))

# let's look at the proportion of NAs of radii column
hurricane %>% 
  select(min_pressure:max_radii) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "value") %>% 
  group_by(var) %>% 
  summarise(n = n(),
            n_nas = sum(is.na(value)),
            perc_nas = n_nas/n) %>% 
  ungroup() %>% 
  select(var, perc_nas) %>% 
  pivot_wider(names_from = var, values_from = perc_nas) %>% 
  View()

# proportion of 0s of radii column
hurricane %>% 
  select(min_pressure:max_radii) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "value") %>% 
  group_by(var) %>% 
  summarise(n = n(),
            n_0s = sum(value == 0, na.rm = TRUE),
            perc_0s = n_0s/n) %>% 
  ungroup() %>% 
  select(var, perc_0s) %>% 
  pivot_wider(names_from = var, values_from = perc_0s) %>% 
  View()

# impute 0s as NAs which are quite common but not likely in quandrant radii columns
hurricane <- hurricane %>% 
  mutate(across(ne_radii_34kt:nw_radii_64kt, ~ na_if(., 0)))

# now let's find system level and swath means for radii -- we are only interested in 64kt
radii_64kt_means <- hurricane %>% 
  select(status, ne_radii_64kt:nw_radii_64kt) %>% 
  pivot_longer(cols = ne_radii_64kt:nw_radii_64kt, names_to = "var", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  group_by(status) %>% 
  summarise(n = n(),
            ave = mean(value),
            min = min(value),
            max = max(value)) %>% 
  ungroup()

radii_64kt_swath_means <- hurricane %>% 
  select(swath_id, ne_radii_64kt:nw_radii_64kt) %>% 
  pivot_longer(cols = ne_radii_64kt:nw_radii_64kt, names_to = "var", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  group_by(swath_id) %>% 
  summarise(ave = mean(value)) %>% 
  ungroup()

# (sub)tropical cyclone of (sub)tropical depression intensity (SD/TD) are below 34kt by definition - we remove these
# also remove WV, DB which aren't well defined at 50kt
hurricane <- hurricane %>% 
  filter(status %in% c("HU", "EX", "LO")) %>%
  select(-c(ne_radii_34kt:nw_radii_50kt))

# replace radii NAs
hurricane <- hurricane %>%
  # 1: take tangential radii values for each observation
  mutate(ne_radii_64kt = case_when(!is.na(ne_radii_64kt) ~ ne_radii_64kt,
                                   !is.na(nw_radii_64kt) & !is.na(se_radii_64kt) ~ 
                                     rowMeans(across(c(nw_radii_64kt, se_radii_64kt))),
                                   TRUE ~ NA),
         se_radii_64kt = case_when(!is.na(se_radii_64kt) ~ se_radii_64kt,
                                   !is.na(ne_radii_64kt) & !is.na(sw_radii_64kt) ~ 
                                     rowMeans(across(c(ne_radii_64kt, sw_radii_64kt))),
                                   TRUE ~ NA),
         sw_radii_64kt = case_when(!is.na(sw_radii_64kt) ~ sw_radii_64kt,
                                   !is.na(se_radii_64kt) & !is.na(nw_radii_64kt) ~ 
                                     rowMeans(across(c(se_radii_64kt, nw_radii_64kt))),
                                   TRUE ~ NA),
         nw_radii_64kt = case_when(!is.na(nw_radii_64kt) ~ nw_radii_64kt,
                                   !is.na(sw_radii_64kt) & !is.na(ne_radii_64kt) ~ 
                                     rowMeans(across(c(sw_radii_64kt, ne_radii_64kt))),
                                   TRUE ~ NA)) %>%
  # 2: take a mean of any row-wise values available
  mutate(ne_radii_64kt = case_when(is.na(ne_radii_64kt) ~ 
                                     rowMeans(across(c(se_radii_64kt, sw_radii_64kt, nw_radii_64kt)), na.rm = TRUE),
                                   TRUE ~ ne_radii_64kt),
         se_radii_64kt = case_when(is.na(se_radii_64kt) ~ 
                                    rowMeans(across(c(ne_radii_64kt, sw_radii_64kt, nw_radii_64kt)), na.rm = TRUE),
                                   TRUE ~ se_radii_64kt),
         sw_radii_64kt = case_when(is.na(sw_radii_64kt) ~
                                    rowMeans(across(c(ne_radii_64kt, se_radii_64kt, nw_radii_64kt)), na.rm = TRUE),
                                   TRUE ~ sw_radii_64kt),
         nw_radii_64kt = case_when(is.na(nw_radii_64kt) ~
                                    rowMeans(across(c(ne_radii_64kt, se_radii_64kt, sw_radii_64kt)), na.rm = TRUE),
                                   TRUE ~ nw_radii_64kt)) %>% 
  # 3: join on swath level means
  ungroup() %>% 
  left_join(radii_64kt_swath_means, by = "swath_id") %>% 
  mutate(across(ne_radii_64kt:nw_radii_64kt, ~ coalesce(., ave))) %>% 
  select(-ave) %>% 
  # 4: join on system level means
  left_join(select(radii_64kt_means, status, ave), by = "status") %>% 
  mutate(across(ne_radii_64kt:nw_radii_64kt, ~ coalesce(., ave))) %>% 
  select(-ave)

# create a point for each radii adjustment
hurricane_sf <- hurricane %>% 
  pivot_longer(cols = ne_radii_64kt:nw_radii_64kt, names_to = "quadrant", values_to = "radii") %>% 
  mutate(quadrant = substr(quadrant, 1, 2),
         radii = radii * 1000,
         bearing = case_when(quadrant == "ne" ~ 45,
                             quadrant == "se" ~ 135,
                             quadrant == "sw" ~ 225,
                             quadrant == "nw" ~ 315))

lon_new = c(); lat_new = c()
for (i in 1:nrow(hurricane_sf)) {
  temp = destPoint(p = c(hurricane_sf$lon[i], 
                         hurricane_sf$lat[i]), 
                   b = hurricane_sf$bearing[i], 
                   d = hurricane_sf$radii[i])
  
  lon_new = c(lon_new, temp[1])
  lat_new = c(lat_new, temp[2])
}

hurricane_sf$lat_shift <- lat_new
hurricane_sf$lon_shift <- lon_new

# create convex hull
hurricane_sf <- hurricane_sf %>% 
  select(-lat, -lon) %>% 
  rename("lat" = lat_shift, "lon" = lon_shift) %>% 
  rbind(
    mutate(
      distinct(
        select(hurricane_sf, -lat_shift, -lon_shift, -quadrant, -radii, -bearing)
      ),
      quadrant = "",
      radii = 0,
      bearing = 0
      )
    ) %>% 
  arrange(swath_id, date_time) %>% 
  st_as_sf(coords = c("lon","lat"),
           crs = 4326) %>%
  st_transform(crs = 3857)

hurricane_swaths <- hurricane_sf %>% 
  group_by(swath_id) %>% 
  summarise(begin_datetime = min(date_time),
         end_datetime = max(date_time),
         geometry = st_combine(geometry) |> st_convex_hull()) %>% 
  ungroup()

# plot some of the hurricane swaths
us_map <- ne_states(country = "united states of america", returnclass = "sf") %>% 
  st_transform(crs = 3857)

rand_swaths <- sample(1500:max(hurricane$swath_id), size = 15) # visualise recent hurricanes

plt_data1 <- hurricane_swaths %>% 
  filter(swath_id %in% rand_swaths) %>% 
  mutate(swath_id = as.factor(swath_id))

plt_data2 <- hurricane_sf %>% 
  filter(swath_id %in% rand_swaths & 
           quadrant == "") %>% 
  mutate(swath_id = as.factor(swath_id))

plt <- ggplot() +
  geom_sf(data = us_map, fill = "gray80") +
  geom_sf(data = plt_data1, aes(color = swath_id), alpha = 0.4) +
  geom_sf(data = plt_data2, aes(color = swath_id), size = 0.5) +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "HURDAT2 Hurricane/Cyclone Swaths")

ggplotly(plt)

# export
saveRDS(hurricane_swaths, file = "../data/hurricane_swaths.rds")
rm(list = c("pacific", "atlantic", "hurricane", "radii_64kt_means", "radii_64kt_swath_means",
            "lat_new", "lon_new","plt_data1", "plt_data2", "plt", "us_map"))
