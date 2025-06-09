#install.packages(c("tidyquant"))
#remotes::install_github("ropensci/GSODR")

# load required packages
library(tidyquant); library(GSODR); library(tidyverse)

# financial data
eth_usd <- tq_get("ETH-USD", get = "stock.prices", from = "2020-12-20", to = "2024-12-31")
treasury <- tq_get("DGS1", get = "economic.data", from = "2020-12-20", to = "2024-12-31")
sp500 <- tq_get("^GSPC", get = "stock.prices", from = "2020-12-20", to = "2024-12-31") %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "sp500_ret")

prices <- eth_usd %>%
  select(date, close) %>% 
  rename("eth_usd" = "close") %>% 
  left_join(sp500,
    by = "date") %>% 
  left_join(
    select(treasury, date, price),
    by = "date") %>% 
  rename("rf" = "price") %>% 
  select(date, sp500_ret, rf, eth_usd) %>% 
  mutate(rf_ret = replace_na(rf, 0)/252/100,
         sp500_ret = replace_na(sp500_ret, 0)) %>% 
  select(-rf) %>% 
  tail(nrow(.) - 1)

saveRDS(prices, "financial_data.rds")

# US GSOD
#us_gsod <- tibble()
#for (i in 1986:1987) {
#  temp <- get_GSOD(years = i, country = "UNITED STATES")
#  us_gsod <- us_gsod %>% rbind(temp)
#  rm(temp)
#}
#saveRDS(gsod_full2, "../data/GSOD_1984-1995.rds")#

gsod_full1 <- readRDS("../data/GSOD_1943-1984.rds")
gsod_full2 <- readRDS("../data/GSOD_1985-1995.rds")

gsod_peril1 <- gsod_full1 %>%
  select("STNID", "STATE", "YEARMODA",
         "GUST", "MXSPD", "PRCP", "MAX", "TEMP", "DEWP", 
         "RH", "I_RAIN_DRIZZLE", "I_HAIL", "I_SNOW_ICE")
gsod_attributes1 <- gsod_full1 %>%
  distinct(STNID, CTRY, STATE, NAME,
         LATITUDE, LONGITUDE, ELEVATION, BEGIN, END)

gsod_peril2 <- gsod_full2 %>%
  select("STNID", "STATE", "YEARMODA",
         "GUST", "MXSPD", "PRCP", "MAX", "TEMP", "DEWP",
         "RH", "I_RAIN_DRIZZLE", "I_HAIL", "I_SNOW_ICE")
gsod_attributes2 <- gsod_full2 %>%
  distinct(STNID, CTRY, STATE, NAME,
           LATITUDE, LONGITUDE, ELEVATION, BEGIN, END)

rm(gsod_full1); rm(gsod_full2)

gsod_full3 <- readRDS("../data/GSOD_1996-2007.rds")
gsod_full4 <- readRDS("../data/GSOD_2008-2017.rds")
gsod_full5 <- readRDS("../data/GSOD_2018-2023.rds")

gsod_peril3 <- gsod_full3 %>%
  select("STNID", "STATE", "YEARMODA",
         "GUST", "MXSPD", "PRCP", "MAX", "TEMP", "DEWP",
         "RH", "I_RAIN_DRIZZLE", "I_HAIL", "I_SNOW_ICE")
gsod_attributes3 <- gsod_full3 %>%
  distinct(STNID, CTRY, STATE, NAME,
           LATITUDE, LONGITUDE, ELEVATION, BEGIN, END)

gsod_peril4 <- gsod_full4 %>%
  select("STNID", "STATE", "YEARMODA",
         "GUST", "MXSPD", "PRCP", "MAX", "TEMP", "DEWP", 
         "RH", "I_RAIN_DRIZZLE", "I_HAIL", "I_SNOW_ICE")
gsod_attributes4 <- gsod_full4 %>%
  distinct(STNID, CTRY, STATE, NAME,
           LATITUDE, LONGITUDE, ELEVATION, BEGIN, END)

gsod_peril5 <- gsod_full5 %>%
  select("STNID", "STATE", "YEARMODA",
         "GUST", "MXSPD", "PRCP", "MAX", "TEMP", "DEWP", 
         "RH", "I_RAIN_DRIZZLE", "I_HAIL", "I_SNOW_ICE")
gsod_attributes5 <- gsod_full5 %>%
  distinct(STNID, CTRY, STATE, NAME,
           LATITUDE, LONGITUDE, ELEVATION, BEGIN, END)

rm(gsod_full3); rm(gsod_full4); rm(gsod_full5)

gsod_attributes <- gsod_attributes1 %>% 
  rbind(gsod_attributes2) %>% 
  rbind(gsod_attributes3) %>% 
  rbind(gsod_attributes4) %>% 
  rbind(gsod_attributes5) %>% 
  distinct()

rm(gsod_attributes1); rm(gsod_attributes2); rm(gsod_attributes3); rm(gsod_attributes4); rm(gsod_attributes5)
saveRDS(gsod_attributes, "../data/GSOD_attributes.rds")


gsod_peril <- gsod_peril1 %>% 
  rbind(gsod_peril2)

rm(gsod_peril1); rm(gsod_peril2)
saveRDS(gsod_peril, "../data/GSOD_peril_1943-1995.rds")
rm(gsod_peril)

gsod_peril <- gsod_peril3 %>% 
  rbind(gsod_peril4)
rm(gsod_peril3); rm(gsod_peril4)

gsod_peril <- gsod_peril %>%
  rbind(gsod_peril5)
rm(gsod_peril5)
saveRDS(gsod_peril, "../data/GSOD_peril_1996-2023.rds")
