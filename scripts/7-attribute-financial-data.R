# AMAZON FOOTPRINT IN FOOD AND FINANCIAL SYSTEMS --------
# Financial flows analysis
# Stage 7 - Attributing financial data --------
# github.com/lyd-m/amazon-footprint

# This project contains the financial analysis for the paper, Singh et al. (2025)
# This script:
## Takes the clean 2008-2024 data per country-commodity from the previous script
## Sensitivity tests different ways of classifying flows as direct/indirect
## And sets up how to exclude companies that have phased out of the Amazon etc

## prerequisites ------------------------------
rm(list = ls())

## packages  ------------------------------
library(tidyverse)
library(readxl)
library(fuzzyjoin)
library(gridExtra)
library(janitor)
library(purrr)
library(RColorBrewer)
library(stringr)
library(lubridate)
library(stringr)
library(scales)

## 1. import data --------------------

flows_by_manager_files <- list.files("./intermediate-results",
                    pattern = "flows_clean_manager_level")

flows <- list()
for (file_ in flows_by_manager_files) {
  var_name <- str_replace(file_, "flows_clean_manager_level_2008-2024_", "")
  var_name <- str_replace(var_name, "\\(.*","")
  var_name <- str_replace(var_name, ".csv","")
  
  df <- read_csv(paste0("./intermediate-results/",file_)) %>%
    clean_names()
  
  flows[[var_name]] <- df
}

# separate out the combined data
flows_all_amazon <- flows[["flows_clean_manager_level_2008_2024_all_amazon"]]
flows[["flows_clean_manager_level_2008_2024_all_amazon"]] <- NULL

## 2. direct / indirect -------------------

### Issued in producer country? ----------------
for (country_commodity in names(flows)) {
  print(country_commodity)
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) {
    result <- df %>%
      mutate(issued_in_producer_country = if_else(
        producer_country == borrower_issuer_country,
        "Issued in producer country",
        "Issued outside producer country"
      )) %>%
      group_by(issued_in_producer_country) %>%
      summarise(amount_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(pct = percent(amount_usd_m/sum(amount_usd_m), accuracy = 0.01)) %>%
      as_tibble()
    
    print(result, n = Inf) 
    
  } else print("Empty df")
}

### Sector issued in ----------------
trbc_activities <- tibble()
for (country_commodity in names(flows)) {
  print(country_commodity)
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) {
    result <- df %>%
      group_by(producer_country, commodity, borrower_issuer_trbc_activity) %>%
      summarise(amount_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(pct = percent(amount_usd_m/sum(amount_usd_m), accuracy = 0.01)) %>%
      as_tibble() %>%
      arrange(desc(amount_usd_m))
    
    print(result, n = Inf) 
    
    trbc_activities <- bind_rows(trbc_activities,
                                 result %>%  select(producer_country, commodity, borrower_issuer_trbc_activity))
    
  } else print("Empty df")
}

write_csv(trbc_activities, "./intermediate-results/trbc_activities_classification.csv")

## 3. exclusion around appearance in SEI-Trase/DeDuCE data -------------------

