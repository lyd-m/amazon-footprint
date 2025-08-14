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

# totals for each country_commodity

totals_usd_m <- tibble()
for (country_commodity in names(flows)){
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) {
    result <- df %>% 
      group_by(producer_country, commodity) %>%
      summarise(total_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm=TRUE))
    
    totals_usd_m <- bind_rows(totals_usd_m, result)
  } else print("Empty df")
}

write_csv(totals_usd_m, "./analytical-results/flows_total_per_country_commodity.csv")

## 2. direct / indirect -------------------

### Issued in producer country? ----------------

# get continental regions for case studies, matching the flows data
regions_unsd <- read_csv('./input-data/regions_map.csv') %>%
  select(country, region_unsd)

# add producer regions
producer_regions <- tibble(
  "country" = c("Brazil", "Bolivia", "Colombia", "Peru", "Ecuador"),
  "producer_region" = rep("Latin America and the Caribbean", 5)
)

# adding classification of where flows are issued
flows_by_issuance_location_type <- tibble()
for (country_commodity in names(flows)) {
  print(country_commodity)
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) { # skip empty data (Brazil coffee, Brazil maize)
    df <- df %>%
      # add producer_region (LatAm)
      left_join(producer_regions, by = c("producer_country" = "country")) %>%
      # reclassify issuer regions in line with unsd
      left_join(regions_unsd, by = c("borrower_issuer_country" = "country")) %>%
      rename(borrower_issuer_region_unsd = region_unsd) %>%
      mutate(flow_issuance_location_type = case_when(
        producer_country == borrower_issuer_country ~ "Domestic",
        producer_region == borrower_issuer_region_unsd & producer_country != borrower_issuer_country ~ "Regional",
        TRUE ~ "International"
      ))
    
    flows[[country_commodity]] <- df # add adjusted data back to df
    
    result <- df %>%
      group_by(flow_issuance_location_type) %>%
      summarise(amount_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(pct = percent(amount_usd_m/sum(amount_usd_m), accuracy = 0.01)) %>%
      as_tibble() %>% 
      arrange(factor(flow_issuance_location_type, levels = c("Domestic", "Regional", "International"))) %>%
      mutate(case = paste0(country_commodity))
    
    print(result, n = Inf) 
    
    flows_by_issuance_location_type <- bind_rows(flows_by_issuance_location_type, result)
    
  } else print("Empty df")
}

write_csv(flows_by_issuance_location_type, "./analytical-results/flows_by_issuance_location_type.csv")

### Financed in producer country? ----------------
# adding classification of where flows are financed from

flows_by_financed_location_type <- tibble()
for (country_commodity in names(flows)) {
  print(country_commodity)
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) { # skip empty data (Brazil coffee, Brazil maize)
    df <- df %>%
      # reclassify financier regions in line with unsd
      left_join(regions_unsd, by = c("manager_true_ultimate_parent_country_of_headquarters" = "country")) %>%
      select(-manager_true_ultimate_parent_region_of_headquarters_unsd) %>%
      rename(manager_true_ultimate_parent_region_of_headquarters_unsd = region_unsd) %>%
      mutate(flow_financed_location_type = case_when(
        producer_country == manager_true_ultimate_parent_country_of_headquarters ~ "Domestic",
        producer_region == manager_true_ultimate_parent_region_of_headquarters_unsd & producer_country != manager_true_ultimate_parent_country_of_headquarters ~ "Regional",
        TRUE ~ "International"
      ))
    
    flows[[country_commodity]] <- df # add adjusted data back to df
    
    result <- df %>%
      group_by(flow_financed_location_type) %>%
      summarise(amount_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(pct = percent(amount_usd_m/sum(amount_usd_m), accuracy = 0.01)) %>%
      as_tibble() %>% 
      arrange(factor(flow_financed_location_type, levels = c("Domestic", "Regional", "International"))) %>%
      mutate(case = paste0(country_commodity))
    
    print(result, n = Inf) 
    
    flows_by_financed_location_type <- bind_rows(flows_by_financed_location_type, result)
    
  } else print("Empty df")
}

write_csv(flows_by_financed_location_type, "./analytical-results/flows_by_issuance_location_type.csv")


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
## these were then manually assigned as 'direct', 'indirect', and 'minimally-related'

trbc_activities_classified <- read_excel("./intermediate-results/trbc_activities_classification_final.xlsx") %>%
  select(-Notes)

## 3. exclusion SEI-Trase/DeDuCE data, all dates -------------------

## 3. exclusion SEI-Trase/DeDuCE data, appearance (i.e., phase-in / phase-out) -------------------

