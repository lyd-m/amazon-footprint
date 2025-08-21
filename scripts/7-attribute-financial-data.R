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
library(janitor)
library(purrr)
library(stringr)
library(lubridate)
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

companies_yearly <- read_csv("./analytical-results/companies_all_years.csv") %>%
  select(-1)

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

write_csv(totals_usd_m, "./analytical-results/flows_total_2008_2024_per_country_commodity.csv")

## 2. Boundary periods -------------------

### 2.1 Simple SEI-Trase/DeDuCE data boundary periods +/- threshold -------------------
# i.e., not accounting for phase out
# call 'simple' since just use the exact SEI-Trase dates
sei_trase_dates_available <- 
  companies_yearly %>% 
  distinct(producer_country, commodity, years_available) %>%
  separate_wider_delim(years_available, delim = "-", names = c("sei_trase_data_min_year", "sei_trase_data_max_year")) %>%
  mutate(sei_trase_data_min_year = as.numeric(sei_trase_data_min_year),
         sei_trase_data_max_year = as.numeric(sei_trase_data_max_year))

for (country_commodity in names(flows)) {
  print(country_commodity)
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) {
    df <- df %>%
      # join on years available for country_commodity
      left_join(sei_trase_dates_available, 
                by = c("producer_country", "commodity")) %>%
      rowwise() %>%
      # add column marking boundary period
      mutate(year_relative_to_trase_period = case_when(
        year >= sei_trase_data_min_year & year <= sei_trase_data_max_year ~ 0,
        year < sei_trase_data_min_year ~ year - sei_trase_data_min_year, 
        year > sei_trase_data_max_year ~ year - sei_trase_data_max_year
      ))
    
    flows[[country_commodity]] <- df
  } else print(" Empty df")
}

### 2.2 SEI-Trase/DeDuCE data boundary periods, within country-commodities exclude based on pattern (i.e., phase-in / phase-out) -------------------
# discuss with colleagues before doing

for (country_commodity in names(flows)) {
  print(country_commodity)
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) {
    df <- df %>%
      # join on years available for country_commodity
      rowwise() %>%
      mutate(
        deforestation_phase_out = !is.na(phase_out_year_consolidated) & phase_out_year_consolidated < sei_trase_data_max_year,
        # tag to remove flow if entity phased out of Amazon deforestation and the financing year is after that phase out yaer
        remove_flow_based_on_phase_out = deforestation_phase_out & year > phase_out_year_consolidated) 
    
    flows[[country_commodity]] <- df
  } else print(" Empty df")
}

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

write_csv(flows_by_financed_location_type, "./analytical-results/flows_by_financed_location_type.csv")


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

trbc_activities_classified_v1 <- read_excel("./intermediate-results/trbc_activities_classification_final.xlsx",
                                            sheet = "v1_15_August_25") %>%
  select(-notes)

trbc_activities_classified_v2 <- read_excel("./intermediate-results/trbc_activities_classification_final.xlsx",
                                            sheet = "v2_20_August_25") %>%
  select(-notes)

flows_by_trbc_activity_strength_classification <- tibble()
for (country_commodity in names(flows)) {
  print(country_commodity)
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) { # skip empty data (Brazil coffee, Brazil maize)
    df <- df %>%
      # reclassify financier regions in line with unsd
      left_join(trbc_activities_classified_v2, by = c( # can change between v1 and v2 here
        "producer_country",
        "commodity",
        "borrower_issuer_trbc_activity"
        )) %>%
      rename(borrower_issuer_trbc_activity_strength_classification = direct_indirect_classification)
    
    flows[[country_commodity]] <- df # add adjusted data back to df
    
    result <- df %>%
      group_by(borrower_issuer_trbc_activity_strength_classification) %>%
      summarise(amount_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(pct = percent(amount_usd_m/sum(amount_usd_m), accuracy = 0.01)) %>%
      as_tibble() %>% 
      arrange(factor(borrower_issuer_trbc_activity_strength_classification, levels = c("Direct", "Indirect", "Minimally related"))) %>%
      mutate(case = paste0(country_commodity))
    
    print(result, n = Inf) 
    
    flows_by_trbc_activity_strength_classification <- bind_rows(flows_by_trbc_activity_strength_classification, result)
    
  } else print("Empty df")
}
# remember to write version of classification in the file name
write_csv(flows_by_trbc_activity_strength_classification, "./analytical-results/flows_by_trbc_activity_strength_classification_v2.csv")

### Combining country and sector issued in to classify flows as low, medium, high ----------------
flows_by_link_strength <- tibble()
for (country_commodity in names(flows)) {
  print(country_commodity)
  df <- flows[[country_commodity]]
  if (nrow(df) > 0) { # skip empty data (Brazil coffee, Brazil maize)
    df <- df %>%
      # reclassify financier regions in line with unsd
      mutate(financial_flow_link_strength = case_when(
        flow_issuance_location_type == "Domestic" & borrower_issuer_trbc_activity_strength_classification == "Direct" ~ "High",
        flow_issuance_location_type == "Domestic" & borrower_issuer_trbc_activity_strength_classification == "Indirect" ~ "High",
        flow_issuance_location_type == "Regional" & borrower_issuer_trbc_activity_strength_classification == "Direct" ~ "High",
        flow_issuance_location_type == "Domestic" & borrower_issuer_trbc_activity_strength_classification == "Minimally related" ~ "Medium",
        flow_issuance_location_type == "Regional" & borrower_issuer_trbc_activity_strength_classification == "Indirect" ~ "Medium",
        flow_issuance_location_type == "International" & borrower_issuer_trbc_activity_strength_classification == "Direct" ~ "Medium",
        flow_issuance_location_type == "Regional" & borrower_issuer_trbc_activity_strength_classification == "Minimally related" ~ "Low",
        flow_issuance_location_type == "International" & borrower_issuer_trbc_activity_strength_classification == "Indirect" ~ "Low",
        flow_issuance_location_type == "International" & borrower_issuer_trbc_activity_strength_classification == "Minimally related" ~ "Low"
      ))
    
  
    flows[[country_commodity]] <- df # add adjusted data back to df
    
    result <- df %>%
      group_by(financial_flow_link_strength) %>%
      summarise(amount_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(pct = percent(amount_usd_m/sum(amount_usd_m), accuracy = 0.01)) %>%
      as_tibble() %>% 
      arrange(factor(financial_flow_link_strength, levels = c("High", "Medium", "Low"))) %>%
      mutate(case = paste0(country_commodity))
    
    print(result, n = Inf) 
    
    flows_by_link_strength <- bind_rows(flows_by_link_strength, result)
    
  } else print("Empty df")
}

write_csv(flows_by_link_strength, "./analytical-results/flows_by_link_strength_v2.csv")



