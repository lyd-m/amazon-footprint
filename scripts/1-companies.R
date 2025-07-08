# AMAZON FOOTPRINT IN FOOD AND FINANCIAL SYSTEMS --------
# Financial flows analysis: Stage 1 - Company identification -------

# This project contains the financial analysis for the paper, Singh et al. (2025)
# This script takes data from the combination DeDuCE and Trase data to identify a company list.

## 1 - Dependencies --------
library(tidyverse)
library(janitor)
library(readxl)
library(stringr)
library(glue)

## 2 - Functions --------
to_snake_case <- function(str) {
  snake_case_string <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", str, perl = TRUE)
  snake_case_string <- gsub(" ", "_", tolower(snake_case_string))
  return(snake_case_string)
}

## 3 - Data import --------

files <- list.files("./input-data",
                    pattern = "DeDuCE")

deduce_data <- list()
for (file_ in files) {
  var_name <- str_replace(file_, "DeDuCE_Trase_", "")
  var_name <- str_replace(var_name, "\\(.*","")
  var_name <- to_snake_case(var_name)
  var_name <- str_replace(var_name,"_(?!.*_)", "")
  
  df <- read_csv(paste0("./input-data/",file_)) %>%
    clean_names() %>%
    rename(id = 1)
  
  deduce_data[[var_name]] <- df
}

# checking which years appear in DeDuCE data

country_commodity_years <- tibble()
for (name in names(deduce_data)) {
  df <- deduce_data[[name]]
  
  df <- df %>%
    filter(year >= 2010) %>% # ensuring only have from 2010 onwards in line with our methodology
    group_by(producer_country, commodity) %>%
    mutate(min_year = min(year),
           max_year = max(year)) %>%
    distinct(producer_country, commodity, min_year, max_year) %>%
    ungroup() %>%
    mutate(year_range = glue("{min_year}-{max_year}")) %>%
    select(-min_year, -max_year)
  
  print(name)
  print(df)
  
  country_commodity_years <- bind_rows(country_commodity_years, df)
}

## 4 - Company lists --------
# Focusing on companies with >50 ha of deforestation from 2010 onwards. (considering unique combinations of ‘Producer country’, ‘Year’, ‘Commodity’, and ‘Exporter group’). This gives us a list to about 516 companies.

### 4.1 yearly vs cumulative vs average --------
companies_yearly_50ha <- list() # companies with a minimum >50 ha in any given year, from 2010 onwards
for (df_name in names(deduce_data)) {
  df <- deduce_data[[df_name]]
  
  df <- df %>%
    filter(year >= 2010) %>%
    group_by(producer_country, 
             commodity, 
             year, 
             exporter_group) %>%
    summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE), .groups = "drop") %>%
    filter(adjusted_deforestation_exposure >= 50)
  
  companies_yearly_50ha[[df_name]] <- df

}

companies_yearly_50ha <- plyr::ldply(companies_yearly_50ha, data.frame, .id="DeDuCE_output")

companies_cumulative_50ha <- list() # companies from 2010 onwards, with a minimum of 50ha cumulatively
for (df_name in names(deduce_data)) {
  df <- deduce_data[[df_name]]
  
  df <- df %>%
    filter(year >= 2010) %>% # 2010 onwards
    group_by(producer_country, 
             commodity,  
             exporter_group) %>%
    summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE), .groups = "drop") %>%
    filter(adjusted_deforestation_exposure >= 50)
  
  companies_cumulative_50ha[[df_name]] <- df
  
}

companies_cumulative_50ha <- plyr::ldply(companies_cumulative_50ha, data.frame, .id="DeDuCE_output")

### comparing number of distinct companies in each method
length(unique(companies_yearly_50ha$exporter_group))  
length(unique(companies_cumulative_50ha$exporter_group)) 

companies_yearly_50ha %>%
  group_by(producer_country, commodity) %>%
  summarise(company_count = n_distinct(exporter_group)) %>% View()

## 5 - Filtered DeDuCE dataset for companies for fin flows analysis --------

# New dataset so can understand attribution extent for financial flows analysis (i.e., what % of ha of identified companies had financial flows)
deduce_data_selective <- list()
for (name in names(deduce_data)) {
  df <- deduce_data[[name]]
  
  print(glue("{name}\nNumber of companies in original: {length(unique(df$exporter_group))}")) # check number of companies
  
  company_list <- companies_yearly_50ha %>% # distinct companies per commodity for this selection
    filter(DeDuCE_output == name) %>%
    distinct(commodity, exporter_group)
  
  df <- df %>%
    filter(year >= 2010) %>%
    semi_join(company_list, by = c("commodity", "exporter_group")) # filter out companies not included via >50ha filter
  
  print(glue("Number of companies in filtered: {length(unique(df$exporter_group))}"))  # check number of companies
  
  deduce_data_selective[[name]] <- df
}

deduce_data_selective <- plyr::ldply(deduce_data_selective, data.frame, .id="DeDuCE_output")

### 5.1 Cleaning and adding additional variables -------
# adjusting to have the total per exporter group, per commodity, per year
deduce_data_selective <- deduce_data_selective %>%
  group_by(producer_country, commodity, year, exporter_group) %>%
  summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE), .groups = "drop") 

# checking whether there are any duplicates in names
deduce_data_selective %>%
  arrange(exporter_group) %>%
  mutate(exporter_group_similarity = stringdist::stringsim(exporter_group, lag(exporter_group), method = "jw")) %>% 
  distinct(exporter_group, .keep_all=TRUE) %>%
  select(exporter_group, exporter_group_similarity) %>%
  write_csv("./intermediate-results/exporter_groups_duplicates_check.csv")

# duplicates checked manually and reimported with final names

exporter_groups_duplicates_checks <- readxl::read_excel("./intermediate-results/exporter_groups_duplicates_check.xlsx")

deduce_data_selective <- deduce_data_selective %>%
  left_join(exporter_groups_duplicates_checks %>% select(exporter_group, final_name),
            by = c("exporter_group")) %>%
  select(-exporter_group) %>%
  rename(exporter_group = final_name) %>%
  group_by(producer_country, commodity, year, exporter_group) %>% # recalculate yearly amounts for new groups
  mutate(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE)) %>%
  ungroup() %>% # add a record of which years the exporter appeared in the amazon data
  group_by(producer_country, commodity, exporter_group) %>%
  mutate(years_appeared = str_c(sort(unique(year)), collapse = ", ")) %>%
  ungroup() %>% # adding on data availability years
  left_join(country_commodity_years, by = c("producer_country", "commodity")) %>%
  rename(years_available = year_range) %>%  # this allows you to look at whether the deforestation has been phased out
  distinct(producer_country, commodity, year, exporter_group, .keep_all = TRUE) # remove duplicate entries (should only lose 5 or so from renaming)

### 5.2 Save down companies --------
# This is a version with the years as text rather than vectors
deduce_data_selective %>% 
  select(producer_country, commodity, year, exporter_group, everything()) %>%
  write_csv("./intermediate-results/exporter_groups_deduce_data.csv")

## 6 - Do any companies phase in or out of the Amazon? ------
# convert years from strings to vectors to be able to cross-check
# phase-in - first observed year is after start of available range but then remains continuous
# phase-out - last observed year is before the end of available range and does not reappear
# intermittent - years are non-continuous
# full coverage - appears in all years of the data, or nearly

deduce_data_selective_temporal_patterns <- deduce_data_selective %>% 
  mutate(
    years_appeared = str_split(years_appeared, ",\\s*") %>% map(as.integer),
    years_available = str_extract(years_available, "\\d{4}-\\d{4}") %>%
      str_split("-") %>%
      map(~ seq(as.integer(.x[1]), as.integer(.x[2])))
  ) %>%
  mutate(
    year_appeared_first = map_int(years_appeared, min),
    year_appeared_last = map_int(years_appeared, max),
    year_available_first = map_int(years_available, min),
    year_available_last = map_int(years_available, max),
    is_continuous = map_lgl(years_appeared, ~ all(diff(sort(.x)) == 1)),
    coverage_ratio = map2_dbl(years_appeared, years_available, ~ length(intersect(.x, .y)) / length(.y)),
    largest_gap = map_int(years_appeared, ~ {
      diffs <- diff(sort(.x))
      if(length(diffs) == 0) 0 else max(diffs)
    }),
    deduce_temporal_pattern = case_when(
      coverage_ratio == 1 ~ "Full coverage",
      (year_appeared_first > year_available_first) & (year_appeared_last < year_available_last) & is_continuous ~ "Phased in, then out",
      year_appeared_first > year_available_first & is_continuous ~ "Phased in",
      year_appeared_last < year_available_last & is_continuous ~ "Phased out",
      coverage_ratio > 0.9 & largest_gap <= 2 ~ "Near-continuous",
      TRUE ~ "Intermittent"
    )
  ) 

deduce_data_selective_temporal_patterns %>%
  group_by(deduce_temporal_pattern) %>%
  summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = adjusted_deforestation_exposure / sum(adjusted_deforestation_exposure)) %>%
  ggplot() +
  geom_col(aes(x = adjusted_deforestation_exposure, 
               y = reorder(deduce_temporal_pattern, adjusted_deforestation_exposure))) +
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1), 
                x = adjusted_deforestation_exposure/2, 
                y = deduce_temporal_pattern),
            colour = "white",
            size = 3) +
  theme_minimal() +
  labs(y = "")

# get the numbers
deduce_data_selective_temporal_patterns %>%
  group_by(deduce_temporal_pattern) %>%
  summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop = adjusted_deforestation_exposure / sum(adjusted_deforestation_exposure)) %>%
  arrange(desc(adjusted_deforestation_exposure)) %>% View()

# explore those that phased in and out
deduce_data_selective_temporal_patterns %>%
  filter(deduce_temporal_pattern %in% c("Phased in", "Phased out")) %>%
  View()

deduce_data_selective_temporal_patterns %>%
  filter(deduce_temporal_pattern %in% c("Phased in", "Phased out", "Phased in, then out")) %>%
  distinct(exporter_group, .keep_all = TRUE) %>%
  select(producer_country, commodity, exporter_group, years_appeared, years_available, deduce_temporal_pattern) %>%
  write_csv("./intermediate-results/exporter_groups_phased_in_out_both.csv")

## 7 - Set up master company database for saving data

write_csv(deduce_data_selective %>%
            left_join(deduce_data_selective_temporal_patterns %>% distinct(producer_country, commodity, exporter_group, deduce_temporal_pattern),
                      by = c("producer_country", "commodity", "exporter_group")) %>% # join on temporal pattern to simpler data
            arrange(exporter_group), # arrange by companies
          "./analytical-results/companies.csv")
