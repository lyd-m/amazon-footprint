# AMAZON FOOTPRINT IN FOOD AND FINANCIAL SYSTEMS --------
# Financial flows analysis: Stage 7 - Early figures -------
# github.com/lyd-m/amazon-footprint
# This project contains the financial analysis for the paper, Singh et al. (2025)
# This script takes the final company and financial data and visualises it

rm(list = ls())
### 0. DEPENDENCIES --------------------------
library(readxl)
library(tidyverse)
library(janitor)
library(patchwork)
library(RColorBrewer)
library(scales)
library(openxlsx)
library(purrr)
library(ggrepel)

### 0. FUNCTIONS --------------------------
to_file_save_format <- function(str) {
  file_save_format_string <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", str, perl = TRUE)
  file_save_format_string <- gsub(" ", "-", tolower(file_save_format_string))
  return(file_save_format_string)
}

modulus <- function(x) {
  sqrt(x^2)
}

rowsum_helper <- function(df,
                          cols,
                          new_col = "row_sum",
                          na.rm = TRUE) {
  df %>%
    dplyr::mutate({{new_col}} := rowSums(dplyr::select(., dplyr::all_of(cols)), na.rm = na.rm))
}

colour_scheme_for_x_variables <- function(x) {
  palette_colors <- brewer.pal(8, "Set2")
  palette_colors[c(1:x)]  # Change the index to pick a different color
}

### 1. DATA IMPORT --------------------------
regions_unsd <- read_csv("./input-data/regions_map.csv") %>%
  select(country, region_unsd)

#### 1.1. Companies --------------------------
companies_all_yrs <- read_csv("./analytical-results/companies_all_years.csv") %>%
  clean_names() %>%
  select(-1) %>%
  mutate(ultimate_parent_company_oa_perm_id = as.character(ultimate_parent_company_oa_perm_id))

company_info_by_permid <- read_csv("./intermediate-results/companies_info_by_permid.csv") %>%
  select(-organization_perm_id, -1)

unsd_regions <- read_csv("./input-data/regions_map.csv")

company_info_by_permid <- company_info_by_permid %>%
  left_join(
    unsd_regions %>% select(country, region_unsd),
    by = c("country_of_headquarters" = "country")
  ) %>%
  rename(region_of_headquarters_unsd = region_unsd)

#### 1.2. Flows --------------------------
flows_files <- list.files("./intermediate-results/", pattern = "flows_attributed_")

flows <- list()
for (file_ in flows_files) {
  var_name <- str_replace(file_, "flows_attributed_2008-2024_flows_", "")
  var_name <- str_replace(var_name, "\\(.*", "")
  var_name <- str_replace(var_name, ".csv", "")
  
  df <- read_csv(paste0("./intermediate-results/", file_)) %>%
    clean_names()
  
  flows[[var_name]] <- df
}

#### 1.3. Production --------------------------
# physical volumes
production_volumes_file <- list.files("./input-data/", pattern = "FAOSTAT_data_production_physical", full.names = TRUE)

production_volumes <- read_csv(production_volumes_file) %>%
  clean_names() %>%
  filter(element == "Production") %>%
  select(area, item, year, unit, value) %>%
  mutate(
    area = if_else(grepl("Bolivia", area), # change from plurinational nation of bolivia
                   "Bolivia", area),
    item = if_else(grepl("cattle", item), "Cattle meat", # make consistent with data
                   item)
  )

# allow 2024 production volumes to be the same as 2023 as no data available

production_volumes_2024 <- production_volumes %>%
  filter(year == 2023) %>%
  mutate(year = 2024)

production_volumes <- bind_rows(production_volumes, production_volumes_2024)

# production monetary value (use unadjusted values as will compare within year anyway)
production_monetary_file <- list.files("./input-data/", pattern = "FAOSTAT_data_production_value", full.names = TRUE)

production_monetary <- read_csv(production_monetary_file) %>%
  clean_names() %>%
  filter(element == "Gross Production Value (constant 2014-2016 thousand US$)") %>%
  select(area, item, year, unit, value) %>%
  mutate(
    area = if_else(grepl("Bolivia", area), # change from plurinational nation of bolivia
                   "Bolivia", area),
    item = if_else(grepl("cattle", item), "Cattle meat", # make consistent with data
                   item)
  )

# allow 2024 production volumes to be the same as 2023 as no data available

production_monetary_2024 <- production_monetary %>%
  filter(year == 2023) %>%
  mutate(year = 2024)

production_monetary <- bind_rows(production_monetary, production_monetary_2024)

production <- list("volumes" = production_volumes, "values" = production_monetary)

# check data availability
for (i in names(production)) {
  print(i)
  df <- production[[i]]
  result <- df %>%
    group_by(area, item) %>%
    summarise(min_year = min(year), max_year = max(year))
  
  print(result, n = Inf)
}

### 2. CODES FOR STANDARD PLOT TYPES ---------------------
## NB - legal entity ownership charts need checking that they still work
horizontal_bar_chart <- function(grouped_data,
                                 # all variable names need to be in quotes ""
                                 x_value = "dummy_x",
                                 # can change if you're using multiple categories, remember to create dummy x if want one line
                                 y_value,
                                 # col being used (a prop between 0 and 1)
                                 fill_value,
                                 # what you're studying the distribution of
                                 y_value_label,
                                 fill_value_label,
                                 title_label,
                                 custom_colours = NA,
                                 outside_label = NA) {
  df_plot <- grouped_data
  plot <- ggplot(data = grouped_data, aes(x = .data[[x_value]], y = .data[[y_value]], fill = .data[[fill_value]])) +
    geom_bar(stat = "identity") +
    # percentage labels
    geom_text(
      aes(
        label = scales::percent(.data[[y_value]], accuracy = 1),
        y = !!sym(y_value)
      ),
      position = position_stack(vjust = 0.5),
      color = "white",
      size = 4
    ) +
    labs(
      fill = fill_value_label,
      x = "",
      y = "",
      title = title_label
    ) +
    theme_void() +
    coord_flip() +
    scale_fill_manual(values = custom_colours) +
    theme(
      legend.position = "bottom",
      legend.direction = "vertical",
      axis.text.y = element_text(colour = "black", hjust = 1),
      # right justified
      plot.title = element_text(),
      plot.title.position = "panel"
    )
  
  return(plot)
}


### 3. COMPANY DATA ANALYSIS --------------------------

# Add on extra info
companies_all_yrs <- companies_all_yrs %>%
  left_join(company_info_by_permid,
            by = c("ultimate_parent_company_oa_perm_id" = "permid")) %>%
  rename_with(.fn = ~ paste0("ultimate_parent_", .x),
              .cols = 19:54)

#### 3.1. Legal entity data availability ------------------
# this data as it stands includes deforestation exposure attributed to UNKNOWN
legal_entity_availability <- companies_all_yrs %>%
  group_by(producer_country, commodity, legal_entity_mapped) %>% # for each country-commodity setting (ignore years)
  summarise(
    adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE),
    # sum across all years for each one
    count = n_distinct(exporter_group)
  ) %>%
  ungroup() %>%
  group_by(producer_country, commodity) %>%
  mutate(
    prop_adjusted_deforestation_exposure = adjusted_deforestation_exposure /
      sum(adjusted_deforestation_exposure, na.rm = TRUE),
    prop_count = count / sum(count)
  ) %>%
  mutate(country_commodity = str_c(producer_country, " - ", commodity))

plt_legal_entity_by_count <- horizontal_bar_chart(
  legal_entity_availability,
  x_value = "country_commodity",
  y_value = "prop_count",
  fill_value = "legal_entity_mapped",
  y_value_label = "% (count)",
  fill_value_label = "Legal entity mapped?",
  title_label = "% (count)",
  custom_colours = c("grey", "lightgreen")
)

plt_legal_entity_by_defn_exposure <- horizontal_bar_chart(
  legal_entity_availability,
  x_value = "country_commodity",
  y_value = "prop_adjusted_deforestation_exposure",
  fill_value = "legal_entity_mapped",
  y_value_label = "% (deforestation exposure)",
  fill_value_label = "Legal entity mapped?",
  title_label = "% (deforestation exposure)",
  custom_colours = c("grey", "lightgreen")
)

wrap_plots(plt_legal_entity_by_count,
           plt_legal_entity_by_defn_exposure,
           ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("./figures/draft_legal_entity_data_avail_w_unknown.pdf")

# without UNKNOWN
legal_entity_availability_wo_unknown <- companies_all_yrs %>%
  filter(!exporter_group == "UNKNOWN") %>%
  group_by(producer_country, commodity, legal_entity_mapped) %>% # for each country-commodity setting (ignore years)
  summarise(
    adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE),
    # sum across all years for each one
    count = n_distinct(exporter_group)
  ) %>%
  ungroup() %>%
  group_by(producer_country, commodity) %>%
  mutate(
    prop_adjusted_deforestation_exposure = adjusted_deforestation_exposure /
      sum(adjusted_deforestation_exposure, na.rm = TRUE),
    prop_count = count / sum(count)
  ) %>%
  mutate(country_commodity = str_c(producer_country, " - ", commodity))

plt_legal_entity_by_count_wo_unknown <- horizontal_bar_chart(
  legal_entity_availability_wo_unknown,
  x_value = "country_commodity",
  y_value = "prop_count",
  fill_value = "legal_entity_mapped",
  y_value_label = "% (count)",
  fill_value_label = "Legal entity mapped?",
  title_label = "% (count)",
  custom_colours = c("grey", "lightgreen")
)

plt_legal_entity_by_defn_exposure_wo_unknown <- horizontal_bar_chart(
  legal_entity_availability_wo_unknown,
  x_value = "country_commodity",
  y_value = "prop_adjusted_deforestation_exposure",
  fill_value = "legal_entity_mapped",
  y_value_label = "% (deforestation exposure)",
  fill_value_label = "Legal entity mapped?",
  title_label = "% (deforestation exposure)",
  custom_colours = c("grey", "lightgreen")
)

wrap_plots(
  plt_legal_entity_by_count,
  plt_legal_entity_by_count_wo_unknown,
  plt_legal_entity_by_defn_exposure,
  plt_legal_entity_by_defn_exposure_wo_unknown,
  ncol = 2
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("./figures/draft_legal_entity_data_avail_all.pdf")

#### 3.2. Legal entity type ------------------

legal_entity_types <- companies_all_yrs %>%
  mutate(
    legal_entity_type = case_when(
      ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
      ultimate_parent_organization_is_public_flag == TRUE &
        ultimate_parent_free_float_percent_ >= 90 &
        ultimate_parent_government_ultimate_parent != "True" ~ "Publicly listed",
      ultimate_parent_organization_is_public_flag == TRUE &
        ultimate_parent_free_float_percent_ < 90 &
        ultimate_parent_government_ultimate_parent != "True" ~ "Part-listed",
      ultimate_parent_organization_is_public_flag == FALSE &
        ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
      ultimate_parent_organization_is_public_flag == FALSE &
        (
          ultimate_parent_government_ultimate_parent == "False" |
            ultimate_parent_government_ultimate_parent == "Unknown"
        ) ~ "Private",
      legal_entity_mapped == FALSE ~ "No legal entity mapped",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(producer_country, commodity, legal_entity_type) %>%
  summarise(
    adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE),
    count = n_distinct(exporter_group)
  ) %>%
  ungroup() %>%
  group_by(producer_country, commodity) %>%
  mutate(
    prop_adjusted_deforestation_exposure = adjusted_deforestation_exposure / sum(adjusted_deforestation_exposure, na.rm = TRUE),
    prop_count = count / sum(count)
  ) %>%
  mutate(country_commodity = str_c(producer_country, " - ", commodity))

plt_legal_entity_types_by_count <- horizontal_bar_chart(
  legal_entity_types,
  x_value = "country_commodity",
  y_value = "prop_count",
  fill_value = "legal_entity_type",
  y_value_label = "% (count)",
  fill_value_label = "Ultimate parent type",
  title_label = "% (count)",
  custom_colours = c(
    "Unknown" = "grey",
    "No legal entity mapped" = "lightgrey",
    "Publicly listed" = "lightblue",
    "Part-listed" = "lightgreen",
    "Private" = "darkgreen",
    "State-owned" = "darkblue"
  )
)

plt_legal_entity_types_by_defn_exposure <- horizontal_bar_chart(
  legal_entity_types,
  x_value = "country_commodity",
  y_value = "prop_adjusted_deforestation_exposure",
  fill_value = "legal_entity_type",
  y_value_label = "% (deforestation exposure)",
  fill_value_label = "Ultimate parent type",
  title_label = "% (deforestation exposure)",
  custom_colours = c(
    "Unknown" = "grey",
    "No legal entity mapped" = "lightgrey",
    "Publicly listed" = "lightblue",
    "Part-listed" = "lightgreen",
    "Private" = "darkgreen",
    "State-owned" = "darkblue"
  )
)

legal_entity_types_wo_unknown <- companies_all_yrs %>%
  filter(!exporter_group == "UNKNOWN") %>%
  mutate(
    legal_entity_type = case_when(
      ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
      ultimate_parent_organization_is_public_flag == TRUE &
        ultimate_parent_free_float_percent_ >= 90 &
        ultimate_parent_government_ultimate_parent != "True" ~ "Publicly listed",
      ultimate_parent_organization_is_public_flag == TRUE &
        ultimate_parent_free_float_percent_ < 90 &
        ultimate_parent_government_ultimate_parent != "True" ~ "Part-listed",
      ultimate_parent_organization_is_public_flag == FALSE &
        ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
      ultimate_parent_organization_is_public_flag == FALSE &
        (
          ultimate_parent_government_ultimate_parent == "False" |
            ultimate_parent_government_ultimate_parent == "Unknown"
        ) ~ "Private",
      legal_entity_mapped == FALSE ~ "No legal entity mapped",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(producer_country, commodity, legal_entity_type) %>%
  summarise(
    adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE),
    count = n_distinct(exporter_group)
  ) %>%
  ungroup() %>%
  group_by(producer_country, commodity) %>%
  mutate(
    prop_adjusted_deforestation_exposure = adjusted_deforestation_exposure / sum(adjusted_deforestation_exposure, na.rm = TRUE),
    prop_count = count / sum(count)
  ) %>%
  mutate(country_commodity = str_c(producer_country, " - ", commodity))

plt_legal_entity_types_by_count_wo_unknown <- horizontal_bar_chart(
  legal_entity_types_wo_unknown,
  x_value = "country_commodity",
  y_value = "prop_count",
  fill_value = "legal_entity_type",
  y_value_label = "% (count)",
  fill_value_label = "Ultimate parent type",
  title_label = "% (count)",
  custom_colours = c(
    "Unknown" = "grey",
    "No legal entity mapped" = "lightgrey",
    "Publicly listed" = "lightblue",
    "Part-listed" = "lightgreen",
    "Private" = "darkgreen",
    "State-owned" = "darkblue"
  )
)

plt_legal_entity_types_by_defn_exposure_wo_unknown <- horizontal_bar_chart(
  legal_entity_types_wo_unknown,
  x_value = "country_commodity",
  y_value = "prop_adjusted_deforestation_exposure",
  fill_value = "legal_entity_type",
  y_value_label = "% (deforestation exposure)",
  fill_value_label = "Ultimate parent type",
  title_label = "% (deforestation exposure)",
  custom_colours = c(
    "Unknown" = "grey",
    "No legal entity mapped" = "lightgrey",
    "Publicly listed" = "lightblue",
    "Part-listed" = "lightgreen",
    "Private" = "darkgreen",
    "State-owned" = "darkblue"
  )
)


wrap_plots(
  plt_legal_entity_types_by_count,
  plt_legal_entity_types_by_count_wo_unknown,
  plt_legal_entity_types_by_defn_exposure,
  plt_legal_entity_types_by_defn_exposure_wo_unknown,
  ncol = 2
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("./figures/draft_legal_entity_types_all.pdf")

#### 3.3 Legal entity headquarters ------------------

legal_entity_hqs <- companies_all_yrs %>%
  group_by(producer_country,
           commodity,
           ultimate_parent_region_of_headquarters_unsd) %>%
  summarise(
    adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE),
    count = n_distinct(exporter_group)
  ) %>%
  ungroup() %>%
  group_by(producer_country, commodity) %>%
  mutate(
    prop_adjusted_deforestation_exposure = adjusted_deforestation_exposure / sum(adjusted_deforestation_exposure, na.rm = TRUE),
    prop_count = count / sum(count)
  ) %>%
  mutate(country_commodity = str_c(producer_country, " - ", commodity))

# PLOTS NEED FIXING
#plt_legal_entity_hqs_by_count <- horizontal_bar_chart(
legal_entity_hqs, x_value = "country_commodity", y_value = "prop_count", fill_value = "ultimate_parent_region_of_headquarters", y_value_label = "% (count)", fill_value_label = "Ultimate parent HQ region", title_label = "% (count)", custom_colours = "grey"
)

# PLOTS NEED FIXING
#plt_legal_entity_types_by_defn_exposure <- horizontal_bar_chart(
legal_entity_types, x_value = "country_commodity", y_value = "prop_adjusted_deforestation_exposure", fill_value = "legal_entity_type", y_value_label = "% (deforestation exposure)", fill_value_label = "Ultimate parent type", title_label = "% (deforestation exposure)", custom_colours = c(
  "Unknown" = "grey",
  "No legal entity mapped" = "lightgrey",
  "Publicly listed" = "lightblue",
  "Part-listed" = "lightgreen",
  "Private" = "darkgreen",
  "State-owned" = "darkblue"
)
)

#legal_entity_types_wo_unknown <- companies_all_yrs %>%
filter(!exporter_group == "UNKNOWN") %>%
  mutate(
    legal_entity_type = case_when(
      ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
      ultimate_parent_organization_is_public_flag == TRUE &
        ultimate_parent_free_float_percent_ >= 90 &
        ultimate_parent_government_ultimate_parent != "True" ~ "Publicly listed",
      ultimate_parent_organization_is_public_flag == TRUE &
        ultimate_parent_free_float_percent_ < 90 &
        ultimate_parent_government_ultimate_parent != "True" ~ "Part-listed",
      ultimate_parent_organization_is_public_flag == FALSE &
        ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
      ultimate_parent_organization_is_public_flag == FALSE &
        (
          ultimate_parent_government_ultimate_parent == "False" |
            ultimate_parent_government_ultimate_parent == "Unknown"
        ) ~ "Private",
      legal_entity_mapped == FALSE ~ "No legal entity mapped",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(producer_country, commodity, legal_entity_type) %>%
  summarise(
    adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE),
    count = n_distinct(exporter_group)
  ) %>%
  ungroup() %>%
  group_by(producer_country, commodity) %>%
  mutate(
    prop_adjusted_deforestation_exposure = adjusted_deforestation_exposure / sum(adjusted_deforestation_exposure, na.rm = TRUE),
    prop_count = count / sum(count)
  ) %>%
  mutate(country_commodity = str_c(producer_country, " - ", commodity))

#plt_legal_entity_types_by_count_wo_unknown <- horizontal_bar_chart(
legal_entity_types_wo_unknown, x_value = "country_commodity", y_value = "prop_count", fill_value = "legal_entity_type", y_value_label = "% (count)", fill_value_label = "Ultimate parent type", title_label = "% (count)", custom_colours = c(
  "Unknown" = "grey",
  "No legal entity mapped" = "lightgrey",
  "Publicly listed" = "lightblue",
  "Part-listed" = "lightgreen",
  "Private" = "darkgreen",
  "State-owned" = "darkblue"
)
)

#plt_legal_entity_types_by_defn_exposure_wo_unknown <- horizontal_bar_chart(
legal_entity_types_wo_unknown, x_value = "country_commodity", y_value = "prop_adjusted_deforestation_exposure", fill_value = "legal_entity_type", y_value_label = "% (deforestation exposure)", fill_value_label = "Ultimate parent type", title_label = "% (deforestation exposure)", custom_colours = c(
  "Unknown" = "grey",
  "No legal entity mapped" = "lightgrey",
  "Publicly listed" = "lightblue",
  "Part-listed" = "lightgreen",
  "Private" = "darkgreen",
  "State-owned" = "darkblue"
)
)


#wrap_plots(
plt_legal_entity_types_by_count, plt_legal_entity_types_by_count_wo_unknown, plt_legal_entity_types_by_defn_exposure, plt_legal_entity_types_by_defn_exposure_wo_unknown, ncol = 2
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#ggsave("./figures/draft_legal_entity_types_all.pdf")


### 4.FINANCIAL DATA ANALYSIS --------------------------

boundary_periods <- c(0, 1, 2, 3)

#### 4.1. Combined datasets ---------------
##### 4.1.1. Commodity dataset (i.e., combine same commodities from different countries) ---------------------------
# bind all data together for same commodities
# duplicate transactions are split between them based on relative production volumes between those countries for that year
# start with doing SEI-Trase simple, no phase out

all_flows_simple_bind <- tibble()
for (country_commodity in names(flows)) {
  df <- flows[[country_commodity]] %>%
    mutate(bond_isin = as.character(bond_isin)) # sorting out inconsistent column types
  
  all_flows_simple_bind <- bind_rows(all_flows_simple_bind, df)
}

# parse commodity datasets between different countries based on production volumes

parse_by_production_volumes <- function(df, commodity_filter) {
  df %>%
    filter(commodity == commodity_filter) %>%
    left_join(
      production_volumes %>% select(-unit),
      by = c(
        "producer_country" = "area",
        "year" = "year",
        "commodity" = "item"
      )
    ) %>%
    rename(production_volume_commodity_this_yr = value) %>%
    group_by(tranche_id) %>% # handling where transactions exist across two countries for the same commodity (e.g., JBS)
    mutate(
      tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity =
        tranche_amount_per_manager_usd_m_final_in_2024_av_usd * (production_volume_commodity_this_yr / sum(
          unique(production_volume_commodity_this_yr)
        ))
    ) %>%
    ungroup()
}

commodity_names <- c(
  "Cattle meat",
  "Cocoa beans",
  "Coffee, green",
  "Oil palm fruit",
  "Soya beans",
  "Sugar cane"
)

commodity_map <- setNames(
  commodity_names,
  c(
    "cattle_meat",
    "cocoa_beans",
    "coffee_green",
    "oil_palm",
    "soya_beans",
    "sugar_cane"
  )
)

flows_by_commodity <- tibble(commodity = names(commodity_map)) %>% mutate(data = map(commodity, ~ {
  df <- parse_by_production_volumes(all_flows_simple_bind, commodity_map[[.x]]) 
  
  # also create annualised data by sei_trase years
  df %>%
    mutate(
      sei_trase_years_available = sei_trase_data_max_year - sei_trase_data_min_year + 1,
      tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average =
        tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity / sei_trase_years_available) %>%
    mutate(country_commodity = str_c(producer_country, commodity, sep = " - "))
    
}))

check_sums <- purrr::map_dbl(flows_by_commodity$data, function(df) {
  df_distinct <- df %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
  
  (
    sum(
      df_distinct$tranche_amount_per_manager_usd_m_final_in_2024_av_usd,
      na.rm = TRUE
    ) -
      sum(
        df$tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity,
        na.rm = TRUE
      )
  ) /
    sum(df_distinct$tranche_amount_per_manager_usd_m_final_in_2024_av_usd,
        na.rm = TRUE)
})
check_sums

wb <- createWorkbook()
pwalk(flows_by_commodity, function(commodity, data, ...) {
  # Create a unique sheet name
  sheet_name <- paste0(commodity)
  # Sheet names have to be <= 31 chars in Excel
  sheet_name <- substr(sheet_name, 1, 31)
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, data)
})
saveWorkbook(wb,
             "./intermediate-results/flows_by_commodity_all_yrs.xlsx",
             overwrite = TRUE)

##### 4.1.2. Overall amazon dataset (i.e., combine different commodities from different countries) ---------------------------
# bind all data together for same commodities
# duplicate transactions are split between them based on relative production MONETARY VALUES between those commodity/countries for that year
# Ideally you would have firm-level production volumes / monetary values but this is not readily available in a consistent format

# create combined dataset of all country-commodity groups
parse_by_production_value <- function(df) {
  df %>%
    left_join(
      production_monetary %>% select(-unit),
      by = c(
        "producer_country" = "area",
        "year" = "year",
        "commodity" = "item"
      )
    ) %>%
    rename(production_value_country_commodity_this_yr = value) %>%
    group_by(tranche_id) %>% # handling where transactions exist across multiple commodities/countries for the same commodity
    mutate(
      tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates =
        tranche_amount_per_manager_usd_m_final_in_2024_av_usd * (production_value_country_commodity_this_yr / sum(
          unique(production_value_country_commodity_this_yr)
        ))
    ) %>%
    ungroup()
}

flows_all_countries_commodities <- all_flows_simple_bind %>%
  parse_by_production_value() %>%
  mutate(country_commodity = str_c(producer_country, commodity, sep = " - "))

# check the calculation is consistent (i.e., deals are actually split)
check_sums <- (
  sum((
    flows_all_countries_commodities %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
  )$tranche_amount_per_manager_usd_m_final_in_2024_av_usd,
  na.rm = TRUE
  ) -
    sum((
      flows_all_countries_commodities$tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates
    ),
    na.rm = TRUE
    )
) / sum((
  flows_all_countries_commodities %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
)$tranche_amount_per_manager_usd_m_final_in_2024_av_usd,
na.rm = TRUE
)
check_sums # only a rounding errors

# annualising the data to account for number of SEI-TRASE years available (bc the financial data spans different time periods)
flows_all_countries_commodities <- flows_all_countries_commodities %>%
  mutate(
    sei_trase_years_available = sei_trase_data_max_year - sei_trase_data_min_year + 1,
    # add one since inclusive (e.g., 2015, 2016, 2017)
    # N.B: that as this stands it does not account for the boundary period, just the SEI-Trase years
    tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised = tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates / sei_trase_years_available
  )

# save version in intermediate results
write_csv(
  flows_all_countries_commodities,
  "./intermediate-results/flows_all_countries_commodities_all_yrs.csv"
)

##### 4.1.3. Financial flows data availability ------------------
exporter_gps_consol_in_flows <- flows_all_countries_commodities %>%
  distinct(exporter_group)

# noting that some exporter groups are combined (w a separator) in the flows data as they relate to the same legal entity
# this means we need to do a substring match from the company data rather than a 1:1 match

exporter_gps_data_availability <- companies_all_yrs %>%
  distinct(producer_country, commodity, exporter_group) %>%
  rowwise() %>%
  mutate(fin_flows = any(
    grepl(exporter_group, exporter_gps_consol_in_flows$exporter_group)
  ))

##### 4.1.4. Combine financial flows and legal entity data availability ------------------
exporter_gps_w_legal_entities <- companies_all_yrs %>%
  distinct(producer_country,
           commodity,
           exporter_group,
           legal_entity_mapped) %>%
  filter(legal_entity_mapped == TRUE)

exporter_gps_data_availability <- exporter_gps_data_availability %>%
  mutate(legal_entity = any(
    grepl(
      exporter_group,
      exporter_gps_w_legal_entities$exporter_group
    )
  ))

write_csv(
  exporter_gps_data_availability,
  "./analytical-results/exporter_groups_financial_data_availability.csv"
)


#### 4.2. Setting up sensitivity testing datasets --------------------------
country_commodity_dict <- tibble()
for (country_commodity_ in names(flows)) {
  df <- flows[[country_commodity_]]
  df <- df %>%
    distinct(producer_country, commodity) %>%
    mutate(country_commodity = country_commodity_) %>%
    select(country_commodity, everything())
  
  country_commodity_dict <- bind_rows(country_commodity_dict, df)
}

boundary_periods <- c("all", 0, 1, 2, 3)
deforestation_phase_out_status <- c(FALSE, TRUE) # if true, remove financial flows after the year deforestation has been phased out

##### for unadjusted data (explore unadjusted flows individually for each commodity) ----------
#country_commodity_param_grid <- tibble(country_commodity_case = names(flows)) %>%
# crossing(boundary_period = boundary_periods, defn_phase_out = deforestation_phase_out_status)

#flows_by_country_commodity_sensitivity_df

##### for countries combined together for each commodity ----------
commodity_param_grid <- tibble(commodity_case = unique(flows_by_commodity$commodity)) %>%
  crossing(boundary_period = boundary_periods, defn_phase_out = deforestation_phase_out_status)

flows_by_commodity_sensitivity_df <- commodity_param_grid %>%
  mutate(flows_filtered = pmap(list(commodity_case, boundary_period, defn_phase_out), function(comm, bp, defn) {
    # get the nested df
    df <- flows_by_commodity %>%
      filter(commodity == comm) %>%
      pull(data) %>% .[[1]]
    
    # apply boundary period filter
    if (bp != "all") {
      df <- df %>%
        filter(modulus(year_relative_to_trase_period) <= bp)
    }
    
    # apply deforestation phase-out filter
    if (defn) {
      df <- df %>%
        filter(!remove_flow_based_on_phase_out)
    }
    
    df
  }))

wb <- createWorkbook()

pwalk(flows_by_commodity_sensitivity_df, function(commodity_case,
                                                  boundary_period,
                                                  defn_phase_out,
                                                  flows_filtered,
                                                  ...) {
  sheet_name <- paste0(
    commodity_case,
    "_",
    boundary_period,
    "_",
    ifelse(defn_phase_out, "phaseout", "no_phaseout")
  )
  
  sheet_name <- substr(sheet_name, 1, 31)
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, flows_filtered)
})

saveWorkbook(
  wb,
  "./intermediate-results/flows_by_commodity_sensitivity_data.xlsx",
  overwrite = TRUE
)

##### for all countries and commodities combined ----------
overall_param_grid <- expand_grid(boundary_period = boundary_periods, defn_phase_out = deforestation_phase_out_status)

flows_all_countries_commodities_sensitivity_df <- overall_param_grid %>%
  mutate(flows_filtered = pmap(list(boundary_period, defn_phase_out), ~ {
    bp <- ..1
    defn <- ..2
    
    df <- flows_all_countries_commodities
    
    # boundary filter only if bp is not 'all'
    if (bp != "all") {
      df <- df %>%
        filter(modulus(year_relative_to_trase_period) <= bp)
    }
    
    # defn phase out filter
    if (defn) {
      df <- df %>%
        filter(!remove_flow_based_on_phase_out) # remove flows that have phased out
    }
    
    df
  }))

wb <- createWorkbook()

pwalk(flows_all_countries_commodities_sensitivity_df, function(boundary_period,
                                                               defn_phase_out,
                                                               flows_filtered,
                                                               ...) {
  sheet_name <- paste0("bp_",
                       boundary_period,
                       "_",
                       ifelse(defn_phase_out, "phaseout", "no_phaseout"))
  
  sheet_name <- substr(sheet_name, 1, 31)
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, flows_filtered)
})

saveWorkbook(
  wb,
  "./intermediate-results/flows_all_countries_commodities_sensitivity_data.xlsx",
  overwrite = TRUE
)

#### 4.2. Totals ------------
# find total in absolute USD millions and annualised by SEI-Trase dates

# by commodity
flows_by_commodity_totals <- flows_by_commodity_sensitivity_df %>%
  mutate(results = map(flows_filtered, ~ {
    df <- .x
    
    if (nrow(df) == 0) {
      return(
        tibble(
          total_usd_m_high_conf = 0,
          total_usd_m_medium_conf = 0,
          total_usd_m_low_conf = 0,
          total_usd_m = 0
        )
      )
    }
    
    df %>%
      summarise(
        total_usd_m_high_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity[financial_flow_link_strength == "High"],
          na.rm = TRUE
        ),
        total_usd_m_medium_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity[financial_flow_link_strength == "Medium"],
          na.rm = TRUE
        ),
        total_usd_m_low_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity[financial_flow_link_strength == "Low"],
          na.rm = TRUE
        )
      ) %>%
      mutate(total_usd_m = total_usd_m_high_conf + total_usd_m_medium_conf + total_usd_m_low_conf)
  })) %>%
  unnest(results) %>%
  mutate(results = map(flows_filtered, ~ {
    df <- .x
    
    if (nrow(df) == 0) {
      return(
        tibble(
          total_usd_m_yr_high_conf = 0,
          total_usd_m_yr_medium_conf = 0,
          total_usd_m_yr_low_conf = 0,
          total_usd_m_yr = 0
        )
      )
    }
    
    df %>%
      summarise(
        total_usd_m_yr_high_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average[financial_flow_link_strength == "High"],
          na.rm = TRUE
        ),
        total_usd_m_yr_medium_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average[financial_flow_link_strength == "Medium"],
          na.rm = TRUE
        ),
        total_usd_m_yr_low_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average[financial_flow_link_strength == "Low"],
          na.rm = TRUE
        )
      ) %>%
      mutate(total_usd_m_yr = total_usd_m_yr_high_conf + total_usd_m_yr_medium_conf + total_usd_m_yr_low_conf)
  })) %>%
  unnest(results) %>%
  select(-flows_filtered)

write_csv(
  flows_by_commodity_totals,
  "./analytical-results/flows_by_commodity_totals_sensitivity.csv"
)

# for combined data
flows_all_combined_totals <- flows_all_countries_commodities_sensitivity_df %>%
  mutate(results = map(flows_filtered, ~ {
    df <- .x
    
    if (nrow(df) == 0) {
      return(
        tibble(
          total_usd_m_high_conf = 0,
          total_usd_m_medium_conf = 0,
          total_usd_m_low_conf = 0,
          total_usd_m = 0
        )
      )
    }
    
    df %>%
      summarise(
        total_usd_m_high_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates[financial_flow_link_strength == "High"],
          na.rm = TRUE
        ),
        total_usd_m_medium_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates[financial_flow_link_strength == "Medium"],
          na.rm = TRUE
        ),
        total_usd_m_low_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates[financial_flow_link_strength == "Low"],
          na.rm = TRUE
        )
      ) %>%
      mutate(total_usd_m = total_usd_m_high_conf + total_usd_m_medium_conf + total_usd_m_low_conf)
  })) %>%
  unnest(results) %>%
  mutate(results = map(flows_filtered, ~ {
    df <- .x
    
    if (nrow(df) == 0) {
      return(
        tibble(
          total_usd_m_yr_high_conf = 0,
          total_usd_m_yr_medium_conf = 0,
          total_usd_m_yr_low_conf = 0,
          total_usd_m_yr = 0
        )
      )
    }
    
    df %>%
      summarise(
        total_usd_m_yr_high_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised[financial_flow_link_strength == "High"],
          na.rm = TRUE
        ),
        total_usd_m_yr_medium_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised[financial_flow_link_strength == "Medium"],
          na.rm = TRUE
        ),
        total_usd_m_yr_low_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised[financial_flow_link_strength == "Low"],
          na.rm = TRUE
        )
      ) %>%
      mutate(total_usd_m_yr = total_usd_m_yr_high_conf + total_usd_m_yr_medium_conf + total_usd_m_yr_low_conf)
  })) %>%
  unnest(results) %>%
  select(-flows_filtered)

write_csv(
  flows_all_combined_totals,
  "./analytical-results/flows_all_countries_commodities_totals_sensitivity.csv"
)

##### visualise totals for different sensitivities ------------------
plot_df <- flows_all_combined_totals %>%
  filter(!defn_phase_out) %>%
  mutate(boundary_period = factor(boundary_period, levels = c("0", "1", "2", "3", "all")),
         central = total_usd_m_high_conf + total_usd_m_medium_conf,
         lower = total_usd_m_high_conf,
         upper = total_usd_m)

# Scatter plot with whiskers
ggplot(plot_df, aes(x = boundary_period, y = central)) +
  geom_point(size = 2.5, color = colour_scheme_for_x_variables(1)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = colour_scheme_for_x_variables(1)) +
  theme_minimal(base_size = 10) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "k", big.mark = ",")) +
  labs(
    x = "Boundary period \n (around years with exporter groups available)",
    y = "Financial flows (2024 US$m)",
    title = ""
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("./figures/totals_by_boundary_period.pdf")

#### 4.2. Summary by grouping variables ------------

vars_grouping <- c(
  "producer_country",
  "commodity",
  "country_commodity",
  "year",
  "asset_class",
  "flag_sust_finance",
  "borrower_issuer_country",
  "borrower_issuer_region",
  "manager_true_ultimate_parent_organisation_name",
  "manager_true_ultimate_parent_country_of_headquarters",
  "manager_true_ultimate_parent_region_of_headquarters_unsd",
  "government_ultimate_parent",
  "manager_true_ultimate_parent_trbc_activity_name",
  "financial_flow_link_strength",
  "flow_issuance_location_type",
  "flow_financed_location_type",
  "borrower_issuer_trbc_activity_strength_classification",
  "borrower_issuer_trbc_activity",
  "use_of_proceeds"
) # n.b. use of proceeds not consolidated

vars_grouping_for_subgrouping <- vars_grouping <- c(
  "producer_country",
  "commodity",
  "country_commodity",
  "year",
  "asset_class",
  "flag_sust_finance",
  "borrower_issuer_country",
  "borrower_issuer_region",
  "manager_true_ultimate_parent_organisation_name",
  "manager_true_ultimate_parent_country_of_headquarters",
  "manager_true_ultimate_parent_region_of_headquarters_unsd",
  "government_ultimate_parent",
  "manager_true_ultimate_parent_trbc_activity_name",
  "flow_issuance_location_type",
  "flow_financed_location_type",
  "borrower_issuer_trbc_activity_strength_classification",
  "borrower_issuer_trbc_activity",
  "use_of_proceeds"
)

vars_analytical <- c(
  "tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates",
  "tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised"
)

vars_subgrouping <- c("financial_flow_link_strength")

summarise_totals <- function(df, group_var, subgroup_var = NULL) {
  group_syms <- syms(c(group_var, subgroup_var))
  
  df %>%
    group_by(!!!group_syms) %>%
    summarise(
      total_usd_m = sum(
        tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates,
        na.rm = TRUE
      ),
      total_usd_m_yr = sum(
        tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      pct_total = total_usd_m / sum(total_usd_m, na.rm = TRUE),
      pct_total_yr = total_usd_m_yr / sum(total_usd_m_yr, na.rm = TRUE),
      rank_total = dense_rank(desc(total_usd_m)),
      rank_yr = dense_rank(desc(total_usd_m_yr))
    )
}

flows_all_by_grouping_vars <- flows_all_countries_commodities_sensitivity_df %>%
  mutate(results = map(flows_filtered, function(df) {
    map(set_names(vars_grouping), function(var) {
      summarise_totals(df, group_var = var)
    })
  })) %>%
  select(boundary_period, defn_phase_out, results) %>%
  mutate(sensitivity_id = paste0("bp", boundary_period, "_defn_po_", defn_phase_out))

# subgrouping by link strength
flows_all_by_grouping_vars_by_strength <- flows_all_countries_commodities_sensitivity_df %>%
  mutate(results = map(flows_filtered, function(df) {
    map(set_names(vars_grouping_for_subgrouping), function(var) {
      summarise_totals(df, group_var = var, subgroup_var = "financial_flow_link_strength")
    })
  })) %>%
  select(boundary_period, defn_phase_out, results) %>%
  mutate(sensitivity_id = paste0("bp", boundary_period, "_defn_po_", defn_phase_out))


##### 4.3.1. visualise changes in top commodity/countries, top countries, managers, regions, depending on the sensitivity analysis

plot_topN_sensitivity <- function(nested_df,
                                  group_var,
                                  top_n = 20,
                                  by_strength = FALSE,
                                  scenario_label = TRUE,
                                  value_var = "total_usd_m",
                                  rank_var = "rank_total") {
  
  # extract the relevant tibble from results
  df_long <- nested_df %>%
    transmute(boundary_period, defn_phase_out, data = map(results, ~ .x[[group_var]])) %>%
    unnest(data)
  
  # optionally add a scenario label
  df_long <- df_long %>%
    mutate(scenario = if (scenario_label)
      paste0("bp", boundary_period, "_defn_po_", defn_phase_out)
      else NA) %>%
    mutate(
      !!sym(group_var) := as.character(!!sym(group_var)),
      !!sym(group_var) := if_else(is.na(!!sym(group_var)), "Unknown", !!sym(group_var))
    )
  
  # top N per scenario using selected value variable
  value_sym <- sym(value_var)
  df_topN <- df_long %>%
    group_by(boundary_period, defn_phase_out) %>%
    slice_max(!!value_sym, n = top_n) %>%
    ungroup()
  
  # Rank change plot
  rank_sym <- sym(rank_var)
  rank_plot <- ggplot(df_topN,
                      aes(
                        x = scenario,
                        y = !!rank_sym,
                        group = !!sym(group_var),
                        color = !!sym(group_var)
                      )) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_text_repel(
      data = df_topN %>%
        group_by(!!sym(group_var)) %>%
        filter(scenario == min(scenario)),
      aes(label = !!sym(group_var)),
      nudge_x = 0.1,
      nudge_y = 0.1,
      size = 2.5,
      show.legend = FALSE
    ) +
    scale_y_reverse(breaks = 1:10, expand = expansion(mult = c(0.05, 0.05))) +
    labs(
      x = "Scenario",
      y = "Rank",
      title = paste0(str_to_sentence(group_var)," rank by ", str_to_sentence(value_var))
    ) +
    theme_minimal(base_size = 9) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 90, hjust = 1),
      axis.title = element_text(size = 9)
    )
  
  list(rank_plot = rank_plot, data = df_topN)
}

output_dir <- "./figures/sensitivity_analysis"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (var_ in vars_grouping) {
  
  # Total flows
  res_total <- plot_topN_sensitivity(
    nested_df = flows_all_by_grouping_vars %>% filter(!defn_phase_out),
    group_var = var_,
    top_n = 10,
    by_strength = FALSE,
    scenario_label = TRUE,
    value_var = "total_usd_m",
    rank_var = "rank_total"
  )
  
  print(res_total$rank_plot)
  
  ggsave(
    filename = file.path(output_dir, paste0("rank_plot_total_", var_, ".png")),
    plot = res_total$rank_plot,
    width = 6, height = 4, units = "in", dpi = 300
  )
  
  # Annualized flows
  res_annual <- plot_topN_sensitivity(
    nested_df = flows_all_by_grouping_vars %>% filter(!defn_phase_out),
    group_var = var_,
    top_n = 10,
    by_strength = FALSE,
    scenario_label = TRUE,
    value_var = "total_usd_m_yr",
    rank_var = "rank_yr"
  )
  
  print(res_annual$rank_plot)
  
  ggsave(
    filename = file.path(output_dir, paste0("rank_plot_annual_", var_, ".png")),
    plot = res_annual$rank_plot,
    width = 6, height = 4, units = "in", dpi = 300
  )
}

#### 4.4. geographic analysis ------------

eu_countries <- c(
  "Austria",
  "Belgium",
  "Bulgaria",
  "Croatia",
  "Cyprus",
  "Czech Republic",
  "Denmark",
  "Estonia",
  "Finland",
  "France",
  "Germany",
  "Greece",
  "Hungary",
  "Ireland; Republic of",
  "Ireland",
  "Italy",
  "Latvia",
  "Lithuania",
  "Luxembourg",
  "Malta",
  "Netherlands",
  "Poland",
  "Portugal",
  "Romania",
  "Slovakia",
  "Slovak Republic",
  "Slovenia",
  "Spain",
  "Sweden"
)

##### 4.4.1. simple column plots ------------
create_country_col_plot <- function(data,
                                    case,
                                    y_label_position_change,
                                    # 60000/60000 for Amazon/Indonesia
                                    y_label_hjust,
                                    # 12000/1000 for Amazon/Indonesia
                                    y_breaks,
                                    y_max,
                                    plot_title,
                                    top_n) {
  df <- data
  
  plot_data <- df %>%
    mutate(
      manager_true_ultimate_parent_country_of_headquarters = if_else(
        is.na(manager_true_ultimate_parent_country_of_headquarters),
        "Unknown",
        manager_true_ultimate_parent_country_of_headquarters
      )
    ) %>%
    group_by(manager_true_ultimate_parent_country_of_headquarters) %>%
    summarise(
      amount_usd_m = sum(
        tranche_amount_per_manager_usd_m_final_in_2024_av_usd,
        na.rm = TRUE
      )
    ) %>%
    ungroup() %>%
    arrange(desc(amount_usd_m)) %>%
    mutate(rank = dense_rank(desc(amount_usd_m)), pct = amount_usd_m / sum(amount_usd_m))
  
  write_csv(plot_data,
            paste0("./figures/flows_by_country_col_chart_data_", case, ".csv"))
  
  plot_data <- plot_data %>%
    filter(rank <= top_n) %>%
    ungroup() %>%
    left_join(
      regions_unsd,
      by = c("manager_true_ultimate_parent_country_of_headquarters" = "country")
    ) %>%
    mutate(region_unsd = if_else(is.na(region_unsd), "Unknown", region_unsd)) # correct for NA regions
  
  plot <- ggplot(plot_data,
                 aes(
                   x = reorder(
                     manager_true_ultimate_parent_country_of_headquarters,
                     desc(rank)
                   ),
                   y = amount_usd_m,
                   fill = region_unsd
                 )) +
    geom_col() +
    geom_text(aes(
      label = paste0(percent(pct, accuracy = 0.1)),
      y = if_else(
        amount_usd_m > y_label_position_change,
        (amount_usd_m - y_label_hjust),
        (amount_usd_m + y_label_hjust)
      )
    ), size = 3) +
    theme_minimal() +
    scale_fill_manual(
      values = c(
        "Asia" = "#FC8D62",
        "Europe" = "#8DA0CB",
        "Latin America and the Caribbean" = "#E78AC3",
        "Northern America" = "#A6D854",
        "Oceania" = "#FFD92F",
        "Unknown" = "#E5C494"
      )
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0, 0.04)),
      limits = c(0, y_max),
      breaks = seq(0, y_max, y_breaks)
    ) +
    scale_x_discrete(expand = expansion(mult = c(0))) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid = element_line(colour = "grey70"),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.title.y = element_text(size = 12, colour = "black"),
      axis.title.x = element_text(size = 12, colour = "black"),
      legend.position = c(0.75, 0.3)
    ) +
    labs(
      x = NULL,
      y = "Flows (2024 USDm)",
      fill = "Region",
      title = plot_title
    ) +
    coord_flip()
  
  print(plot)
}

create_country_col_plot_w_alpha <- function(data,
                                            case,
                                            y_label_position_change,
                                            # 60000/60000 for Amazon/Indonesia
                                            y_label_hjust,
                                            # 12000/1000 for Amazon/Indonesia
                                            y_breaks,
                                            y_max,
                                            plot_title,
                                            top_n,
                                            alpha_var,
                                            alpha_var_order,
                                            alpha_values,
                                            analytical_variable,
                                            y_lab) {
  df <- data
  
  total_flows_for_this_df <- sum(pull(df, !!sym(analytical_variable)), na.rm = TRUE)
  
  plot_data <- df %>%
    mutate(
      manager_true_ultimate_parent_country_of_headquarters = if_else(
        is.na(manager_true_ultimate_parent_country_of_headquarters),
        "Unknown",
        manager_true_ultimate_parent_country_of_headquarters
      )
    ) %>%
    group_by(manager_true_ultimate_parent_country_of_headquarters,
             !!sym(alpha_var)) %>%
    summarise(amount_usd_m_alpha_var = sum(!!sym(analytical_variable), na.rm = TRUE),
              .groups = "drop") %>%
    group_by(manager_true_ultimate_parent_country_of_headquarters) %>%
    mutate(amount_usd_m = sum(amount_usd_m_alpha_var, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(amount_usd_m)) %>%
    mutate(rank = dense_rank(desc(amount_usd_m)),
           pct = amount_usd_m / total_flows_for_this_df) %>%
    group_by(manager_true_ultimate_parent_country_of_headquarters) %>%
    mutate(!!sym(alpha_var) := factor(!!sym(alpha_var), levels = alpha_var_order)) %>%
    arrange(manager_true_ultimate_parent_country_of_headquarters,
            !!sym(alpha_var))
  
  write_csv(
    plot_data,
    paste0(
      "./figures/flows_by_country_col_chart_data_by_",
      alpha_var,
      "_",
      case,
      ".csv"
    )
  )
  
  plot_data <- plot_data %>%
    filter(rank <= top_n) %>%
    ungroup() %>%
    left_join(
      regions_unsd,
      by = c("manager_true_ultimate_parent_country_of_headquarters" = "country")
    ) %>%
    mutate(region_unsd = if_else(is.na(region_unsd), "Unknown", region_unsd)) # correct for NA regions
  
  plot <- ggplot(
    plot_data,
    aes(
      x = reorder(
        manager_true_ultimate_parent_country_of_headquarters,
        desc(rank)
      ),
      y = amount_usd_m_alpha_var,
      fill = region_unsd,
      alpha = !!sym(alpha_var)
    )
  ) +
    geom_col() +
    geom_text(aes(
      label = paste0(percent(pct, accuracy = 0.1)),
      y = if_else(
        amount_usd_m > y_label_position_change,
        (amount_usd_m - y_label_hjust),
        (amount_usd_m + y_label_hjust)
      )
    ), size = 3) +
    theme_minimal() +
    scale_fill_manual(
      values = c(
        "Asia" = "#FC8D62",
        "Europe" = "#8DA0CB",
        "Latin America and the Caribbean" = "#E78AC3",
        "Northern America" = "#A6D854",
        "Oceania" = "#FFD92F",
        "Unknown" = "#E5C494"
      )
    ) +
    scale_alpha_manual(values = alpha_values) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0, 0.04)),
      limits = c(0, y_max),
      breaks = seq(0, y_max, y_breaks)
    ) +
    scale_x_discrete(expand = expansion(mult = c(0))) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid = element_line(colour = "grey70"),
      axis.text.x = element_text(size = 10, colour = "black"),
      axis.text.y = element_text(size = 10, colour = "black"),
      axis.title.y = element_text(size = 12, colour = "black"),
      axis.title.x = element_text(size = 12, colour = "black"),
      legend.position.inside = c(0.75, 0.3)
    ) +
    labs(
      x = NULL,
      y = y_lab,
      fill = "Region",
      title = plot_title,
      alpha = str_to_title(alpha_var)
    ) +
    coord_flip()
  
  print(plot)
}

## these need adjusting for different loops possibly
country_col_plot_params <- tribble(
  ~ country_commodity,
  ~ y_label_position_change,
  ~ y_label_hjust,
  ~ y_breaks,
  ~ y_max,
  "bolivia_soya_beans",
  20000,
  2000,
  20000,
  40000,
  "brazil_cattle_meat",
  12000,
  1500,
  12500,
  25000,
  "brazil_oil_palm_fruit",
  5000,
  500,
  4000,
  8000,
  "brazil_soya_beans",
  100000,
  15000,
  150000,
  300000,
  "brazil_sugar_cane",
  40000,
  7500,
  50000,
  125000,
  "colombia_cattle_meat",
  2500,
  300,
  3000,
  6000,
  "colombia_coffee_green",
  7500,
  2000,
  10000,
  25000,
  "ecuador_cocoa_beans",
  40000,
  5000,
  50000,
  100000,
  "peru_cocoa_beans",
  1000,
  250,
  2000,
  4000,
  "peru_coffee_green",
  15000,
  2000,
  20000,
  35000
)

# without alpha - country plots
for (country_commodity in names(flows)) {
  for (period_ in boundary_periods) {
    for (defn_status_ in deforestation_phase_out_status) {
      print(
        paste0(
          country_commodity,
          ": ",
          as.character(period_),
          " yrs around SEI-Trase dates - defn phase out included: ",
          defn_status_
        )
      )
      df <- flows[[country_commodity]]
      df <- df %>%
        filter(modulus(year_relative_to_trase_period) <= period_) %>%
        filter(remove_flow_based_on_phase_out == defn_status_)
      
      if (nrow(df) > 0) {
        plot_name <- paste0(
          "plot_",
          "boundary_period_",
          period_,
          "_defn_po_",
          defn_status_,
          "_",
          country_commodity
        )
        
        params <- country_col_plot_params %>%
          filter(country_commodity == !!country_commodity)
        
        plot <- create_country_col_plot(
          df,
          case = country_commodity,
          y_label_position_change = params$y_label_position_change,
          y_label_hjust = params$y_label_hjust,
          y_breaks = params$y_breaks,
          y_max = params$y_max,
          plot_title = country_commodity,
          top_n = 10
        )
        assign(plot_name, plot, envir = .GlobalEnv)
      } else {
        print(paste0("No financial flows for ", country_commodity))
      }
    }
  }
}

plot_boundary_period_3_defn_po_FALSE_bolivia_soya_beans +
  plot_boundary_period_3_defn_po_FALSE_brazil_cattle_meat +
  plot_boundary_period_3_defn_po_FALSE_brazil_oil_palm_fruit +
  plot_boundary_period_3_defn_po_FALSE_brazil_soya_beans +
  plot_boundary_period_3_defn_po_FALSE_brazil_sugar_cane +
  plot_boundary_period_3_defn_po_FALSE_colombia_cattle_meat +
  plot_boundary_period_3_defn_po_FALSE_colombia_coffee_green +
  plot_boundary_period_3_defn_po_FALSE_ecuador_cocoa_beans +
  plot_boundary_period_3_defn_po_FALSE_peru_cocoa_beans +
  plot_boundary_period_3_defn_po_FALSE_peru_coffee_green +
  patchwork::plot_layout(ncol = 5, guides = "collect")

ggsave(
  './figures/flows_by_country_rough_plot_boundary_period_3_defn_po_FALSE.pdf',
  height = 10,
  width = 20
)

# with alpha - test for 2010-2022 version
for (country_commodity in names(flows)) {
  df <- flows[[country_commodity]]
  df <- df %>% filter(year >= 2010 & year <= 2022)
  if (nrow(df) > 0) {
    plot_name <- paste0("plot_", country_commodity)
    
    params <- country_col_plot_params %>%
      filter(country_commodity == !!country_commodity)
    
    plot <- create_country_col_plot_w_alpha(
      df,
      case = country_commodity,
      y_label_position_change = params$y_label_position_change,
      y_label_hjust = params$y_label_hjust,
      y_breaks = params$y_breaks,
      y_max = params$y_max,
      plot_title = country_commodity,
      top_n = 10,
      alpha_var = "financial_flow_link_strength",
      alpha_var_order = c("Low", "Medium", "High"),
      alpha_values = c(
        "High" = 1.0,
        "Medium" = 0.75,
        "Low" = 0.5
      )
    )
    assign(plot_name, plot, envir = .GlobalEnv)
  } else {
    print(paste0("No financial flows for ", country_commodity))
  }
}

plot_bolivia_soya_beans +
  plot_brazil_cattle_meat +
  plot_brazil_oil_palm_fruit +
  plot_brazil_soya_beans +
  plot_brazil_sugar_cane +
  plot_colombia_cattle_meat +
  plot_colombia_coffee_green +
  plot_ecuador_cocoa_beans +
  plot_peru_cocoa_beans +
  plot_peru_coffee_green +
  patchwork::plot_layout(ncol = 5, guides = "collect")

ggsave(
  './figures/flows_by_country_rough_plot_2010-2022_w_alpha.pdf',
  height = 10,
  width = 20
)

##### 4.4.2. simple col plots with commodities grouped (i.e., Brazil and Colombia cattle together) ----------
col_plot_params <- tribble(
  ~ commodity,
  ~ y_label_position_change,
  ~ y_label_hjust,
  ~ y_breaks,
  ~ y_max,
  "cattle_meat",
  0,
  0,
  2500,
  2500,
  "cocoa_beans",
  0,
  0,
  12000,
  12000,
  "coffee_green",
  0,
  0,
  4000,
  4000,
  "oil_palm",
  0,
  0,
  1500,
  1500,
  "soya_beans",
  0,
  0,
  25000,
  25000,
  "sugar_cane",
  0,
  0,
  13000,
  13000
)

for (commodity_ in names(flows_by_commodity)) {
  df <- flows_by_commodity[[commodity_]]
  df <- df %>% filter(modulus(year_relative_to_trase_period) == 0) # intensive boundary
  if (nrow(df) > 0) {
    plot_name <- paste0("plot_", commodity_)
    
    params <- col_plot_params %>%
      filter(commodity == !!commodity_)
    
    plot <- create_country_col_plot_w_alpha(
      df,
      case = commodity_,
      y_label_position_change = params$y_label_position_change,
      y_label_hjust = params$y_label_hjust,
      y_breaks = params$y_breaks,
      y_max = params$y_max,
      analytical_variable = "tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average",
      plot_title = commodity_,
      top_n = 10,
      alpha_var = "financial_flow_link_strength",
      alpha_var_order = c("Low", "Medium", "High"),
      alpha_values = c(
        "High" = 1.0,
        "Medium" = 0.7,
        "Low" = 0.4
      ),
      y_lab = "Financial flows US$m/y \n (Production-weighted annual average)"
    )
    assign(plot_name, plot, envir = .GlobalEnv)
  } else {
    print(paste0("No financial flows for ", commodity_))
  }
}

plot_cattle_meat +
  plot_cocoa_beans +
  plot_coffee_green +
  plot_oil_palm +
  plot_soya_beans +
  plot_sugar_cane +
  patchwork::plot_layout(ncol = 2, guides = "collect")

ggsave(
  "./figures/plot_by_commodity_top_countries.pdf",
  height = 13,
  width = 11
)

##### 4.4.3. data for commodity-level Sankey plots (then generated in Graphica) -----------
# For Sankeys, we want financing country --> receiving country, coloured or shaded by direct/indirect
# Because we are looking at structural trends, we use the data normalised to the SEI-Trase years available, to keep the breadth of data available while maintaining comparability
# Sensitivity test the data

# commodity level (duplicates adjusted by production volumes)
sankey_data_by_commodity <- commodity_param_grid %>%
  mutate(results = pmap(list(commodity_case, boundary_period, defn_phase_out), function(commodity_, period_, defn_status_) {
    message(
      commodity_,
      ": ",
      period_,
      " yrs around SEI-Trase dates - defn phase out included: ",
      defn_status_
    )
    
    df <- flows_by_commodity[[commodity_]] %>%
      filter(modulus(year_relative_to_trase_period) <= period_)
    
    if (defn_status_ == TRUE) {
      df <- df %>% filter(!remove_flow_based_on_phase_out == TRUE) # remove flows that have phased out (keep ones with FALSE for remove)
    }
    
    if (nrow(df) == 0) {
      message("No financial flows for ", commodity_)
      return(
        tibble(
          commodity_case = commodity_,
          boundary_period = period_,
          defn_phase_out = defn_status_,
          manager_country_grouped = character(),
          producer_country = character(),
          financial_flow_link_strength = character(),
          total_usd_m_normalised_to_sei_trase_by_strength = numeric(),
          total_usd_m_normalised_to_sei_trase = numeric(),
          manager_country_grouped_link_strength_pct = numeric()
        )
      )
    }
    # group EU together and mark NA as unknown
    df <- df %>%
      mutate(
        manager_true_ultimate_parent_country_of_headquarters_sankey = case_when(
          manager_true_ultimate_parent_country_of_headquarters %in% eu_countries ~ "EU27",
          is.na(manager_true_ultimate_parent_country_of_headquarters) ~ "Unknown",
          manager_true_ultimate_parent_country_of_headquarters == "United States of America" ~ "USA",
          manager_true_ultimate_parent_country_of_headquarters == "United Kingdom" ~ "UK",
          TRUE ~ manager_true_ultimate_parent_country_of_headquarters
        )
      )
    
    # get totals for ranking and showing only top ten for each commodity
    totals <- df %>%
      group_by(commodity,
               manager_true_ultimate_parent_country_of_headquarters_sankey) %>%
      summarise(
        total_usd_m_normalised_to_sei_trase = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      group_by(commodity) %>%
      mutate(rank = dense_rank(desc(total_usd_m_normalised_to_sei_trase))) %>%
      ungroup()
    
    top10 <- totals %>%
      group_by(commodity) %>%
      mutate(
        manager_country_grouped = if_else(
          rank <= 10,
          manager_true_ultimate_parent_country_of_headquarters_sankey,
          "Other countries"
        )
      ) %>%
      select(
        commodity,
        manager_true_ultimate_parent_country_of_headquarters_sankey,
        manager_country_grouped
      )
    
    sankey_data <- df %>%
      left_join(
        top10,
        by = c(
          "commodity",
          "manager_true_ultimate_parent_country_of_headquarters_sankey"
        )
      ) %>%
      # calculate flows by link strength, for aggregated groups
      group_by(
        commodity,
        producer_country,
        manager_country_grouped,
        financial_flow_link_strength
      ) %>%
      summarise(
        total_usd_m_normalised_to_sei_trase_by_strength = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      # add totals
      left_join(
        totals,
        by = c(
          "commodity" = "commodity",
          "manager_country_grouped" = "manager_true_ultimate_parent_country_of_headquarters_sankey"
        )
      ) %>%
      # calculate % high, medium, low for each financier country for that commodity
      group_by(commodity, producer_country, manager_country_grouped) %>%
      mutate(
        manager_country_grouped_link_strength_pct = total_usd_m_normalised_to_sei_trase_by_strength / total_usd_m_normalised_to_sei_trase
      ) %>%
      ungroup()
    
    return(sankey_data)
  })) %>%
  unnest(results)

write_csv(
  sankey_data_by_commodity,
  "./analytical-results/sankey_data_commodity_grouped_w_sensitivity_analysis.csv"
)

##### 4.4.3. data for all Amazon Sankey plot (then generated in Graphica) -----------
all_amazon_param_grid <- tibble(
  "boundary_period" = c(rep(0, 2), rep(1, 2), rep(2, 2), rep(3, 2)),
  "defn_phase_out" = c(rep(c(FALSE, TRUE), 4))
)

sankey_data_overall <- all_amazon_param_grid %>%
  mutate(results = pmap(list(boundary_period, defn_phase_out), function(period_, defn_status_) {
    message(period_,
            " yrs around SEI-Trase dates - defn phase out included: ",
            defn_status_)
    
    df <- flows_all_countries_commodities %>% # adjust for boundary period
      filter(modulus(year_relative_to_trase_period) <= period_)
    
    if (defn_status_ == TRUE) {
      df <- df %>% filter(!remove_flow_based_on_phase_out == TRUE) # remove flows that have phased out (keep ones with FALSE for remove)
    }
    
    if (nrow(df) == 0) {
      message("No financial flows for this spec")
      return(
        tibble(
          boundary_period = period_,
          defn_phase_out = defn_status_,
          manager_country_grouped = character(),
          producer_country = character(),
          financial_flow_link_strength = character(),
          total_usd_m_normalised_to_sei_trase_by_strength = numeric(),
          total_usd_m_normalised_to_sei_trase = numeric(),
          manager_country_grouped_link_strength_pct = numeric()
        )
      )
    }
    
    df <- flows_all_countries_commodities %>%
      mutate(
        manager_true_ultimate_parent_country_of_headquarters_sankey = case_when(
          manager_true_ultimate_parent_country_of_headquarters %in% eu_countries ~ "EU27",
          is.na(manager_true_ultimate_parent_country_of_headquarters) ~ "Unknown",
          manager_true_ultimate_parent_country_of_headquarters == "United States of America" ~ "USA",
          manager_true_ultimate_parent_country_of_headquarters == "United Kingdom" ~ "UK",
          TRUE ~ manager_true_ultimate_parent_country_of_headquarters
        )
      )
    
    totals_by_commodity <- df %>%
      group_by(commodity,
               manager_true_ultimate_parent_country_of_headquarters_sankey) %>%
      summarise(
        total_usd_m_normalised_to_sei_trase = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      group_by(commodity) %>%
      mutate(rank = dense_rank(desc(total_usd_m_normalised_to_sei_trase))) %>%
      ungroup()
    
    top10_by_commodity <- totals_by_commodity %>% # top ten financing countries for each commodity shown, rest attributed as 'other'
      group_by(commodity) %>%
      mutate(
        manager_country_grouped = if_else(
          rank <= 10,
          manager_true_ultimate_parent_country_of_headquarters_sankey,
          "Other countries"
        )
      ) %>%
      select(
        commodity,
        manager_true_ultimate_parent_country_of_headquarters_sankey,
        manager_country_grouped
      )
    
    totals_overall <- df %>%
      group_by(manager_true_ultimate_parent_country_of_headquarters_sankey) %>%
      summarise(
        total_usd_m_normalised_to_sei_trase = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      mutate(rank_overall = dense_rank(desc(total_usd_m_normalised_to_sei_trase))) %>%
      ungroup()
    
    top10_overall <- totals_overall %>%
      mutate(
        manager_country_grouped = if_else(
          rank_overall <= 10,
          manager_true_ultimate_parent_country_of_headquarters_sankey,
          "Other countries"
        )
      ) %>%
      select(
        manager_true_ultimate_parent_country_of_headquarters_sankey,
        manager_country_grouped
      )
    
    sankey_data <- df %>% # only show top ten overall
      left_join(
        top10_overall,
        by = c(
          "manager_true_ultimate_parent_country_of_headquarters_sankey"
        )
      ) %>%
      # calculate flows by link strength, for aggregated groups
      group_by(
        commodity,
        producer_country,
        manager_country_grouped,
        financial_flow_link_strength
      ) %>%
      summarise(
        total_usd_m_normalised_to_sei_trase_by_strength = sum(
          tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised,
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      # add totals
      left_join(
        totals_overall,
        by = c(# note that this won't return a total for 'other'
          "manager_country_grouped" = "manager_true_ultimate_parent_country_of_headquarters_sankey")
      ) %>%
      # calculate % high, medium, low for each financier country for that commodity
      group_by(commodity, producer_country, manager_country_grouped) %>%
      mutate(
        manager_country_grouped_link_strength_pct = total_usd_m_normalised_to_sei_trase_by_strength / total_usd_m_normalised_to_sei_trase
      ) %>%
      ungroup()
    return(sankey_data)
  })) %>%
  unnest(results)
write_csv(sankey_data_overall, "./analytical-results/sankey_data_all_amazon_w_sensitivity_analysis.csv")


### 4.6. financial actors ------------
##### 4.6.1. simple league table --------------

create_manager_league_table_w_fill <- function(data,
                                               # includes government-owned entities marked with a *
                                               fill_option,
                                               case,
                                               n_shown,
                                               y_label_position_change,
                                               # shift labels from inside cols to outside cols
                                               y_label_hjust,
                                               # position of labels
                                               y_max,
                                               y_lab,
                                               y_breaks,
                                               analytical_variable) {
  plot_data <- data %>%
    group_by(manager_true_ultimate_parent_organisation_name) %>%
    summarise(amount_usd_m = sum(!!sym(analytical_variable), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(amount_usd_m)) %>%
    mutate(rank = dense_rank(desc(amount_usd_m)), prop = amount_usd_m / sum(amount_usd_m)) %>%
    filter(rank <= n_shown) %>%
    # join on additional info
    left_join((
      data %>%
        distinct(
          manager_true_ultimate_parent_organisation_name,
          .keep_all = TRUE
        ) %>%
        select(
          manager_true_ultimate_parent_organisation_name,!!sym(fill_option),
          government_ultimate_parent
        )
    ),
    by = c("manager_true_ultimate_parent_organisation_name")
    ) %>%
    # tag government ultimate parent
    # handling "Not Applicable" as managers (self-arranged)
    mutate(
      government_ultimate_parent = if_else(
        is.na(government_ultimate_parent),
        FALSE,
        government_ultimate_parent
      ),
      manager_true_ultimate_parent_organisation_name = case_when(
        government_ultimate_parent == TRUE ~ paste0(manager_true_ultimate_parent_organisation_name, "*"),
        is.na(manager_true_ultimate_parent_organisation_name) ~ "Unknown",
        grepl("DZ BANK", manager_true_ultimate_parent_organisation_name) ~ "DZ Bank AG",
        manager_true_ultimate_parent_organisation_name == "Not Applicable" ~ "Self-arranged",
        TRUE ~ manager_true_ultimate_parent_organisation_name
      )
    )
  
  write_csv(
    plot_data,
    paste0(
      "./figures/",
      "/flows_by_manager_",
      to_file_save_format(case),
      "_",
      ".csv"
    )
  )
  
  plot <- plot_data %>%
    ggplot(aes(
      x = reorder(
        manager_true_ultimate_parent_organisation_name,
        desc(rank)
      ),
      y = amount_usd_m,
      label = percent(prop, accuracy = 0.1),
      fill = !!sym(fill_option)
    )) +
    geom_col() +
    geom_text(aes(
      label = percent(prop, accuracy = 0.1),
      y = if_else(
        amount_usd_m > y_label_position_change,
        (amount_usd_m - y_label_hjust),
        (amount_usd_m + y_label_hjust)
      )
    ), size = 3) +
    scale_fill_manual(
      values = c(
        "Asia" = "#FC8D62",
        "Europe" = "#8DA0CB",
        "Latin America and the Caribbean" = "#E78AC3",
        "Northern America" = "#A6D854",
        "Oceania" = "#FFD92F",
        "Unknown" = "#E5C494"
      )
    ) +
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0.01, 0.05)),
      breaks = seq(0, y_max, y_breaks)
    ) +
    scale_x_discrete(expand = expansion(mult = c(0))) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.title.y = element_text(size = 8, color = "black"),
      axis.title.x = element_text(size = 10, color = "black"),
      legend.position = "none",
      axis.line.x = element_line(color = "grey"),
      plot.title = element_text(
        hjust = 0.9,
        size = 10,
        color = "black"
      )
    ) +
    labs(
      x = NULL,
      y = y_lab,
      fill = "Region",
      title = case
    ) +
    coord_flip()
  
  print(plot)
}

league_table_plot_params <- tribble(
  ~ country_commodity,
  ~ y_label_position_change,
  ~ y_label_hjust,
  ~ y_breaks,
  ~ y_max,
  ~ y_lab,
  "bolivia_soya_beans",
  0,
  0,
  5000,
  9000,
  "Financial flows $m (SEI-Trase years)",
  "brazil_cattle_meat",
  0,
  0,
  5000,
  6500,
  "Financial flows $m (SEI-Trase years)",
  "brazil_oil_palm_fruit",
  0,
  0,
  2000,
  3500,
  "Financial flows $m (SEI-Trase years)",
  "brazil_soya_beans",
  0,
  0,
  50000,
  110000,
  "Financial flows $m (SEI-Trase years)",
  "brazil_sugar_cane",
  0,
  0,
  20000,
  45000,
  "Financial flows $m (SEI-Trase years)",
  "colombia_cattle_meat",
  0,
  0,
  2000,
  3000,
  "Financial flows $m (SEI-Trase years)",
  "colombia_coffee_green",
  0,
  0,
  10000,
  15000,
  "Financial flows $m (SEI-Trase years)",
  "ecuador_cocoa_beans",
  0,
  0,
  25000,
  50000,
  "Financial flows $m (SEI-Trase years)",
  "peru_cocoa_beans",
  0,
  0,
  500,
  1200,
  "Financial flows $m (SEI-Trase years)",
  "peru_coffee_green",
  0,
  0,
  10000,
  20000,
  "Financial flows $m (SEI-Trase years)"
)

for (country_commodity in names(flows)) {
  df <- flows[[country_commodity]] %>%
    filter(modulus(year_relative_to_trase_period) == 0)
  if (nrow(df) > 0) {
    plot_name <- paste0("plot_", country_commodity)
    
    params <- league_table_plot_params %>%
      filter(country_commodity == !!country_commodity)
    
    plot <- create_manager_league_table_w_fill(
      df,
      case = country_commodity,
      fill_option = "manager_true_ultimate_parent_region_of_headquarters_unsd",
      n_shown = 20,
      y_label_position_change = params$y_label_position_change,
      y_label_hjust = params$y_label_hjust,
      y_max = params$y_max,
      y_breaks = params$y_breaks,
      y_lab = params$y_lab,
      analytical_variable = "tranche_amount_per_manager_usd_m_final_in_2024_av_usd"
    )
    
    assign(plot_name, plot, envir = .GlobalEnv)
  } else {
    print(paste0("No financial flows for ", country_commodity))
  }
}

plot_bolivia_soya_beans +
  plot_brazil_cattle_meat +
  plot_brazil_oil_palm_fruit +
  plot_brazil_soya_beans +
  plot_brazil_sugar_cane +
  plot_colombia_cattle_meat +
  plot_colombia_coffee_green +
  plot_ecuador_cocoa_beans +
  plot_peru_cocoa_beans +
  plot_peru_coffee_green +
  patchwork::plot_layout(ncol = 5, guides = "collect")

ggsave(
  './figures/flows_by_manager_country_commodity_sei_trase_yrs.pdf',
  width = 20,
  height = 12
)

##### 4.6.2. China top FIs -------------

vertical_bar_chart_theme_elements <- function(df) {
  theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.title.y = element_text(size = 8, color = "black"),
      axis.title.x = element_text(size = 10, color = "black"),
      legend.position.inside = "none",
      axis.line.x = element_line(color = "grey"),
      plot.title = element_text(
        hjust = 0.9,
        size = 10,
        color = "black"
      )
    ) +
    coord_flip()
}

p1 <- create_manager_league_table_w_fill(
  flows_by_commodity[["soya_beans"]] %>% filter(modulus(year_relative_to_trase_period) ==
                                                  0),
  case = "soya_beans",
  n_shown = 20,
  y_label_position_change = 0,
  y_label_hjust = 0,
  y_max = 10000,
  y_breaks = 2000,
  y_lab = "Financial flows US$m/yr \n (production-weighted annual average)",
  fill_option = "manager_true_ultimate_parent_region_of_headquarters_unsd",
  analytical_variable = "tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average"
)

p2 <- create_manager_league_table_w_fill(
  flows_by_commodity[["sugar_cane"]] %>% filter(modulus(year_relative_to_trase_period) ==
                                                  0),
  case = "sugar_cane",
  n_shown = 20,
  y_label_position_change = 0,
  y_label_hjust = 0,
  y_max = 10000,
  y_breaks = 2000,
  y_lab = "Financial flows US$m/yr \n (production-weighted annual average)",
  fill_option = "manager_true_ultimate_parent_region_of_headquarters_unsd",
  analytical_variable = "tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average"
)

p3 <- create_manager_league_table_w_fill(
  flows_by_commodity[["oil_palm"]] %>% filter(modulus(year_relative_to_trase_period) ==
                                                0),
  case = "oil_palm",
  n_shown = 20,
  y_label_position_change = 0,
  y_label_hjust = 0,
  y_max = 10000,
  y_breaks = 2000,
  y_lab = "Financial flows US$m/yr \n (production-weighted annual average)",
  fill_option = "manager_true_ultimate_parent_region_of_headquarters_unsd",
  analytical_variable = "tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_commodity_annual_average"
)

p1 + p2 + p3 + patchwork::plot_layout(ncol = 1)
ggsave("./figures/china_top_fis.pdf",
       height = 14,
       width = 6)


##### 4.6.3. Final league table data (visualised in Graphica) -------------

by_fi <- map(flows_all_by_grouping_vars$results, "manager_true_ultimate_parent_organisation_name")
by_fi_by_strength <- map(flows_all_by_grouping_vars_by_strength$results, "manager_true_ultimate_parent_organisation_name")

names(by_fi) <- flows_all_by_grouping_vars$sensitivity_id
names(by_fi_by_strength) <- flows_all_by_grouping_vars_by_strength$sensitivity_id

# dictionary of govt ownership
government_owned_flags <- flows_all_countries_commodities %>%
  distinct(manager_true_ultimate_parent_organisation_name,
           government_ultimate_parent)

# dictionary of hqs
manager_hqs <- flows_all_countries_commodities %>%
  distinct(
    manager_true_ultimate_parent_organisation_name,
    manager_true_ultimate_parent_country_of_headquarters,
    manager_true_ultimate_parent_region_of_headquarters_unsd
  )

# generate league table data
fi_league_tables <- list() 

for (i in names(by_fi_by_strength)) { # where i is the sensitivity analysis
  df <- by_fi_by_strength[[i]]
  df_total <- by_fi[[i]]
  
  # join on totals
  df <- df %>%
    select(-ends_with("yr")) %>%
    # rename strength columns to only be this strength
    rename_with(~ paste0(.,"_this_strength"),
                c(total_usd_m, pct_total, rank_total)) %>%
    left_join(df_total %>% 
                select(manager_true_ultimate_parent_organisation_name,
                       total_usd_m,
                       pct_total,
                       rank_total))
    
  # join on government ownership and mark flag
  df <- df %>%
    left_join(government_owned_flags,
              by = "manager_true_ultimate_parent_organisation_name")
  
  # join on HQs
  df <- df %>%
    left_join(manager_hqs,
              by = "manager_true_ultimate_parent_organisation_name")
    
  # add * for government owned
  df <- df %>%
    mutate(fi_name_for_plot = if_else(
      !is.na(government_ultimate_parent) & government_ultimate_parent, # handle NAs
      paste0(manager_true_ultimate_parent_organisation_name, "*"),
      manager_true_ultimate_parent_organisation_name)
      )
  
  # consolidate ones below 30 into "other financial institutions"
  df <- df %>%
    mutate(fi_name_for_plot = if_else(
      rank_total <= 50,
      fi_name_for_plot,
      "Other financial institutions"
    )) %>%
    group_by(fi_name_for_plot, financial_flow_link_strength) %>%
    summarise(total_usd_m = sum(unique(total_usd_m)), # not double counting totals
              total_usd_m_this_strength = sum(total_usd_m_this_strength),
              .groups = "drop")
  
  # calculate link strength % for each manager (for bar labels)
  df <- df %>%
    group_by(fi_name_for_plot) %>%
    mutate(pct_this_strength_for_fi = percent(total_usd_m_this_strength / total_usd_m, accuracy = 1))
  
  # arrange and reorder so that undisclosed advisor goes at the bottom
  df <- df %>%
    mutate(
      fi_name_for_plot = fct_reorder(fi_name_for_plot, total_usd_m,.desc = TRUE),
      fi_name_for_plot = fct_relevel(fi_name_for_plot, "Undisclosed Advisor", "Other financial institutions", after = Inf)
    )
  
  fi_league_tables[[i]] <- df
  
  write_csv(df, paste0("./figures/league_tables/",i,".csv"))
}


##### old code for league table ------
manager_by_country_commodity <- flows_all_countries_commodities %>%
  filter(modulus(year_relative_to_trase_period) == 0) %>%
  mutate(country_commodity = str_c(producer_country, commodity, sep = " - ")) %>%
  group_by(manager_true_ultimate_parent_organisation_name,
           country_commodity) %>%
  summarise(
    total_usd_m_yr_this_country_commodity = sum(
      tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(manager_true_ultimate_parent_organisation_name) %>%
  mutate(
    prop_this_country_commodity = percent(
      total_usd_m_yr_this_country_commodity / sum(total_usd_m_yr_this_country_commodity),
      accuracy = 1
    )
  )

manager_by_strength <- flows_all_countries_commodities %>%
  filter(modulus(year_relative_to_trase_period) == 0) %>%
  mutate(country_commodity = str_c(producer_country, commodity, sep = " - ")) %>%
  group_by(manager_true_ultimate_parent_organisation_name,
           financial_flow_link_strength) %>%
  summarise(
    total_usd_m_yr_this_strength = sum(
      tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  group_by(manager_true_ultimate_parent_organisation_name) %>%
  mutate(prop_this_strength = percent(
    total_usd_m_yr_this_strength / sum(total_usd_m_yr_this_strength),
    accuracy = 1
  ))

league_table_all_data <- flows_all_countries_commodities %>%
  filter(modulus(year_relative_to_trase_period) == 0) %>%
  mutate(country_commodity = str_c(producer_country, commodity, sep = " - ")) %>%
  group_by(
    manager_true_ultimate_parent_organisation_name,
    country_commodity,
    financial_flow_link_strength
  ) %>%
  summarise(
    total_usd_m_yr_this_strength = sum(
      tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates_annualised,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  # join on totals, ranks, hqs, and ownership status
  left_join(manager_totals,
            by = c("manager_true_ultimate_parent_organisation_name")) %>%
  left_join(
    manager_by_strength %>% select(
      manager_true_ultimate_parent_organisation_name,
      financial_flow_link_strength,
      prop_this_strength
    ),
    by = c(
      "manager_true_ultimate_parent_organisation_name",
      "financial_flow_link_strength"
    )
  ) %>%
  left_join(
    manager_by_country_commodity %>% select(
      manager_true_ultimate_parent_organisation_name,
      country_commodity,
      prop_this_country_commodity
    ),
    by = c(
      "manager_true_ultimate_parent_organisation_name",
      "country_commodity"
    )
  ) %>%
  left_join(government_owned_flag,
            by = c("manager_true_ultimate_parent_organisation_name")) %>%
  left_join(manager_hqs,
            by = c("manager_true_ultimate_parent_organisation_name")) %>%
  arrange(desc(total_usd_m_yr_overall))

write_csv(
  league_table_all_data,
  "./analytical-results/league_table_all_data_bp_0_po_FALSE.csv"
)

##### 4.6.4. Comparison with Brazilian Rural Credit Scheme -------------
brazil_rcs_filter <- expr(year >= 2020 & year <= 2024 &
                        producer_country == "Brazil" &
                        commodity %in% c("Cattle meat", "Soy", "Oil palm fruit"))

flows_all_countries_commodities %>% filter(!!brazil_rcs_filter) %>% # no oil palm flows after 2019
  group_by(financial_flow_link_strength) %>%
  summarise(total = sum(tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates, na.rm = TRUE))

flows_all_countries_commodities %>% filter(!!brazil_rcs_filter) %>%
  group_by(flow_financed_location_type) %>%
  summarise(total = sum(tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates, na.rm = TRUE))

flows_all_countries_commodities %>% filter(!!brazil_rcs_filter) %>%
  group_by(financial_flow_link_strength, flow_financed_location_type) %>%
  summarise(total = sum(tranche_amount_per_manager_usd_m_final_in_2024_av_usd_adjusted_for_duplicates, na.rm = TRUE))
