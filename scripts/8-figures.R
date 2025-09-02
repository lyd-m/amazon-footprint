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

### 0. FUNCTIONS --------------------------
to_file_save_format <- function(str) {
  file_save_format_string <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", str, perl = TRUE)
  file_save_format_string <- gsub(" ", "-", tolower(file_save_format_string))
  return(file_save_format_string)
}

modulus <- function(x) {
  sqrt(x^2)
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
# simple descriptive plots to start with
boundary_periods <- c(0, 1, 2, 3)

#### 4.1. Combined datasets ---------------
##### 4.1.1. Commodity dataset (i.e., combine same commodities from different countries) ---------------------------
# bind all data together for same commodities
# duplicate transactions are split between them based on relative production volumes between those countries for that year
# start with doing SEI-Trase simple, no phase out

all_flows_simple_bind <- tibble()
for (country_commodity in names(flows)) {
  df <- flows[[country_commodity]] %>%
    filter(modulus(year_relative_to_trase_period) <= max(boundary_periods)) %>%
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
      tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity =
        tranche_amount_per_manager_usd_m_final_in_dec_2024_usd * (production_volume_commodity_this_yr / sum(
          unique(production_volume_commodity_this_yr)
        ))
    ) %>%
    ungroup()
}

flows_cattle_meat_all <- all_flows_simple_bind %>%
  parse_by_production_volumes(commodity_filter = "Cattle meat")

(sum((
  flows_cattle_meat_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
)$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd
) -
    sum((
      flows_cattle_meat_all$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity
    )
    )) / sum((
      flows_cattle_meat_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
    )$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd
    )

flows_cocoa_beans_all <- all_flows_simple_bind %>%
  parse_by_production_volumes(commodity_filter = "Cocoa beans")

(sum((
  flows_cocoa_beans_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
)$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd
) -
    sum((
      flows_cocoa_beans_all$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity
    )
    )) / sum((
      flows_cocoa_beans_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
    )$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd
    )

flows_coffee_green_all <- all_flows_simple_bind %>%
  parse_by_production_volumes(commodity_filter = "Coffee, green")

(sum((
  flows_coffee_green_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
)$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd
) -
    sum((
      flows_coffee_green_all$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity
    )
    )) / sum((
      flows_coffee_green_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
    )$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd
    )

flows_oil_palm_all <- all_flows_simple_bind %>%
  parse_by_production_volumes(commodity_filter = "Oil palm fruit")

(sum((
  flows_oil_palm_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
)$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd
) -
    sum((
      flows_oil_palm_all$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity
    )
    )) / sum((
      flows_oil_palm_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
    )$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd
    )

flows_soya_beans_all <- all_flows_simple_bind %>%
  parse_by_production_volumes(commodity_filter = "Soya beans")

(
  sum((
    flows_soya_beans_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
  )$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd,
  na.rm = TRUE
  ) -
    sum((
      flows_soya_beans_all$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity
    ),
    na.rm = TRUE
    )
) / sum((
  flows_soya_beans_all %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
)$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd,
na.rm = TRUE
)

flows_sugar_cane_all <- all_flows_simple_bind %>%
  parse_by_production_volumes(commodity_filter = "Sugar cane")

# create list of commodity datasets

flows_by_commodity <- list(
  "cattle_meat" = flows_cattle_meat_all,
  "cocoa_beans" = flows_cocoa_beans_all,
  "coffee_green" = flows_coffee_green_all,
  "oil_palm" = flows_oil_palm_all,
  "soya_beans" = flows_soya_beans_all,
  "sugar_cane" = flows_sugar_cane_all
)

# annualising the data to account for number of SEI-TRASE years available (bc the financial data spans different time periods)
# this allows a structural comparison (i.e., who are the most important countries financing these companies over the available period of SEI-trase data?)
# so we normalise to the number of years of trase data that's available to make it more comparable - longer time periods see their flows reduced by more.
for (commodity_ in names(flows_by_commodity)) {
  df <- flows_by_commodity[[commodity_]]
  df <- df %>%
    mutate(
      sei_trase_years_available = sei_trase_data_max_year - sei_trase_data_min_year + 1,
      # add one since inclusive (e.g., 2015, 2016, 2017)
      # N.B: that as this stands it does not account for the boundary period
      tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average = tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity / sei_trase_years_available
    )
  flows_by_commodity[[commodity_]] <- df
}

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
      tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates =
        tranche_amount_per_manager_usd_m_final_in_dec_2024_usd * (production_value_country_commodity_this_yr / sum(
          unique(production_value_country_commodity_this_yr)
        ))
    ) %>%
    ungroup()
}

flows_all_countries_commodities <- all_flows_simple_bind %>%
  parse_by_production_value()

# check the calculation is consistent (i.e., deals are actually split)
(
  sum((
    flows_all_countries_commodities %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
  )$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd,
  na.rm = TRUE
  ) -
    sum((
      flows_all_countries_commodities$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates
    ),
    na.rm = TRUE
    )
) / sum((
  flows_all_countries_commodities %>% distinct(tranche_id, manager_name, .keep_all = TRUE)
)$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd,
na.rm = TRUE
) # only rounding error worth of difference

# annualising the data to account for number of SEI-TRASE years available (bc the financial data spans different time periods)
# this allows a structural comparison (i.e., who are the most important countries financing these companies over the available period of SEI-trase data?)
# so we normalise to the number of years of trase data that's available to make it more comparable - longer time periods see their flows reduced by more.
flows_all_countries_commodities <- flows_all_countries_commodities %>%
  mutate(
    sei_trase_years_available = sei_trase_data_max_year - sei_trase_data_min_year + 1,
    # add one since inclusive (e.g., 2015, 2016, 2017)
    # N.B: that as this stands it does not account for the boundary period, just the SEI-Trase years
    tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates_annualised = tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates / sei_trase_years_available
  )

# save version in intermediate results
write_csv(
  flows_all_countries_commodities,
  "./intermediate-results/flows_all_countries_commodities.csv"
)


#### 4.2. Setting up sensitivity testing --------------------------
## boundary period (0-3 years)
## deforestation phase out or not
country_commodity_dict <- tibble()
for (country_commodity_ in names(flows)) {
  df <- flows[[country_commodity_]]
  df <- df %>%
    distinct(producer_country, commodity) %>%
    mutate(country_commodity = country_commodity_) %>%
    select(country_commodity, everything())
  
  country_commodity_dict <- bind_rows(country_commodity_dict, df)
}

boundary_periods <- c(0, 1, 2, 3)
deforestation_phase_out_status <- c(FALSE, TRUE) # if true, remove financial flows after the year deforestation has been phased out

country_commodity_param_grid <- tibble(country_commodity_case = names(flows)) %>%
  crossing(boundary_period = boundary_periods, defn_phase_out = deforestation_phase_out_status)

commodity_param_grid <- tibble(commodity_case = names(flows_by_commodity)) %>%
  crossing(boundary_period = boundary_periods, defn_phase_out = deforestation_phase_out_status)

## then can use purrr pmap function to do this for the different cases

#### 4.2. Total financial flows (overall for country-commodity, normalised for commodity) ------------
flows_country_commodity_totals <- country_commodity_param_grid %>%
  mutate(results = pmap(list(country_commodity_case, boundary_period, defn_phase_out), function(country_commodity, period_, defn_status_) {
    message(
      country_commodity,
      ": ",
      period_,
      " yrs around SEI-Trase dates - defn phase out included: ",
      defn_status_
    )
    
    df <- flows[[country_commodity]] %>%
      filter(modulus(year_relative_to_trase_period) <= period_)
    
    if (defn_status_ == TRUE) {
      df <- df %>% filter(!remove_flow_based_on_phase_out == TRUE) # remove flows that have phased out (keep ones with FALSE for remove)
    }
    
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
          tranche_amount_per_manager_usd_m_final_in_dec_2024_usd[financial_flow_link_strength == "High"],
          na.rm = TRUE
        ),
        total_usd_m_medium_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_dec_2024_usd[financial_flow_link_strength == "Medium"],
          na.rm = TRUE
        ),
        total_usd_m_low_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_dec_2024_usd[financial_flow_link_strength == "Low"],
          na.rm = TRUE
        )
      ) %>%
      mutate(
        total_usd_m = sum(
          total_usd_m_high_conf,
          total_usd_m_medium_conf,
          total_usd_m_low_conf,
          na.rm = TRUE
        )
      )
  })) %>%
  unnest(results)

write_csv(
  flows_country_commodity_totals,
  "./analytical-results/flows_totals_country_commodity_w_boundary_periods_po_conf_levels.csv"
)

# do the same for each commodity (noting that this will be normalised to the number of SEI-Trase years available)
flows_commodity_totals <- commodity_param_grid %>%
  mutate(results = pmap(list(commodity_case, boundary_period, defn_phase_out), function(commodity, period_, defn_status_) {
    message(
      commodity,
      ": ",
      period_,
      " yrs around SEI-Trase dates - defn phase out included: ",
      defn_status_
    )
    
    df <- flows_by_commodity[[commodity]] %>%
      filter(modulus(year_relative_to_trase_period) <= period_)
    
    if (defn_status_ == TRUE) {
      df <- df %>% filter(!remove_flow_based_on_phase_out == TRUE) # remove flows that have phased out (keep ones with FALSE for remove)
    }
    
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
        total_usd_m_sei_trase_normalised_high_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average[financial_flow_link_strength == "High"],
          na.rm = TRUE
        ),
        total_usd_m_sei_trase_normalised_medium_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average[financial_flow_link_strength == "Medium"],
          na.rm = TRUE
        ),
        total_usd_m_sei_trase_normalised_low_conf = sum(
          tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average[financial_flow_link_strength == "Low"],
          na.rm = TRUE
        )
      ) %>%
      mutate(
        total_usd_m_sei_trase_normalised = sum(
          total_usd_m_sei_trase_normalised_high_conf,
          total_usd_m_sei_trase_normalised_medium_conf,
          total_usd_m_sei_trase_normalised_low_conf,
          na.rm = TRUE
        )
      )
  })) %>%
  unnest(results)

write_csv(
  flows_country_commodity_totals,
  "./analytical-results/flows_totals_commodity_sei_trase_normalised_w_boundary_periods_po_conf_levels.csv"
)


#### 4.3. flow types (asset class, use of proceeds, directness) ------------
## NB - Fri 29 August - NEEDS UPDATING TO NEW DATA
#for (country_commodity in names(flows_2010_2022)) {
  df <- flows_2010_2022[[country_commodity]]
  plot_name <- paste0("plot_", country_commodity)
  if (nrow(df) > 0) {
    total_label = sum(df$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd,
                      na.rm = TRUE)
    plot <- horizontal_bar_chart(
      df %>%
        mutate(
          financial_flow_link_strength = factor(
            financial_flow_link_strength,
            levels = c("Low", "Medium", "High")
          )
        ) %>%
        group_by(financial_flow_link_strength) %>%
        summarise(
          total_usd_m = sum(
            tranche_amount_per_manager_usd_m_final_in_dec_2024_usd,
            na.rm = TRUE
          ),
          .groups = "drop"
        ) %>%
        mutate(pct = total_usd_m / sum(total_usd_m), dummy_x = "All flows"),
      x_value = "dummy_x",
      y_value = "pct",
      fill_value = "financial_flow_link_strength",
      y_value_label = "Proportion of financial flows",
      fill_value_label = "Strength of link to Amazon",
      title_label = country_commodity,
      custom_colours = c(
        "High" = "grey20",
        "Medium" = "grey60",
        "Low" = "grey85"
      )
    )
    print(plot)
    assign(plot_name, plot, envir = .GlobalEnv)
  } else {
    print(paste0("No financial flows for ", country_commodity))
  }
}

#plot_bolivia_soya_beans +
  plot_brazil_cattle_meat +
  plot_brazil_oil_palm_fruit +
  plot_brazil_soya_beans +
  plot_brazil_sugar_cane +
  plot_colombia_cattle_meat +
  plot_colombia_coffee_green +
  plot_ecuador_cocoa_beans +
  plot_peru_cocoa_beans +
  plot_peru_coffee_green +
  patchwork::plot_layout(ncol = 1, guides = "collect")

ggsave("./figures/flows_by_link_strength_rough_plot.pdf")

#### 4.4. geography ------------

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
        tranche_amount_per_manager_usd_m_final_in_dec_2024_usd,
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
      analytical_variable = "tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average",
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
          tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average,
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
          tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average,
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

write_csv(sankey_data_by_commodity,
          "./analytical-results/sankey_data_commodity_grouped_w_sensitivity_analysis.csv")

##### 4.4.3. data for all Amazon Sankey plot (then generated in Graphica) -----------
all_amazon_param_grid <- tibble(
  "boundary_period" = c(rep(0, 2), rep(1, 2), rep(2,2), rep(3,2)),
  "defn_phase_out" = c(rep(c(FALSE, TRUE), 4))
)

sankey_data_overall <- all_amazon_param_grid %>%
  mutate(results = pmap(list(boundary_period, defn_phase_out), function(period_, defn_status_) {
    message(
      period_,
      " yrs around SEI-Trase dates - defn phase out included: ",
      defn_status_
    )
    
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
        tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates_annualised,
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
        tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates_annualised,
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
        tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates_annualised,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    # add totals
    left_join(
      totals_overall,
      by = c( # note that this won't return a total for 'other'
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
  
write_csv(sankey_data_overall, "./analytical-results/sankey_data_all_amazon_w_sensitivity_analysis.csv")

### 4.5. summary tables per exporter_group ------------

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
      analytical_variable = "tranche_amount_per_manager_usd_m_final_in_dec_2024_usd"
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
  analytical_variable = "tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average"
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
  analytical_variable = "tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average"
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
  analytical_variable = "tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_commodity_annual_average"
)

p1 + p2 + p3 + patchwork::plot_layout(ncol = 1)
ggsave("./figures/china_top_fis.pdf",
       height = 14,
       width = 6)


##### 4.6.3. Final league table data (visualised in Graphica) -------------

government_owned_flag <- flows_all_countries_commodities %>%
  distinct(manager_true_ultimate_parent_organisation_name,
           government_ultimate_parent)

manager_hqs <- flows_all_countries_commodities %>%
  distinct(
    manager_true_ultimate_parent_organisation_name,
    manager_true_ultimate_parent_country_of_headquarters,
    manager_true_ultimate_parent_region_of_headquarters_unsd
  )

manager_totals <- flows_all_countries_commodities %>%
  filter(modulus(year_relative_to_trase_period) == 0) %>%
  group_by(manager_true_ultimate_parent_organisation_name) %>%
  summarise(
    total_usd_m_yr_overall = sum(
      tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates_annualised,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(total_usd_m_yr_overall)) %>%
  mutate(
    rank_overall = dense_rank(desc(total_usd_m_yr_overall)),
    prop_overall = total_usd_m_yr_overall / sum(total_usd_m_yr_overall)
  )

manager_by_country_commodity <- flows_all_countries_commodities %>%
  filter(modulus(year_relative_to_trase_period) == 0) %>%
  mutate(country_commodity = str_c(producer_country, commodity, sep = " - ")) %>%
  group_by(manager_true_ultimate_parent_organisation_name,
           country_commodity) %>%
  summarise(
    total_usd_m_yr_this_country_commodity = sum(
      tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates_annualised,
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
      tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates_annualised,
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
      tranche_amount_per_manager_usd_m_final_in_dec_2024_usd_adjusted_for_duplicates_annualised,
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
