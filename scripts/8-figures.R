# AMAZON FOOTPRINT IN FOOD AND FINANCIAL SYSTEMS --------
# Financial flows analysis: Stage 7 - Early figures -------
# github.com/lyd-m/amazon-footprint
# This project contains the financial analysis for the paper, Singh et al. (2025)
# This script takes the final company and financial data and visualises it

### DEPENDENCIES --------------------------
library(readxl)
library(tidyverse)
library(janitor)
library(patchwork)
library(RColorBrewer)

### DIRECTORIES --------------------------

### DATA IMPORT --------------------------

companies_all_yrs <- read_csv("./analytical-results/companies_all_years.csv") %>%
  clean_names() %>%
  select(-1) %>%
  mutate(ultimate_parent_company_oa_perm_id = as.character(ultimate_parent_company_oa_perm_id))

company_info_by_permid <- read_csv("./intermediate-results/companies_info_by_permid.csv") %>%
  select(-organization_perm_id,
         -1)

unsd_regions <- read_csv("./input-data/regions_map.csv")

company_info_by_permid <- company_info_by_permid %>%
  left_join(unsd_regions %>% select(country, region_unsd),
            by = c("country_of_headquarters"="country")) %>%
  rename(region_of_headquarters_unsd = region_unsd)

### CODES FOR PLOT TYPES ---------------------
horizontal_bar_chart <- function(grouped_data,# all variable names need to be in quotes "" 
                                 x_value="", # can change if you're using multiple categories
                                 y_value, # col being used (a prop between 0 and 1)
                                 fill_value, # what you're studying the distribution of
                                 y_value_label, 
                                 fill_value_label, 
                                 title_label, 
                                 custom_colours=NA) {
  plot <- ggplot(data = grouped_data,
                 aes(x = .data[[x_value]], y = .data[[y_value]], fill = .data[[fill_value]])) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::percent(.data[[y_value]], accuracy = 1), y = !!sym(y_value)), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 4) +
    labs(fill = fill_value_label,
         x = NULL,
         y = NULL,
         title = title_label) +
    theme_void() +
    coord_flip() +
    scale_fill_manual(values = custom_colours) + 
    theme(legend.position = "bottom",
          legend.direction = "vertical",
          axis.text.y = element_text(colour = "black", hjust=1), # right justified
          plot.title = element_text(),
          plot.title.position = "panel")
  
  return(plot)
}


### COMPANY DATA ANALYSIS --------------------------

# Add on extra info
companies_all_yrs <- companies_all_yrs %>%
  left_join(company_info_by_permid, 
            by = c("ultimate_parent_company_oa_perm_id" = "permid")) %>%
  rename_with(.fn = ~ paste0("ultimate_parent_", .x), .cols = 19:54)

#### Legal entity data availability ------------------
# this data as it stands includes deforestation exposure attributed to UNKNOWN
legal_entity_availability <- companies_all_yrs %>%
  group_by(producer_country, commodity, legal_entity_mapped) %>% # for each country-commodity setting (ignore years)
  summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE), # sum across all years for each one
            count = n_distinct(exporter_group)) %>%
  ungroup() %>%
  group_by(producer_country, commodity) %>%
  mutate(prop_adjusted_deforestation_exposure = adjusted_deforestation_exposure/sum(adjusted_deforestation_exposure, na.rm=TRUE),
         prop_count = count/sum(count)) %>%
  mutate(country_commodity = str_c(producer_country, " - ", commodity))

plt_legal_entity_by_count <- horizontal_bar_chart(legal_entity_availability,
                                                  x_value = "country_commodity",
                                                  y_value = "prop_count",
                                                  fill_value = "legal_entity_mapped",
                                                  y_value_label = "% (count)", 
                                                  fill_value_label = "Legal entity mapped?", 
                                                  title_label = "% (count)", 
                                                  custom_colours = c("grey","lightgreen"))

plt_legal_entity_by_defn_exposure <- horizontal_bar_chart(legal_entity_availability,
                                                  x_value = "country_commodity",
                                                  y_value = "prop_adjusted_deforestation_exposure",
                                                  fill_value = "legal_entity_mapped",
                                                  y_value_label = "% (deforestation exposure)", 
                                                  fill_value_label = "Legal entity mapped?", 
                                                  title_label = "% (deforestation exposure)", 
                                                  custom_colours = c("grey","lightgreen"))

wrap_plots(plt_legal_entity_by_count, plt_legal_entity_by_defn_exposure, ncol = 1) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ggsave("./figures/draft_legal_entity_data_avail_w_unknown.pdf")

# without UNKNOWN
legal_entity_availability_wo_unknown <- companies_all_yrs %>%
  filter(!exporter_group == "UNKNOWN") %>%
  group_by(producer_country, commodity, legal_entity_mapped) %>% # for each country-commodity setting (ignore years)
  summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE), # sum across all years for each one
            count = n_distinct(exporter_group)) %>%
  ungroup() %>%
  group_by(producer_country, commodity) %>%
  mutate(prop_adjusted_deforestation_exposure = adjusted_deforestation_exposure/sum(adjusted_deforestation_exposure, na.rm=TRUE),
         prop_count = count/sum(count)) %>%
  mutate(country_commodity = str_c(producer_country, " - ", commodity))

plt_legal_entity_by_count_wo_unknown <- horizontal_bar_chart(legal_entity_availability_wo_unknown,
                                                  x_value = "country_commodity",
                                                  y_value = "prop_count",
                                                  fill_value = "legal_entity_mapped",
                                                  y_value_label = "% (count)", 
                                                  fill_value_label = "Legal entity mapped?", 
                                                  title_label = "% (count)", 
                                                  custom_colours = c("grey","lightgreen"))

plt_legal_entity_by_defn_exposure_wo_unknown <- horizontal_bar_chart(legal_entity_availability_wo_unknown,
                                                  x_value = "country_commodity",
                                                  y_value = "prop_adjusted_deforestation_exposure",
                                                  fill_value = "legal_entity_mapped",
                                                  y_value_label = "% (deforestation exposure)", 
                                                  fill_value_label = "Legal entity mapped?", 
                                                  title_label = "% (deforestation exposure)", 
                                                  custom_colours = c("grey","lightgreen"))

wrap_plots(plt_legal_entity_by_count, 
           plt_legal_entity_by_count_wo_unknown,
           plt_legal_entity_by_defn_exposure,
           plt_legal_entity_by_defn_exposure_wo_unknown,
           ncol = 2) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ggsave("./figures/draft_legal_entity_data_avail_all.pdf")

#### Legal entity type ------------------

legal_entity_types <- companies_all_yrs %>%
  mutate(legal_entity_type = case_when(
    ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
    ultimate_parent_organization_is_public_flag == TRUE & ultimate_parent_free_float_percent_ >= 90 & ultimate_parent_government_ultimate_parent != "True" ~ "Publicly listed",
    ultimate_parent_organization_is_public_flag == TRUE & ultimate_parent_free_float_percent_ < 90 & ultimate_parent_government_ultimate_parent != "True" ~ "Part-listed",
    ultimate_parent_organization_is_public_flag == FALSE & ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
    ultimate_parent_organization_is_public_flag == FALSE & (ultimate_parent_government_ultimate_parent == "False" | ultimate_parent_government_ultimate_parent == "Unknown") ~ "Private",
    legal_entity_mapped == FALSE ~ "No legal entity mapped",
    TRUE ~ "Unknown"
  )) %>%
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

plt_legal_entity_types_by_count <- horizontal_bar_chart(legal_entity_types,
                                                             x_value = "country_commodity",
                                                             y_value = "prop_count",
                                                             fill_value = "legal_entity_type",
                                                             y_value_label = "% (count)", 
                                                             fill_value_label = "Ultimate parent type", 
                                                             title_label = "% (count)", 
                                                             custom_colours = c(
                                                               "Unknown" ="grey",
                                                               "No legal entity mapped" = "lightgrey",
                                                               "Publicly listed" = "lightblue",
                                                               "Part-listed" = "lightgreen",
                                                               "Private" = "darkgreen",
                                                               "State-owned" = "darkblue"))

plt_legal_entity_types_by_defn_exposure <- horizontal_bar_chart(legal_entity_types,
                                                        x_value = "country_commodity",
                                                        y_value = "prop_adjusted_deforestation_exposure",
                                                        fill_value = "legal_entity_type",
                                                        y_value_label = "% (deforestation exposure)", 
                                                        fill_value_label = "Ultimate parent type", 
                                                        title_label = "% (deforestation exposure)", 
                                                        custom_colours = c(
                                                          "Unknown" ="grey",
                                                          "No legal entity mapped" = "lightgrey",
                                                          "Publicly listed" = "lightblue",
                                                          "Part-listed" = "lightgreen",
                                                          "Private" = "darkgreen",
                                                          "State-owned" = "darkblue"))

legal_entity_types_wo_unknown <- companies_all_yrs %>%
  filter(!exporter_group == "UNKNOWN") %>%
  mutate(legal_entity_type = case_when(
    ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
    ultimate_parent_organization_is_public_flag == TRUE & ultimate_parent_free_float_percent_ >= 90 & ultimate_parent_government_ultimate_parent != "True" ~ "Publicly listed",
    ultimate_parent_organization_is_public_flag == TRUE & ultimate_parent_free_float_percent_ < 90 & ultimate_parent_government_ultimate_parent != "True" ~ "Part-listed",
    ultimate_parent_organization_is_public_flag == FALSE & ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
    ultimate_parent_organization_is_public_flag == FALSE & (ultimate_parent_government_ultimate_parent == "False" | ultimate_parent_government_ultimate_parent == "Unknown") ~ "Private",
    legal_entity_mapped == FALSE ~ "No legal entity mapped",
    TRUE ~ "Unknown"
  )) %>%
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

plt_legal_entity_types_by_count_wo_unknown <- horizontal_bar_chart(legal_entity_types_wo_unknown,
                                                        x_value = "country_commodity",
                                                        y_value = "prop_count",
                                                        fill_value = "legal_entity_type",
                                                        y_value_label = "% (count)", 
                                                        fill_value_label = "Ultimate parent type", 
                                                        title_label = "% (count)", 
                                                        custom_colours = c(
                                                          "Unknown" ="grey",
                                                          "No legal entity mapped" = "lightgrey",
                                                          "Publicly listed" = "lightblue",
                                                          "Part-listed" = "lightgreen",
                                                          "Private" = "darkgreen",
                                                          "State-owned" = "darkblue"))

plt_legal_entity_types_by_defn_exposure_wo_unknown <- horizontal_bar_chart(legal_entity_types_wo_unknown,
                                                                x_value = "country_commodity",
                                                                y_value = "prop_adjusted_deforestation_exposure",
                                                                fill_value = "legal_entity_type",
                                                                y_value_label = "% (deforestation exposure)", 
                                                                fill_value_label = "Ultimate parent type", 
                                                                title_label = "% (deforestation exposure)", 
                                                                custom_colours = c(
                                                                  "Unknown" ="grey",
                                                                  "No legal entity mapped" = "lightgrey",
                                                                  "Publicly listed" = "lightblue",
                                                                  "Part-listed" = "lightgreen",
                                                                  "Private" = "darkgreen",
                                                                  "State-owned" = "darkblue"))


wrap_plots(plt_legal_entity_types_by_count, 
           plt_legal_entity_types_by_count_wo_unknown,
           plt_legal_entity_types_by_defn_exposure,
           plt_legal_entity_types_by_defn_exposure_wo_unknown,
           ncol = 2) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ggsave("./figures/draft_legal_entity_types_all.pdf")

#### Legal entity headquarters ------------------

legal_entity_hqs <- companies_all_yrs %>%
  group_by(producer_country, commodity, ultimate_parent_region_of_headquarters_unsd) %>%
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

plt_legal_entity_hqs_by_count <- horizontal_bar_chart(legal_entity_hqs,
                                                      x_value = "country_commodity",
                                                      y_value = "prop_count",
                                                      fill_value = "ultimate_parent_region_of_headquarters",
                                                      y_value_label = "% (count)",
                                                      fill_value_label = "Ultimate parent HQ region",
                                                      title_label = "% (count)",
                                                      custom_colours = brewer.pal())

plt_legal_entity_types_by_defn_exposure <- horizontal_bar_chart(legal_entity_types,
                                                                x_value = "country_commodity",
                                                                y_value = "prop_adjusted_deforestation_exposure",
                                                                fill_value = "legal_entity_type",
                                                                y_value_label = "% (deforestation exposure)", 
                                                                fill_value_label = "Ultimate parent type", 
                                                                title_label = "% (deforestation exposure)", 
                                                                custom_colours = c(
                                                                  "Unknown" ="grey",
                                                                  "No legal entity mapped" = "lightgrey",
                                                                  "Publicly listed" = "lightblue",
                                                                  "Part-listed" = "lightgreen",
                                                                  "Private" = "darkgreen",
                                                                  "State-owned" = "darkblue"))

legal_entity_types_wo_unknown <- companies_all_yrs %>%
  filter(!exporter_group == "UNKNOWN") %>%
  mutate(legal_entity_type = case_when(
    ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
    ultimate_parent_organization_is_public_flag == TRUE & ultimate_parent_free_float_percent_ >= 90 & ultimate_parent_government_ultimate_parent != "True" ~ "Publicly listed",
    ultimate_parent_organization_is_public_flag == TRUE & ultimate_parent_free_float_percent_ < 90 & ultimate_parent_government_ultimate_parent != "True" ~ "Part-listed",
    ultimate_parent_organization_is_public_flag == FALSE & ultimate_parent_government_ultimate_parent == "True" ~ "State-owned",
    ultimate_parent_organization_is_public_flag == FALSE & (ultimate_parent_government_ultimate_parent == "False" | ultimate_parent_government_ultimate_parent == "Unknown") ~ "Private",
    legal_entity_mapped == FALSE ~ "No legal entity mapped",
    TRUE ~ "Unknown"
  )) %>%
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

plt_legal_entity_types_by_count_wo_unknown <- horizontal_bar_chart(legal_entity_types_wo_unknown,
                                                                   x_value = "country_commodity",
                                                                   y_value = "prop_count",
                                                                   fill_value = "legal_entity_type",
                                                                   y_value_label = "% (count)", 
                                                                   fill_value_label = "Ultimate parent type", 
                                                                   title_label = "% (count)", 
                                                                   custom_colours = c(
                                                                     "Unknown" ="grey",
                                                                     "No legal entity mapped" = "lightgrey",
                                                                     "Publicly listed" = "lightblue",
                                                                     "Part-listed" = "lightgreen",
                                                                     "Private" = "darkgreen",
                                                                     "State-owned" = "darkblue"))

plt_legal_entity_types_by_defn_exposure_wo_unknown <- horizontal_bar_chart(legal_entity_types_wo_unknown,
                                                                           x_value = "country_commodity",
                                                                           y_value = "prop_adjusted_deforestation_exposure",
                                                                           fill_value = "legal_entity_type",
                                                                           y_value_label = "% (deforestation exposure)", 
                                                                           fill_value_label = "Ultimate parent type", 
                                                                           title_label = "% (deforestation exposure)", 
                                                                           custom_colours = c(
                                                                             "Unknown" ="grey",
                                                                             "No legal entity mapped" = "lightgrey",
                                                                             "Publicly listed" = "lightblue",
                                                                             "Part-listed" = "lightgreen",
                                                                             "Private" = "darkgreen",
                                                                             "State-owned" = "darkblue"))


wrap_plots(plt_legal_entity_types_by_count, 
           plt_legal_entity_types_by_count_wo_unknown,
           plt_legal_entity_types_by_defn_exposure,
           plt_legal_entity_types_by_defn_exposure_wo_unknown,
           ncol = 2) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

ggsave("./figures/draft_legal_entity_types_all.pdf")

### FINANCIAL DATA ANALYSIS --------------------------

