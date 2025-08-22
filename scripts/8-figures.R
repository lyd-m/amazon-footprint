# AMAZON FOOTPRINT IN FOOD AND FINANCIAL SYSTEMS --------
# Financial flows analysis: Stage 7 - Early figures -------
# github.com/lyd-m/amazon-footprint
# This project contains the financial analysis for the paper, Singh et al. (2025)
# This script takes the final company and financial data and visualises it

rm(list = ls())
### DEPENDENCIES --------------------------
library(readxl)
library(tidyverse)
library(janitor)
library(patchwork)
library(RColorBrewer)

### DIRECTORIES --------------------------
to_file_save_format <- function(str) {
  file_save_format_string <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", str, perl = TRUE)
  file_save_format_string <- gsub(" ", "-", tolower(file_save_format_string))
  return(file_save_format_string)
}

### DATA IMPORT --------------------------
regions_unsd <- read_csv("./input-data/regions_map.csv") %>%
  select(country, region_unsd)

#### Companies --------------------------
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

#### Flows --------------------------
flows_files <- list.files("./intermediate-results/",
                                       pattern = "flows_attributed_")

flows <- list()
for (file_ in flows_files) {
  var_name <- str_replace(file_, "flows_attributed_2008-2024_flows_", "")
  var_name <- str_replace(var_name, "\\(.*","")
  var_name <- str_replace(var_name, ".csv","")
  
  df <- read_csv(paste0("./intermediate-results/",file_)) %>%
    clean_names()
  
  flows[[var_name]] <- df
}

### CODES FOR PLOT TYPES ---------------------
horizontal_bar_chart <- function(grouped_data,# all variable names need to be in quotes "" 
                                 x_value="dummy_x", # can change if you're using multiple categories
                                 y_value, # col being used (a prop between 0 and 1)
                                 fill_value, # what you're studying the distribution of
                                 y_value_label, 
                                 fill_value_label, 
                                 title_label, 
                                 custom_colours=NA,
                                 outside_label=NA) {
  df_plot <- grouped_data
  plot <- ggplot(data = grouped_data,
                 aes(x = .data[[x_value]], y = .data[[y_value]], fill = .data[[fill_value]])) +
    geom_bar(stat = "identity") +
    # percentage labels
    geom_text(aes(label = scales::percent(.data[[y_value]], accuracy = 1), y = !!sym(y_value)), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 4) +
    # outside labels
    geom_text(data = distinct(df_plot, dummy_x) %>% mutate(label = total_label),
              aes(x = dummy_x, y = 1.01, label = label),
              inherit.aes = FALSE,
              hjust = 0, size = 4) +
    labs(fill = fill_value_label,
         x = "",
         y = "",
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
# simple descriptive plots to start with

#### ABSOLUTE AMOUNTS ------------
flows_totals_2010_2022 <- tibble()
for (country_commodity in names(flows_2010_2022)) {
  print(country_commodity)
  df <- flows_2010_2022[[country_commodity]]
  if (nrow(df) > 0) {
  result <- df %>%
    group_by(producer_country, commodity) %>%
    summarise(
      total_usd_m_high_conf = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd[financial_flow_link_strength=="High"], na.rm = TRUE),
      total_usd_m_medium_conf = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd[financial_flow_link_strength %in% c("High", "Medium")], na.rm = TRUE),
      total_usd_m_low_conf = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE),
      .groups = "drop"
    )
  
  flows_totals_2010_2022 <- bind_rows(flows_totals_2010_2022, result)

  } else {
    result <- df %>%
      distinct(producer_country, commodity) %>%
      mutate(total_usd_m_high_conf = 0,
             total_usd_m_low_conf = 0)
    
    flows_totals_2010_2022 <- bind_rows(flows_totals_2010_2022, result)
  }
}

write_csv(flows_totals_2010_2022,"./analytical-results/flows_totals_2010_2022.csv")

#### FLOW TYPES (asset class, use of proceeds, directness) ------------
for (country_commodity in names(flows_2010_2022)) {
  df <- flows_2010_2022[[country_commodity]]
  plot_name <- paste0("plot_", country_commodity)
  if (nrow(df) > 0) {
    total_label = sum(df$tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE)
    plot <- horizontal_bar_chart(df %>%
                           mutate(financial_flow_link_strength = factor(financial_flow_link_strength, levels = c("Low", "Medium", "High"))) %>%
                           group_by(financial_flow_link_strength) %>% 
                           summarise(total_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE), .groups = "drop") %>%
                           mutate(pct = total_usd_m/sum(total_usd_m),
                                  dummy_x = "All flows"),
                         x_value = "dummy_x",
                         y_value = "pct",
                         fill_value = "financial_flow_link_strength", 
                         y_value_label = "Proportion of financial flows",
                         fill_value_label = "Strength of link to Amazon", 
                         title_label = country_commodity,
                         custom_colours=c(
                           "High" = "grey20",
                           "Medium" = "grey60",
                           "Low" = "grey85"))
    print(plot)
    assign(plot_name, plot, envir = .GlobalEnv)
  } else {print(paste0("No financial flows for ", country_commodity))}
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
  patchwork::plot_layout(ncol = 1,
                         guides = "collect") 

ggsave("./figures/flows_by_link_strength_rough_plot.pdf")

#### GEOGRAPHY ------------
create_country_col_plot <- function(data,
                                    case, 
                                    y_label_position_change, # 60000/60000 for Amazon/Indonesia
                                    y_label_hjust, # 12000/1000 for Amazon/Indonesia
                                    y_breaks, 
                                    y_max,
                                    plot_title,
                                    top_n) {
  df <- data
  
  plot_data <- df %>%
    mutate(manager_true_ultimate_parent_country_of_headquarters = if_else(is.na(manager_true_ultimate_parent_country_of_headquarters),
                                                                          "Unknown",
                                                                          manager_true_ultimate_parent_country_of_headquarters)) %>%
    group_by(manager_true_ultimate_parent_country_of_headquarters) %>%
    summarise(amount_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(amount_usd_m)) %>%
    mutate(rank = dense_rank(desc(amount_usd_m)),
           pct = amount_usd_m/sum(amount_usd_m))
  
  write_csv(plot_data, paste0("./figures/flows_by_country_col_chart_data_", case, ".csv"))
  
  plot_data <- plot_data %>%
    filter(rank <= top_n) %>%
    ungroup() %>%
    left_join(regions_unsd, by = c("manager_true_ultimate_parent_country_of_headquarters" = "country")) %>%
    mutate(region_unsd = if_else(is.na(region_unsd),
                                 "Unknown",
                                 region_unsd)) # correct for NA regions
  
  plot <- ggplot(plot_data, aes(x = reorder(manager_true_ultimate_parent_country_of_headquarters, desc(rank)), y = amount_usd_m, fill = region_unsd)) +
    geom_col() +
    geom_text(aes(label = paste0(percent(pct, accuracy = 0.1)),
                  y = if_else(amount_usd_m > y_label_position_change, 
                              (amount_usd_m - y_label_hjust), 
                              (amount_usd_m + y_label_hjust))),
              size = 3) +
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
    scale_y_continuous(labels = comma,
                       expand = expansion(mult = c(0,0.04)),
                       limits = c(0, y_max),
                       breaks = seq(0,y_max,y_breaks)) +
    scale_x_discrete(expand = expansion(mult = c(0))) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid = element_line(colour = "grey70"),
          axis.text.x = element_text(size = 10, colour = "black"),
          axis.text.y = element_text(size = 10, colour = "black"),
          axis.title.y = element_text(size = 12, colour = "black"),
          axis.title.x = element_text(size = 12, colour = "black"),
          legend.position = c(0.75,0.3)) +
    labs(x = NULL,
         y = "Flows (2024 USDm)",
         fill = "Region",
         title = plot_title) +
    coord_flip()
  
  print(plot)
}

country_col_plot_params <- tribble(
  ~country_commodity, ~y_label_position_change, ~y_label_hjust, ~y_breaks, ~y_max,
  "bolivia_soya_beans", 20000, 2000, 20000, 40000,
  "brazil_cattle_meat", 12000, 1500, 12500, 25000,
  "brazil_oil_palm_fruit", 5000, 500, 4000, 8000,
  "brazil_soya_beans", 100000, 15000, 150000, 300000,
  "brazil_sugar_cane", 40000, 7500, 50000, 125000,
  "colombia_cattle_meat", 2500, 300, 3000, 6000,
  "colombia_coffee_green", 7500, 2000, 10000, 25000,
  "ecuador_cocoa_beans", 40000, 5000, 50000, 100000,
  "peru_cocoa_beans", 1000, 250, 2000, 4000,
  "peru_coffee_green", 15000, 2000, 20000, 35000)

for (country_commodity in names(flows_2010_2022)) {
  df <- flows_2010_2022[[country_commodity]]
  if (nrow(df) > 0) {
    
    plot_name <- paste0("plot_", country_commodity)
    
    params <- country_col_plot_params %>%
      filter(country_commodity == !!country_commodity)
    
    plot <- create_country_col_plot(df,
                                    case = country_commodity,
                                    y_label_position_change = params$y_label_position_change,
                                    y_label_hjust = params$y_label_hjust,
                                    y_breaks = params$y_breaks,
                                    y_max = params$y_max,
                                    plot_title = country_commodity,
                                    top_n = 10)
    assign(plot_name, plot, envir = .GlobalEnv)
  } else {print(paste0("No financial flows for ", country_commodity))}
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
  patchwork::plot_layout(ncol = 5,
                         guides = "collect") 

ggsave('./figures/flows_by_country_rough_plot.pdf',
       height = 10,
       width = 20)

flows_2010_2022[["brazil_soya_beans"]] %>%
  group_by(manager_true_ultimate_parent_country_of_headquarters, exporter_group) %>%
  summarise(total_usd_m = sum(tranche_amount_per_manager_usd_m_final_in_dec_2024_usd, na.rm = TRUE), .groups = "drop") %>%
  group_by(manager_true_ultimate_parent_country_of_headquarters) %>%
  mutate(pct_for_country = total_usd_m/sum(total_usd_m)) %>%
  arrange(desc(pct_for_country),.by_group = TRUE) %>%
  write_csv("./intermediate-results/flows_by_country_top_counterparties_brazil_soya_beans.csv") # big Japan contribution due to Mitsubishi

#### ACTORS ------------
create_manager_league_table_w_fill <- function(data, # includes government-owned entities marked with a *
                                               fill_option,
                                               case,
                                               n_shown,
                                               y_label_position_change, # shift labels from inside cols to outside cols
                                               y_label_hjust, # position of labels
                                               y_max,
                                               y_breaks,
                                               analytical_variable
) {
  plot_data <- data %>%
    group_by(manager_true_ultimate_parent_organisation_name) %>%
    summarise(amount_usd_m = sum(!!sym(analytical_variable), na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(amount_usd_m)) %>%
    mutate(rank = dense_rank(desc(amount_usd_m)),
           prop = amount_usd_m / sum(amount_usd_m)) %>%
    filter(rank <= n_shown) %>%
    # join on additional info
    left_join((data %>% 
                 distinct(manager_true_ultimate_parent_organisation_name,.keep_all = TRUE) %>%
                 select(manager_true_ultimate_parent_organisation_name, !!sym(fill_option), government_ultimate_parent)), 
              by = c("manager_true_ultimate_parent_organisation_name")) %>%
    # tag government ultimate parent 
    # handling "Not Applicable" as managers (self-arranged)
    mutate(government_ultimate_parent = if_else(is.na(government_ultimate_parent),
                                                                             FALSE,
                                                                             government_ultimate_parent),
           manager_true_ultimate_parent_organisation_name = case_when(
             government_ultimate_parent == TRUE ~ paste0(manager_true_ultimate_parent_organisation_name,"*"),
             is.na(manager_true_ultimate_parent_organisation_name) ~ "Unknown",
             grepl("DZ BANK",manager_true_ultimate_parent_organisation_name) ~ "DZ Bank AG",
             manager_true_ultimate_parent_organisation_name == "Not Applicable" ~ "Self-arranged",
             TRUE ~ manager_true_ultimate_parent_organisation_name)
           )
  
  write_csv(plot_data, paste0("./figures/",
                              "/flows_by_manager_",
                              to_file_save_format(case),
                              "_",
                              ".csv"))
  
  plot <- plot_data %>%
    ggplot(aes(x = reorder(manager_true_ultimate_parent_organisation_name, desc(rank)), 
               y = amount_usd_m, 
               label = percent(prop, accuracy = 0.1),
               fill = !!sym(fill_option))) +
    geom_col() +
    geom_text(aes(label = percent(prop, accuracy = 0.1),
                  y = if_else(amount_usd_m > y_label_position_change, 
                              (amount_usd_m - y_label_hjust), 
                              (amount_usd_m + y_label_hjust))),
              size = 3) +
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
    scale_y_continuous(labels = comma,
                       expand = expansion(mult = c(0.01,0.05)),
                       breaks = seq(0,y_max,y_breaks)) +
    scale_x_discrete(expand = expansion(mult = c(0))) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.text.x = element_text(size = 10, color = "black"),
          axis.text.y = element_text(size = 10, color = "black"),
          axis.title.y = element_text(size = 8, color = "black"),
          axis.title.x = element_text(size = 10, color = "black"),
          legend.position = "none",
          axis.line.x = element_line(color = "grey"),
          plot.title = element_text(hjust = 0.9, size = 10, color = "black")) +
    labs(x = NULL,
         y = "",
         fill = "Region",
         title = case) +
    coord_flip()
  
  print(plot)
}

league_table_plot_params <- tribble(
  ~country_commodity, ~y_label_position_change, ~y_label_hjust, ~y_breaks, ~y_max,
  "bolivia_soya_beans", 6000, 500, 5000, 9000,
  "brazil_cattle_meat", 5500, 500, 5000, 6500,
  "brazil_oil_palm_fruit", 2600, 200, 2000, 3500,
  "brazil_soya_beans", 50000, 7500, 50000, 110000,
  "brazil_sugar_cane", 20000, 2500, 20000, 45000,
  "colombia_cattle_meat", 1750, 100, 2000, 3000,
  "colombia_coffee_green", 7500, 1000, 10000, 15000,
  "ecuador_cocoa_beans", 25000, 2500, 25000, 50000,
  "peru_cocoa_beans", 1000, 100, 500, 1200,
  "peru_coffee_green", 10000, 1000, 10000, 20000)

for (country_commodity in names(flows_2010_2022)) {
  df <- flows_2010_2022[[country_commodity]]
  if (nrow(df) > 0) {
    
    plot_name <- paste0("plot_", country_commodity)
    
    params <- league_table_plot_params %>%
      filter(country_commodity == !!country_commodity)
    
    plot <- create_manager_league_table_w_fill(df,
                                               case = country_commodity,
                                               fill_option = "manager_true_ultimate_parent_region_of_headquarters_unsd",
                                               n_shown = 20,
                                               y_label_position_change = params$y_label_position_change,
                                               y_label_hjust = params$y_label_hjust,
                                               y_max = params$y_max,
                                               y_breaks = params$y_breaks,
                                               analytical_variable = "tranche_amount_per_manager_usd_m_final_in_dec_2024_usd")
    
    assign(plot_name, plot, envir = .GlobalEnv)
  } else {print(paste0("No financial flows for ", country_commodity))}
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
  patchwork::plot_layout(ncol = 5,
                         guides = "collect") 

ggsave('./figures/flows_by_manager_rough_plot.pdf',
       width = 20,
       height = 12)

