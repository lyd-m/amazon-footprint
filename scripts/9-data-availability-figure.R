# Clear Environment
rm(list=ls(all=TRUE))

# Clear Plots
if(!is.null(dev.list())) dev.off()

# Clear Console
cat("\014")

## 1 - Dependencies --------
library(tidyverse)
library(janitor)
library(readxl)
library(stringr)

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

exporter_gps_in_flows <- read_csv("./analytical-results/exporter_groups_financial_data_availability.csv") %>%
 select(producer_country, commodity, exporter_group, fin_flows) %>%
  filter(fin_flows)

exporter_gps_le <- read_csv("./analytical-results/exporter_groups_financial_data_availability.csv") %>%
  select(producer_country, commodity, exporter_group, legal_entity) %>%
  filter(legal_entity)
 
## 4 - Analysis --------
# combine deduce data into one sheet

deduce_all <- plyr::ldply(deduce_data, data.frame, .id="DeDuCE_output") 

deduce_all <- deduce_all %>% 
  filter(year >= 2010 & year <= 2022)

# join on availability data

deduce_attributed <- deduce_all %>%
  left_join(exporter_gps_in_flows, 
            by = c("producer_country", "commodity", "exporter_group")) %>% 
  mutate(fin_flows = if_else(
    fin_flows==TRUE & !is.na(fin_flows),
    fin_flows,
    FALSE
  ))

# totals

totals <- deduce_attributed %>%
  group_by(commodity, fin_flows) %>%
  summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure, na.rm = TRUE), .groups = "drop") %>%
  group_by(commodity) %>%
  mutate(prop_attributed = adjusted_deforestation_exposure/sum(adjusted_deforestation_exposure))

totals_attributed <- totals %>%
  filter(fin_flows) %>%
  select(-prop_attributed)

write_csv(totals_attributed, "./analytical-results/exporter_groups_attributed_defn_exposure.csv")

totals_w_unknown <- deduce_attributed %>%
  left_join(exporter_gps_le,
            by = c("producer_country",
                   "commodity",
                   "exporter_group")) %>%
  mutate(legal_entity = if_else(
    legal_entity == TRUE & !is.na(legal_entity),
    legal_entity,
    FALSE
  )) %>%
  mutate(exporter_group = if_else(
    grepl("UNKNOWN", exporter_group, ignore.case = TRUE),
    FALSE,
    TRUE
  )) %>%
  group_by(exporter_group, legal_entity, fin_flows) %>%
  summarise(adjusted_deforestation_exposure = sum(adjusted_deforestation_exposure), .groups = "drop")

# visualise
ggplot(aes(x = factor(commodity, levels = c("Cattle meat",
                                              "Soya beans",
                                              "Maize (corn)",
                                              "Oil palm fruit",
                                              "Cocoa beans",
                                              "Sugar cane",
                                              "Coffee, green")), y = prop_attributed, fill = fin_flows)) +
  geom_col(colour = "grey") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, colour = "black"),
        axis.text.y = element_text(colour = "white")) +
  scale_x_discrete(expand = expansion(0.1,0)) +
  scale_y_continuous(expand = expansion(0,0),
                     breaks = seq(0,1,0.2)) +
  labs(x = "",
       y = "Share of deforestation (%)",
       fill = "Financial flows") +
  scale_fill_manual(values = c("white",
                               "aquamarine"))

