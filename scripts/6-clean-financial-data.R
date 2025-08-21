# AMAZON FOOTPRINT IN FOOD AND FINANCIAL SYSTEMS --------
# Financial flows analysis
# Stage 6 - Cleaning financial data --------
# github.com/lyd-m/amazon-footprint

# This project contains the financial analysis for the paper, Singh et al. (2025)
# This script:
## Combines the raw financial flows pulled in step 4 with the mapped ultimate parents and DeDuCE data
## Only includes entities for certain years
## Splits financial amounts between individual financial institutions
## The next script then tests different methodologies for direct/indirect and phase out

## prerequisites ------------------------------
rm(list = ls())

## packages  ------------------------------
library(tidyverse)
library(readxl)
library(janitor)
library(purrr)
library(stringr)
library(lubridate)
library(stringr)
library(scales)

## functions  ------------------------------
modulus <- function(x) {
  sqrt(x^2)
}

tidydim_sum <- function(x, col_sum = NULL) {
  print(dim(x))
  if (!is.null(col_sum)) {
    cat("Sum of column", col_sum, ":", sum(x[, col_sum], na.rm = TRUE), "\n")
  }
  invisible(x)
} # gives total and amount of dataset


to_file_save_format <- function(str) {
  file_save_format_string <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", str, perl = TRUE)
  file_save_format_string <- gsub(" ", "-", tolower(file_save_format_string))
  return(file_save_format_string)
}

clean_string <- function(x) {
  x %>%
    str_replace_all("[\\(\\),]", "") %>% 
    str_replace_all("[^A-Za-z0-9]", "_") %>% 
    str_to_lower()
}


## 1. import data ------------------------------

### 1.1. company data  ------------------------------

companies_yearly <- read_csv('./analytical-results/companies_all_years.csv') %>%
  select(-1) %>% # remove python index column
  clean_names() %>%
  rename(common_oa_perm_id = oa_perm_id)

companies_column_names <- print(names(companies_yearly)) # saving for reference

### 1.2. financial data  ------------------------------

#### financial flows ------------------------------
asset_classes <- list("Bond deals", "Equity deals", "Loan deals")

flows_data_list <- list()

for (asset_class_ in asset_classes) {
  filename <- list.files("./intermediate-results/", pattern = to_file_save_format(asset_class_), full.names = TRUE)
  df <- read_csv(filename)
  flows_data_list[[asset_class_]] <- df
}

# column names for tidying 
colnames <- read_excel("./input-data/lseg_columns_needed.xlsx") %>%
  clean_names() %>%
  select(asset_class, standardised_name)

# loans - replace columns 2-40
# capital markets - replace columns 2 - 39

for (asset_class_ in names(flows_data_list)) {
  df <- flows_data_list[[asset_class_]] 
  type <- asset_class_
  
  df <- df %>%
    select(-1) %>% # drop index column
    rename(instrument = 1)
  
  if (type %in% c("Bond deals", "Equity deals")) {
    cols_ <- (colnames %>% filter(asset_class == asset_class_))$standardised_name
    df <- df %>%
      rename_with(~cols_,2:39) %>%
      mutate(financial_flow_type = "Capital markets issuance",
             year = year(date_close)) %>%
      select(financial_flow_type, asset_class, queried_company_permid, everything())
    flows_data_list[[asset_class_]] <- df
  }
  else {
    cols_ <- (colnames %>% filter(asset_class == asset_class_))$standardised_name
    df <- df %>%
      rename_with(~cols_,2:40) %>%
      mutate(financial_flow_type = "Lending",
             year = year(date_close)) %>%
      select(financial_flow_type, asset_class, queried_company_permid, everything())
    flows_data_list[[asset_class_]] <- df
  }
}

flows_data_all <- tibble()
for (asset_class_ in names(flows_data_list)) {
  df <- flows_data_list[[asset_class_]] 
  flows_data_all <- bind_rows(flows_data_all, df)
}

#### financial institutions' ultimate parents ------------------------------
fi_ups_all <- read_excel("./intermediate-results/ultimate_parents_database.xlsx",
                         na = c("", " ", "NA", "<NA>", "NaN")) %>%
  clean_names() %>%
  distinct(query, .keep_all = TRUE) %>% # remove any duplicates that have crept in
  rename_with(~ paste0("manager_true_", .x), .cols = c(2:5)) # rename to distinguish from organisation info from flows data
  

#### financial institutions' characteristics by permid ------------------------------
fi_info_by_permid <- read_csv("./intermediate-results/financial_institutions_info_by_permid.csv",
                              na = c("", " ", "NA", "<NA>", "NaN")) %>%
  clean_names() %>%
  select(-x1, -organization_perm_id, -ultimate_parent_id) %>%
  rename(organization_ultimate_parent_inc_govt = organization_ultimate_parent) %>% # distinguishing between the two types of ultimate parent in the ref data
  rename_with(~ paste0("manager_true_ultimate_parent_", .x), .cols = c(2:19)) # rename to distinguish from organisation info from flows data


### 1.3 reference data  ---------------------------------

#### LSEG countries mapped to standardised region classifications (UNSD, World Bank) ------------------------------
regions_unsd <- read_csv("./input-data/regions_map.csv") %>% 
  clean_names() %>%
  select(country, region_unsd)

#### US Consumer Price Index (sourced from LSEG) ------------------------------
us_cpi_monthly <- read_xlsx("./input-data/Economic Indicator_United States CPI Index, NSA__10 Apr 2025.xlsx",
                            sheet = 2,
                            skip = 3,
                            col_types = c("guess",
                                          "guess",
                                          "numeric")) %>%
  clean_names() %>%
  select(-original_release_date) %>%
  mutate(period = parse_date(period, "%b %Y")) %>%
  rename(us_cpi_index_this_period = first_release)

## 2. Tidy up Trase/DeDuCE data to give unique permids for country-commodity settings ------------------------

# convert 'years appeared' and 'years available' back into vectors of numbers within each entry for easier data handling
companies_yearly <- companies_yearly %>%
  mutate(
    years_appeared = str_split(years_appeared, ",\\s*") %>% map(as.integer),
    years_available = str_extract(years_available, "\\d{4}-\\d{4}") %>%
      str_split("-") %>%
      map(~ seq(as.integer(.x[1]), as.integer(.x[2])))
  )

# map of all legal entities and permids associated with each exporter group
permid_map <- companies_yearly %>% 
  pivot_longer(
    cols = c(
      common_name, common_oa_perm_id,
      parent_organisation_name, parent_company_oa_perm_id,
      ultimate_parent_organisation_name, ultimate_parent_company_oa_perm_id,
      joint_venture_organisation_name, joint_venture_company_oa_perm_id
    ),
    names_to = c("legal_entity_type", ".value"),
    names_pattern = "^(common|parent|ultimate_parent|joint_venture)(?:_(?:organisation|company))?_(name|oa_perm_id)$",
    names_transform = list(name = as.character, oa_perm_id = as.numeric)
  ) %>%
  filter(!is.na(oa_perm_id)) %>%
  rename(legal_entity_name = name)

# compressed map where for each country-commodity-year, each permid only appears once
# i.e., permids associated with multiple exporter_groups compress those names together
permid_map_compressed <- permid_map %>% 
  distinct(producer_country, commodity, # treat each country-commodity setting as separate
                        year,
                        oa_perm_id, legal_entity_name,
                        exporter_group, years_appeared) %>% 
  unnest(years_appeared) %>%
  arrange(producer_country, commodity, year, legal_entity_name, exporter_group) %>%
  group_by(producer_country, commodity,
           year,
           oa_perm_id) %>%
  summarise(
    n_exporter_group = n_distinct(exporter_group),
    legal_entity_name = paste(sort(unique(legal_entity_name)), collapse = ", "),
    exporter_group = paste(sort(unique(exporter_group)), collapse = ", "),
    years_appeared_consolidated = list(sort(unique(years_appeared))),
    phase_in_year_consolidated  = min(years_appeared, na.rm = TRUE),
    phase_out_year_consolidated = max(years_appeared, na.rm = TRUE),
    .groups = "drop"
  )

## 2. Splitting flows by country-commodity ------------------------
# Create individual country-commodity datasets, filter to include only the permids for those years
# Join back together for master dataset (which will include duplicates)

country_commodity_combs <- companies_yearly %>%
  distinct(producer_country, commodity)

flows_by_country_commodity_raw <- list()
for (i in seq_len(nrow(country_commodity_combs))) {
  country <- country_commodity_combs$producer_country[i]
  commodity_ <- country_commodity_combs$commodity[i]

  permid_map_subset <- permid_map_compressed %>%
    filter(producer_country == country,
           commodity == commodity_)
  
  flows_subset <- flows_data_all %>%
    # filter out transactions that shouldn't be included for that year for that entity (e.g., due to hierarchy change)
    semi_join(permid_map_subset,
              by = c("year" = "year",
                     "queried_company_permid" = "oa_perm_id")) %>%
    # join on permid info
    left_join(permid_map_subset,
              by = c("year" = "year",
                     "queried_company_permid" = "oa_perm_id"))
    
    var_name_country <- clean_string(country)
    var_name_commodity <- clean_string(commodity_)
    var_name <- paste0("flows_", var_name_country, "_", var_name_commodity)
    
    assign(var_name, flows_subset)
    
    flows_by_country_commodity_raw[[var_name]] <- flows_subset
}

## 3. Cleaning financial flows ------------------------

### 3.1. Transaction-level data ------------------------
# remove duplicates, counting them
flows_by_country_commodity_clean <- list()
for (country_commodity in names(flows_by_country_commodity_raw)) {
  df <- flows_by_country_commodity_raw[[country_commodity]]
  print(country_commodity)
  df <- df %>%
    tidydim_sum(col_sum = "tranche_amount_usd_m") %>% # give initial count + total
    distinct(producer_country, commodity, facility_id, tranche_id, .keep_all = TRUE) %>% # only one of each deal per country-commodity setting
    tidydim_sum(col_sum = "tranche_amount_usd_m") %>% # give remaining count + total
    filter(!is.na(tranche_amount_usd_m), # empty transactions
           !is.na(facility_amount_usd_m)) %>%
    tidydim_sum(col_sum = "tranche_amount_usd_m") %>% # give remaining count + total
    filter(!is.na(date_close)) %>% # uncompleted transactions (not those with no maturity as these are equity issuances)
    tidydim_sum(col_sum = "tranche_amount_usd_m") # give remaining count + total
  
  flows_by_country_commodity_clean[[country_commodity]] <- df
}


# add addition to commitment amount to handle when there are manager duplicates (due to having multiple roles in a deal)
# the duplicates will be removed later but I need the right number of | to split into rows

for (country_commodity in names(flows_by_country_commodity_clean)) {
  df <- flows_by_country_commodity_clean[[country_commodity]]
  print(country_commodity)
  df <- df %>%
    mutate(
      commitment_amount_usd_m = if_else(
        (str_count(commitment_amount_usd_m, "\\|")) == (str_count(manager_name, "\\|")), 
        commitment_amount_usd_m,
        str_c(commitment_amount_usd_m, str_dup("|0", (str_count(manager_name, "\\|") - str_count(commitment_amount_usd_m, "\\|"))))
      ),
      commitment_amount_orig_curr_m = if_else(
        (str_count(commitment_amount_orig_curr_m, "\\|")) == (str_count(manager_name, "\\|")),
        commitment_amount_orig_curr_m,
        str_c(commitment_amount_orig_curr_m, str_dup("|0", (str_count(manager_name, "\\|") - str_count(commitment_amount_orig_curr_m, "\\|"))))
      )
    )
  flows_by_country_commodity_clean[[country_commodity]] <- df
}

for (country_commodity in names(flows_by_country_commodity_clean)) {
  df <- flows_by_country_commodity_clean[[country_commodity]]
  write_csv(df, paste0("./intermediate-results/flows_clean_transaction_level_",country_commodity,".csv"))
}

### 3.2. Manager-level data ------------------------

#### 3.2.1. split into manager-level dataset ------------------------
flows_by_manager <- list() 
for (country_commodity in names(flows_by_country_commodity_clean)) {
  df <- flows_by_country_commodity_clean[[country_commodity]]
  print(country_commodity)
  # df <- flows_by_country_commodity_clean[['flows_brazil_cattle_meat']] for testing
  df_by_manager <- df %>%
    separate_rows(manager_name,
                  manager_parent_name,
                  manager_role,
                  commitment_amount_usd_m,
                  commitment_amount_orig_curr_m,
                  sep = "\\|") %>% 
    # remove duplicate managers (messy data from refinitiv due to duplicate deal roles) - each manager should only be named once on a tranche
    distinct(tranche_id, manager_name, .keep_all = TRUE) %>% 
    # ensure manager counts are correct
    group_by(tranche_id) %>%
    mutate(no_of_managers_check = n()) %>% # there are no falses now
    ungroup() 
  
  flows_by_manager[[country_commodity]] <- df_by_manager
}

#### 3.2.2. join on true ultimate parents ------------------------
for (country_commodity in names(flows_by_manager)) {
  df <- flows_by_manager[[country_commodity]]
  print(paste("Processing:", country_commodity, "- before ups join"))
  df <- df %>%
    left_join((fi_ups_all %>% select(query, manager_true_ultimate_parent_organisation_name, manager_true_ultimate_parent_company_oa_perm_id)),
              by = c("manager_parent_name" = "query"))
    # join on info re: ultimate parent 
  print(paste("Processing:", country_commodity, "- before info_by_permid join"))
  df <- df %>% left_join(fi_info_by_permid,
              by = c("manager_true_ultimate_parent_company_oa_perm_id" = "permid")) %>%
    # add flag for if the manager is a bookrunner
    mutate(escaped_manager_name = str_replace_all(manager_name, "([.+*?()\\[\\]{}|^$])", "\\\\\\1"),
           bookrunner_list = paste0("|", bookrunner_list, "|"),
           is_bookrunner = if_else(!is.na(bookrunner_list), 
                                   if_else(str_detect(bookrunner_list, regex(paste0("\\|", escaped_manager_name, "\\|"))),
                                           TRUE,
                                           FALSE),
                                   FALSE),
           # make sure commitment amounts, if known, are numeric (previously character string)
           commitment_amount_usd_m = as.numeric(commitment_amount_usd_m),
           commitment_amount_orig_curr_m = as.numeric(commitment_amount_orig_curr_m))
  
  flows_by_manager[[country_commodity]] <- df
}

#### 3.2.3. splitting transaction amounts between financial institutions ------------------------
# setting up bookratio method
book_ratio <- function(no_participants, no_bookrunners) {
  if_else(no_bookrunners > 0,
          (no_participants - no_bookrunners) / no_bookrunners,
          0) # case where no bookrunners is the same as where all participants are bookrunners (i.e., book ratio is 0)
}

# depending on their role in the deal, groups of bookrunners vs participants are assigned a proportion of the deal
# this is different for loans vs capital markets issuances
# methodology from Warmerdam et al. (2020): https://forestsandfinance.org/wp-content/uploads/2020/08/Forests-Finance-financial-research-methodology-01Sep2020.pdf 

prop_to_bookrunners_loans <- function(x) {
  br <- x
  if (br >= 0 && br <= 1/3) {
    1
  } else if (br > 1/3 && br <= 2/3) {
    0.75
  } else if (br > 2/3 && br <= 1.5) {
    0.6
  } else if (br > 1.5 && br <= 3) {
    0.4
  } else {
    (1/sqrt(x))/1.443375673
  }
}

prop_to_participants_loans <- function(x) {
  if (x >= 0 && x <= 1/3) {
    prop_to_bookrunners_loans(x) # Where the bookratio is very low (many bookrunners, few participants), treat all participants equally
  } else {
    1 - prop_to_bookrunners_loans(x) # Where the bookratio is normal, treat participants and bookrunners separately
  }
} 

prop_to_bookrunners_issuances <- function(x) {
  br <- x
  if (br >= 0 && br <= 1/3) {
    1
  } else if (br > 1/3 && br <= 2/3) {
    0.75
  } else if (br > 2/3 && br <= 1.5) {
    0.75
  } else if (br > 1.5 && br <= 3) {
    0.75
  } else {
    (1/sqrt(x))/0.769800358
  }
}

prop_to_participants_issuances <- function(x) {
  if (x >= 0 && x <= 1/3) {
    prop_to_bookrunners_issuances(x) # Where the bookratio is very low (many bookrunners, few participants), treat all participants equally
  } else {
    1 - prop_to_bookrunners_issuances(x) # Where the bookratio is normal, treat participants and bookrunners separately
  }
}

for (country_commodity in names(flows_by_manager)) {
  df <- flows_by_manager[[country_commodity]]
  print(country_commodity)
  if (nrow(df) > 0) { # handle case where no financial flows for that country_commodity
    df_parsed <- df %>%
      # known commitment or allotted amounts - add marker to use only if consistent with reported tranche amount
      group_by(tranche_id) %>% # calculate within tranches
      mutate(sum_for_tranche_commitment_amount_usd_m = sum(commitment_amount_usd_m, na.rm = TRUE),
             pct_dif_total_commitment_vs_tranche_amount = (modulus(sum_for_tranche_commitment_amount_usd_m - tranche_amount_usd_m)/tranche_amount_usd_m)*100) %>%
      ungroup() %>%
      # only use commitment amounts if they are present and sum to within 1% of the tranche amount (to allow for rounding errors)
      mutate(use_commitment_amount = if_else(!is.na(pct_dif_total_commitment_vs_tranche_amount),
                                             if_else(pct_dif_total_commitment_vs_tranche_amount <= 1, 
                                                     TRUE,
                                                     FALSE),
                                             FALSE)) %>%
      # equal split between all managers
      mutate(tranche_amount_per_manager_usd_m_equal_split = tranche_amount_usd_m / no_managers_total) %>%
      # equal split between bookrunners only (unless there are no bookrunners)
      mutate(tranche_amount_per_manager_usd_m_bookrunner_split = if_else(no_bookrunners == 0, # case where there are no bookrunners listed, split between all managers (otherwise deal will not be counted)
                                                                         tranche_amount_usd_m / no_managers_total,
                                                                         if_else(is_bookrunner == TRUE,
                                                                                 tranche_amount_usd_m / no_bookrunners, 
                                                                                 0))) %>% 
      # bookratio method (apportions depending on role in deal)
      mutate(book_ratio = book_ratio(no_managers_total, no_bookrunners), # calculate book ratio
             tranche_amount_per_manager_usd_m_book_ratio = if_else(financial_flow_type == "Lending", 
                                                                   
                                                                   # if transaction is a loan, use the book ratio method for loans
                                                                   if_else(book_ratio > 1/3, # special case if bookratio is below or equal to 1/3 as denominator needs to change to treat all participants equally 
                                                                           if_else(is_bookrunner == TRUE,
                                                                                   (sapply(book_ratio, FUN = prop_to_bookrunners_loans)) * tranche_amount_usd_m * 1/no_bookrunners, # to bookrunner
                                                                                   (sapply(book_ratio, FUN = prop_to_participants_loans)) * tranche_amount_usd_m * (1/(no_managers_total - no_bookrunners))), # to participant
                                                                           1 * tranche_amount_usd_m * 1/no_managers_total), # treat all managers equally
                                                                   
                                                                   # else if transaction is an issuance, use the book ratio method for issuances
                                                                   if_else(book_ratio > 1/3, # special case if bookratio is below or equal to 1/3 as denominator needs to change to treat all participants equally 
                                                                           if_else(is_bookrunner == TRUE,
                                                                                   (sapply(book_ratio, FUN = prop_to_bookrunners_issuances)) * tranche_amount_usd_m * 1/no_bookrunners, 
                                                                                   (sapply(book_ratio, FUN = prop_to_participants_issuances)) * tranche_amount_usd_m * (1/(no_managers_total - no_bookrunners))),
                                                                           1 * tranche_amount_usd_m * 1/no_managers_total)) #If book ratio is equal to or below 1/3, treat bookrunners and participants equally
      ) %>%
      mutate(tranche_amount_per_manager_usd_m_final = if_else(use_commitment_amount == TRUE,
                                                              commitment_amount_usd_m,
                                                              tranche_amount_per_manager_usd_m_book_ratio)) %>%
      select(producer_country, 
             commodity, 
             n_exporter_group, 
             exporter_group,
             legal_entity_name,
             year, 
             everything())
  } else {
    df_parsed <- df
  }
  flows_by_manager[[country_commodity]] <- df_parsed
}

#### 3.2.4. data consistency checks ---------
# define smaller dataset for visually inspecting columns
flows_checking_columns <- c(1:10, 14:15, 20:21, 27, 39:45, 53, 73:82) 

# check whether parsing rounding errors are acceptable # within <0.1% 
for (country_commodity in names(flows_by_manager)) {
  df <- flows_by_manager[[country_commodity]]
  print(country_commodity)
  if (nrow(df) > 0) { 
    df <- flows_by_manager[[country_commodity]]
    flows_total <- sum((df %>% distinct(tranche_id, .keep_all = TRUE))$tranche_amount_usd_m)
    print(paste0("Book ratio diff: ",percent((flows_total - sum((df$tranche_amount_per_manager_usd_m_book_ratio))) / flows_total)), accuracy = 0.001)
    print(paste0("Bookrunner diff: ", percent((flows_total - sum((df$tranche_amount_per_manager_usd_m_bookrunner_split))) / flows_total)), accuracy = 0.001)
    print(paste0("Equal split diff: ", percent((flows_total - sum((df$tranche_amount_per_manager_usd_m_equal_split))) / flows_total)), accuracy = 0.001)
    print(paste0("Final split diff: ", percent((flows_total - sum((df$tranche_amount_per_manager_usd_m_final), na.rm = TRUE)) / flows_total)), accuracy = 0.001)
  }
  else {print(paste0("No rows for: ", country_commodity))}
} # all fine

#### 3.2.5. inflation adjustments ---------------
# adjust all values to Dec 2024 USD
us_cpi_target_value_monthly <- us_cpi_monthly %>% 
  filter(period == "2024-12-01") %>%
  pull(us_cpi_index_this_period)

us_cpi_yearly <- us_cpi_monthly %>%
  mutate(period_year = lubridate::year(period)) %>%
  group_by(period_year) %>%
  summarise(us_cpi_average_this_year = mean(us_cpi_index_this_period, na.rm = TRUE)) %>%
  ungroup() %>%
  select(period_year, us_cpi_average_this_year)

us_cpi_target_value_yearly <- us_cpi_yearly %>% 
  filter(period_year == 2024) %>%
  pull(us_cpi_average_this_year)

cpi_adjustment <- function(df, cols_to_adjust) {
  df %>% # inflation adjustment based on Dec 2024 value
    mutate(date_close_month = floor_date(date_close, unit = "month")) %>%
    left_join(us_cpi_monthly, by = c("date_close_month" = "period")) %>%
    mutate(across(
      all_of(cols_to_adjust),
      ~ (.x * us_cpi_target_value_monthly) / us_cpi_index_this_period,
      .names = "{.col}_in_dec_2024_usd"
    )) %>% # inflation adjustment based on 2024 mean value
    left_join(us_cpi_yearly, by = c("year" = "period_year")) %>%
    mutate(across(
      all_of(cols_to_adjust),
      ~ (.x * us_cpi_target_value_yearly) / us_cpi_average_this_year,
      .names = "{.col}_in_2024_av_usd"
    ))
}

colnames_parsed <- c(
  "tranche_amount_per_manager_usd_m_final",
  "tranche_amount_per_manager_usd_m_book_ratio",
  "tranche_amount_per_manager_usd_m_equal_split"
)

for (country_commodity in names(flows_by_manager)){
  df <- flows_by_manager[[country_commodity]]
  if (nrow(df) > 0) { 
    df <- cpi_adjustment(df, cols_to_adjust = colnames_parsed)
  } else {df <- df}
  flows_by_manager[[country_commodity]] <- df
}

#### 3.2.6. join unsd regions ------------
for (country_commodity in names(flows_by_manager)){
  df <- flows_by_manager[[country_commodity]]
  df <- df %>%
    left_join(regions_unsd, by = c("manager_true_ultimate_parent_country_of_headquarters" = "country")) %>%
    rename(manager_true_ultimate_parent_region_of_headquarters_unsd = region_unsd)
  flows_by_manager[[country_commodity]] <- df
}

### 3.3. Export country-commodity level datasets --------
for (country_commodity in names(flows_by_manager)){
  df <- flows_by_manager[[country_commodity]]
  write_csv(df, paste0("./intermediate-results/flows_clean_manager_level_2008-2024_",country_commodity,".csv"))
}

### 3.4. Export combined dataset --------

flows_by_manager_all_amazon <- tibble()
for (country_commodity in names(flows_by_manager)) {
  df <- flows_by_manager[[country_commodity]] 
  flows_by_manager_all_amazon <- bind_rows(flows_by_manager_all_amazon, df)
}

flows_by_manager_all_amazon <- flows_by_manager_all_amazon %>% group_by(tranche_id) %>%
  mutate(
    country_commodity = paste0(producer_country," - ", commodity),
    n_country_commodity = n_distinct(country_commodity),
    country_commodity_all = paste(unique(country_commodity), collapse = "|"),
    exporter_group_all = paste(unique(exporter_group), collapse = "|")
  ) %>%
  ungroup() %>%
  distinct(tranche_id, manager_parent_name, country_commodity_all, .keep_all = TRUE) %>%
  select(-producer_country, -commodity, -n_exporter_group, -exporter_group, -country_commodity) %>%
  select(country_commodity_all, n_country_commodity, exporter_group_all, everything())

write_csv(flows_by_manager_all_amazon, "./intermediate-results/flows_clean_manager_level_2008_2024_all_amazon.csv")  
