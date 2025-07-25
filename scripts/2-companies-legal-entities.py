# AMAZON FOOTPRINT IN FOOD AND FINANCIAL SYSTEMS --------
# Financial flows analysis: Stage 2 - Legal entity identification -------
# github.com/lyd-m/amazon-footprint
# This project contains the financial analysis for the paper, Singh et al. (2025)
# This script takes a company list identified previously and maps it to legal entities in the LSEG database.
# There is some manual work that is completed after this script via an Excel file.

### PREREQUSITES -------------------------
# SET UP AND LOGIN TO EIKON DATA API AND REFINITIV DATA PLATFORM ##
# see instructions online here: https://github.com/LSEG-API-Samples/Example.DataLibrary.Python
# download and install anaconda for python
# create a python environmental called "eikon"
# install eikon into this environment
# select this environment as your python interpreter

### DEPENDENCIES --------------------------
# python=3.11 needed for Refinitiv to work - use eikon environment for this script

### DIRECTORIES --------------------------
# refinivit/lseg files
import refinitiv.data as rd
from refinitiv.data.errors import RDError
from refinitiv.data.content import search

# data analysis and logging
import pandas as pd
import numpy as np
import openpyxl as xl
import time
import datetime
from rapidfuzz import fuzz, process

# other
import os  # working directories
import re  # regex
import glob


path = "/Users/ucliipp/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Documents/programming/main-projects/amazon-footprint"
os.chdir(path)

### import company data and arrange for searching ----------
companies = pd.read_csv(
    "./analytical-results/companies.csv",
)

companies["exporter_group_clean"] = (
    companies["exporter_group"]
    .str.lower()  # Convert to lowercase
    .str.replace("-", " ", regex=False)  # Replace hyphen with space
    .str.replace("/", " ", regex=False)  # Replace slash with space
    .str.replace(r"[^\w\s&]", "", regex=True)  # Remove all other punctuation except &
    .str.replace("  ", " ", regex=False)  # Remove double spacing
    .str.replace("   ", " ", regex=False)  # Remove triple spacing
)

to_search = companies["exporter_group_clean"].unique().tolist()
len(to_search)  # 512 unique companies

### begin desktop API session ----------------------
# set Refinitiv Data Platform API login key as "app_key_rd" in local env via terminal
# this is necessary due to licence restrictions
# Desktop Refinitiv App needs to be open to do this
# open desktop API session
app_key_rd = "[define API key in local environment]"
rd.session.desktop.Definition(app_key=app_key_rd)
session = rd.session.desktop.Definition(app_key=app_key_rd).get_session()
rd.open_session()


# test to see if session is open
def rd_session_test():
    test = rd.get_data(universe="MSFT.O", fields=["TR.CommonName"])
    return test


rd_session_test().notna().any().any()  # Should return a non-empty dataframe, will read "True" if passed

### search company names ----------------------
company_data = pd.DataFrame()

retry_count = 1
retry_max = 5

start_exec = time.time()
for company in to_search:
    retry = True
    while retry:
        try:
            match = rd.discovery.search(
                view=rd.discovery.Views.ORGANISATIONS,
                query=company,
                top=1,  # choose best match
                select="CommonName, OAPermID, ParentOrganisationName, ParentCompanyOAPermID, UltimateParentOrganisationName, UltimateParentCompanyOAPermID",
            )
            if match.empty:
                match = pd.DataFrame(
                    {"query": [company], "source": ["DeDuCE"]}
                )  # Creating a row with the query and source
            else:
                match["query"] = (
                    company  # Adding the original query to the match dataframe
                )
                match["source"] = (
                    "DeDuCE"  # Adding the source column with value 'DEDUCE'
                )

            if not company_data.empty:
                company_data = pd.concat(
                    [company_data, match], axis=0, ignore_index=True
                )
            else:
                company_data = match

            retry = (
                False  # No exception occurred, so set retry to False to exit the loop
            )
        except Exception as e:
            print(f"An error occurred: {e}")
            if retry_count <= retry_max:
                print("Retrying...")
                retry_count += 1
                time.sleep(1)  # Wait for 1 second before retrying, seems long enough
            else:
                print("Retry limit reached.")
                break
end_exec = time.time()
print(f"This code took {end_exec - start_exec} to run")

# check matches for quality


def clean_text(text):
    # Convert non-string data to string
    text = (
        str(text).strip().lower()
    )  # Strip leading/trailing whitespace and convert to lowercase

    # Remove all special characters except spaces
    # text = re.sub(r"[^\w\s]", '', text)
    text = re.sub(r"[^\w\s]|-", " ", text)

    # Replace standalone 'ltd' and 'co' using word boundaries
    # Ensure replacements are done in a non-overlapping manner
    text = re.sub(r"\blimited\b\.?", "ltd", text)
    text = re.sub(r"\bcompany\b\.?", "co", text)
    text = re.sub(r"\bincorporated\b\.?", "inc", text)
    text = re.sub(r"\bcorporation\b\.?", "corp", text)

    return text


# Apply the cleaning function to the 2nd and 3rd columns
company_data["query_clean"] = company_data["query"].apply(clean_text)
company_data["CommonName_clean"] = company_data["CommonName"].apply(clean_text)


# function to compute similarity
def fuzzy_match(row):
    return round(
        fuzz.ratio(row["query_clean"], row["CommonName_clean"]), 3
    )  # returns similarity %


# compare two columns via fuzzy matching
company_data["similarity"] = company_data.apply(fuzzy_match, axis=1)

# check for gaps
columns_to_check = [
    "OAPermID",
    "ParentOrganisationName",
    "ParentCompanyOAPermID",
    "UltimateParentOrganisationName",
    "UltimateParentCompanyOAPermID",
]

# gaps column is "True" if any specified column is empty, False otherwise
company_data["gaps"] = (
    company_data[columns_to_check].replace("", np.nan).isna().any(axis=1)
)


# check for any government organisations
keywords = [
    r"\(government\)",
    "republic of",
    "city of",
    "government of",
    "province of",
    "municipality of",
    "state of",
    "emirate of",
    "canton of",
    "kingdom of",
    "commonwealth of",
    "confederation of",
]

pattern = "|".join(keywords)  # i.e., if contains ANY of the keywords

company_data["has_government_parent"] = company_data[
    "UltimateParentOrganisationName"
].str.contains(pattern, case=False, na=False)

# flag columns for manual checking
company_data["manual_check_needed"] = (
    (company_data["similarity"] < 70)
    | (company_data["gaps"] == True)
    | (company_data["has_government_parent"] == True)
).map({True: "TRUE", False: "FALSE"})

# rename all initial columns with _initial

company_data.rename(
    columns={
        "CommonName": "CommonName_initial",
        "OAPermID": "OAPermID_initial",
        "ParentOrganisationName": "ParentOrganisationName_initial",
        "ParentCompanyOAPermID": "ParentCompanyOAPermID_initial",
        "UltimateParentOrganisationName": "UltimateParentOrganisationName_initial",
        "UltimateParentCompanyOAPermID": "UltimateParentCompanyOAPermID_initial",
    }
)

# export to csv for manual checking
company_data.to_csv("./intermediate-results/exporter_groups_legal_entities_initial.csv")

# manual check completed in Excel (with Refinitiv searching to fill gaps)

company_data_final = pd.read_excel(
    "./intermediate-results/exporter_groups_legal_entities_final.xlsx"
)

company_data_final["legal_entity_hierarchy_year"] = 2025

# join onto original dataframe and resave

companies = companies.merge(
    company_data_final.loc[
        :,
        [
            "query",
            "legal_entity_mapped",
            "legal_entity_hierarchy_year",
            "CommonName",
            "OAPermID",
            "ParentOrganisationName",
            "ParentCompanyOAPermID",
            "UltimateParentOrganisationName",
            "UltimateParentCompanyOAPermID",
        ],
    ],
    left_on="exporter_group_clean",
    right_on="query",
    how="left",
)

companies = companies.drop(columns=["query", "exporter_group_clean"])

# ensuring have an entry for each year
years = list(range(2008, 2025))
unique_companies_commodities = companies[
    [
        "producer_country",
        "commodity",
        "exporter_group",
        "years_appeared",
        "years_available",
        "deduce_temporal_pattern",
        "legal_entity_mapped",
    ]
].drop_duplicates()
# expand grid to get all combinations of years and unique companies
unique_companies_commodities["key"] = 1
years_df = pd.DataFrame({"year": years})
years_df["key"] = 1
all_years_companies_combo = pd.merge(
    unique_companies_commodities, years_df, on="key"
).drop("key", axis=1)

# keep only those combinations not present in the main data
missing_companies_years = (
    all_years_companies_combo.merge(
        right=companies.loc[
            :, ["producer_country", "commodity", "exporter_group", "year"]
        ].drop_duplicates(),
        on=["producer_country", "commodity", "exporter_group", "year"],
        how="left",
        indicator=True,
    )
    .query('_merge == "left_only"')
    .drop("_merge", axis=1)
)

companies_complete_years = pd.concat(
    [companies, missing_companies_years], ignore_index=True
)
# export to csv, then adjust manually in Excel to map changes in hierarchy
# (e.g., if a company is acquired, the legal entity hierarchy may change)

# save to csv to manually change hierarchy
companies_complete_years.to_csv(
    "./intermediate-results/companies_temporal_hierarchy_final.csv", index=False
)

# repull back in and update the main companies dataframe
companies_complete_years_final = pd.read_excel(
    "./intermediate-results/companies_temporal_hierarchy_final.xlsx"
)

companies_complete_years_final.to_csv(
    "./analytical-results/companies_all_years.csv", index=False
)
