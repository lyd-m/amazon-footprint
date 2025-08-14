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

### 0. WORKING DIRECTORY ------------------------
path = "/Users/ucliipp/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Documents/programming/main-projects/amazon-footprint"
os.chdir(path)


### 1. SIMPLE FUNCTIONS ------------------------
def to_snake_case(str):
    snake_case_string = re.sub(r"([a-z0-9])([A-Z])", r"\1_\2", str)
    snake_case_string = snake_case_string.replace(" ", "_").lower()
    return snake_case_string


### 2. BEGIN API SESSION ----------------------
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

### 3. INPUT COMPANY DATA ------------------------
# read in the company data from the previous script

df_companies = pd.read_csv("./analytical-results/companies_all_years.csv").\
    drop(columns=["Unnamed: 0", "legal_entity_hierarchy_year"])  # drop index column

# collect all permids into one longer df

deduce_ref_columns = [
    "producer_country",
    "commodity",
    "year",
    "exporter_group",
    "years_appeared",
    "years_available",
    "deduce_temporal_pattern",
]

df_companies_permids = pd.concat(
    [
        df_companies.loc[:, deduce_ref_columns + ["OAPermID"]],
        df_companies.loc[:, deduce_ref_columns + ["ParentCompanyOAPermID"]],
        df_companies.loc[:, deduce_ref_columns + ["UltimateParentCompanyOAPermID"]],
        df_companies.loc[:, deduce_ref_columns + ["JointVentureCompanyOAPermID"]]
    ]
)

df_companies_permids = (
    pd.melt(
        df_companies_permids,
        id_vars=deduce_ref_columns,
        var_name="hierarchy_level",
        value_name="permid",
    )
    .dropna(subset=["permid"])
    .reset_index()
    .drop_duplicates()
)  # ensure only one entry for each country-commodity-year-exporter_group combination

# ensure strings for searching
df_companies_permids["permid"] = df_companies_permids["permid"].apply(
    lambda x: str(int(x)) if x == x else ""
)
df_companies_permids["year"] = df_companies_permids["year"].astype(str)

# now we should have a yearly list of permids to search (for each country-commodity-year-exporter_group combination)
# create a small list of unique year-permid combinations to save searching time, which can then be joined back onto the main df with the information
df_companies_permids_small = df_companies_permids.loc[
    :, ["permid", "year"]
].drop_duplicates()

# test data for checking code working
permids_test = ["5086635324", "4295903463"]
years_test = [str(year) for year in range(2014, 2025)]  # Convert years to strings
df_companies_permids_test = pd.DataFrame(
    [(perm, year) for perm in permids_test for year in years_test],
    columns=["permid", "year"],
)

### DATA ITEM FIELDS ------------------------
# years to pull the data for
yrs = list(range(2008, 2025))  # 2008 to 2024
yrs = list(map(str, yrs))  # convert to strings for searching

flds = pd.read_excel("./input-data/lseg_columns_needed.xlsx")

asset_classes_flows = [
    "Bond deals",
    "Equity deals",
    "Loan deals",
]  # seem to need to do capital markets first otherwise it breaks...

# getting the fields for each asset class
flds_dict_flows = {
    asset: flds.loc[
        flds["Asset class"].str.contains(asset, na=False), "LSEG field name"
    ].tolist()
    for asset in asset_classes_flows
}


### PULL FINANCIAL FLOWS ------------------------
# function to dynamically create global variables to create output dataframes
def create_variable(name, value):
    globals()[name] = value


# allow for multiple retries in case server times out
retry_max = 5
retry_count = 1

# strings that define the universes for each asset class
universes_dict_flows = {
    "Loan deals": "SCREEN(U(IN(DEALS)/*UNV:DEALSLOAN*/),",
    "Bond deals": "SCREEN(U(IN(DEALS)/*UNV:DEALSBOND*/),TR.NIisECM=False,",
    "Equity deals": "SCREEN(U(IN(DEALS)/*UNV:DEALSEQ*/),TR.NIisECM=True,",
}

participants_dict_flows = {
    "Loan deals": "IN(TR.LNParticipant(LNPartRole=LNB,LNBIP,LNBUP),",
    "Bond deals": "IN(TR.NIParticipant(NIDealPartRole=IS,ISIP,ISUP),",
    "Equity deals": "IN(TR.NIParticipant(NIDealPartRole=IS,ISIP,ISUP),",
}

deals_status_dict = {
    "Loan deals": 'IN(TR.LNStatusOfLoan,"5","4","C"))',
    "Bond deals": 'IN(TR.NITransactionStatus,"LIVE"))',
    "Equity deals": 'IN(TR.NITransactionStatus,"LIVE"))',
}

# Defining parallel processing function ---------------
# using concurrent.futures for parallel processing
# this allows us to process each asset class in parallel, which can speed up the data retrieval
!pip install concurrent.futures
import concurrent.futures

## Global parameters defined above this (yrs, df_companies_permids, flds_dict_flows, etc.)

def process_asset_class(asset_class):
    df = pd.DataFrame()

    for yr in yrs:
        permids_companies_this_yr = df_companies_permids_small.loc[
            df_companies_permids_small["year"] == yr, "permid"
        ].tolist()

        print(f"Year: {yr}, PermIDs found: {len(permids_companies_this_yr)}")
        print(
            f"Processing {asset_class} for year {yr} with {len(permids_companies_this_yr)} permIDs"
        )

        date_start = yr + "0101"
        date_end = yr + "1231"
        dates_dict_flows = {
            "Loan deals": f"),BETWEEN(TR.LNTrancheClosingDate,{date_start},{date_end}),",
            "Bond deals": f"),BETWEEN(TR.NIIssueDate,{date_start},{date_end}),",
            "Equity deals": f"),BETWEEN(TR.NIIssueDate,{date_start},{date_end}),",
        }

        for permid in permids_companies_this_yr:
            retry_count = 1
            retry = True

            while retry:
                try:
                    query = (
                        universes_dict_flows[asset_class]
                        + participants_dict_flows[asset_class]
                        + permid
                        + dates_dict_flows[asset_class]
                        + deals_status_dict[asset_class]
                    )

                    if retry_count == 1:
                        print(f"Sample query for {asset_class} ({yr}): {query}")

                    current_df = rd.get_data(
                        universe=[query], fields=flds_dict_flows[asset_class]
                    )

                    if current_df.empty:
                        print(
                            f"No data found for {asset_class} ({yr}) with permid {permid}. Continuing without joining on..."
                        )
                    else:
                        current_df = current_df.drop_duplicates()
                        current_df["queried_company_permid"] = permid
                        current_df["asset_class"] = asset_class
                        df = pd.concat(
                            [df, current_df.reset_index(drop=True)],
                            ignore_index=True,
                            axis=0,
                        )

                    retry = False

                except Exception as e:
                    print(f"An error occurred with {permid} ({yr}): {e}")
                    if retry_count <= retry_max:
                        print("Retrying...")
                        retry_count += 1
                        time.sleep(0.01)
                    else:
                        print(f"Retry limit reached, skipping {permid} ({yr})")
                        retry = False
                        break

    df_asset_class_name = "df_" + to_snake_case(asset_class)
    return df_asset_class_name, df

# -------- Execute the parallel processing -------

start_exec = time.time()

with concurrent.futures.ProcessPoolExecutor(max_workers=3) as executor:
    results = list(executor.map(process_asset_class, asset_classes_flows))

# Assign results to global variables
for df_name, df in results:
    globals()[df_name] = df
    print(f"Assigned {df_name} to global scope")

end_exec = time.time()
print(f"This code took {end_exec - start_exec:.2f} seconds to run")

