## PROJECT: FINANCIAL FLOWS TO ECOSYSTEM TIPPING POINTS ##
# AIM: PULLING COMPANY DATA FROM REFINTIV DATA PLATFORM
# github.com/lyd-m/wwf-tipping-points

### PREREQUSITES -------------------------
# SET UP AND LOGIN TO EIKON DATA API AND REFINITIV DATA PLATFORM ##
# see instructions online here: https://github.com/LSEG-API-Samples/Example.DataLibrary.Python
# download and install anaconda for python
# create a python environmental called "eikon"
# install eikon into this environment
# select this environment as your python interpreter

### DEPENDENCIES --------------------------
# python=3.11 needed for Refinitiv to work

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

### BEGIN DESKTOP API SESSION ----------------------
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

# SEARCHING COMPANY NAMES
all_companies = pd.read_csv(
    "./intermediate-results/distinct_companies_100ha.csv",
)

all_companies["exporter_group_clean"] = (
    all_companies["exporter_group"]
    .str.lower()  # Convert to lowercase
    .str.replace("-", " ", regex=False)  # Replace hyphen with space
    .str.replace("/", " ", regex=False)  # Replace slash with space
    .str.replace(r"[^\w\s]", "", regex=True)  # Remove all other punctuation
)

to_search = all_companies["exporter_group_clean"].tolist()


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
                    {"query": [company], "source": ["GFW"]}
                )  # Creating a row with the query and source
            else:
                match["query"] = (
                    company  # Adding the original query to the match dataframe
                )
                match["source"] = (
                    "DEDUCE"  # Adding the source column with value 'DEDUCE'
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

company_data.to_csv("./intermediate-results/companies_100ha_rdp_results.csv")
