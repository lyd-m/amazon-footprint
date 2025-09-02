# Amazon footprint in food and financial systems

This repository contains the financial data analysis pipeline (authored by Lydia Marsden) for the preprint, Singh et al. (2025) The Amazon's footprint in food and financial systems.

------------------------------------------------------------
## Overview
- Which global financial centres and institutions provide private financial flows to exporters linked to deforestation in the Amazon?
- Which countries and individual financial institutions are exposed to risks from Amazon deforestation in supply chains?
- This project maps financial flows to companies linked to Amazon deforestation, using the most granular firm-level deforestation data available (see other sections of this study).
- It does so by combining multiple datasets to reconstruct the legal entities involved, their historical legal hierarchies, and their external financing over 2008-2024.
- This repository contains the workflow to reproduce this analysis (noting that financial data from LSEG is only available under licence).

------------------------------------------------------------
## Repository Structure

input-data/            Raw input datasets (publicly available data)
intermediate-results/  Confidential financial data (from LSEG), processed or cleaned data, derived datasets
analytical-results/    Statistical summaries of data, in aggregate format to comply with licencing terms
figures/               Initial plots to support final figures (designed separately in Graphica)
scripts/               Scripts for data processing, analysis, and draft visualisation
README.txt             Documentation

------------------------------------------------------------
## Requirements
- R 4.4.1
- Python 3.11 (not a later version - this is needed for the LSEG Refinitiv API to work)
- Refinitiv Data Platform installed in a relevant Python environment
- Packages/libraries contained in individual source code files
- 
------------------------------------------------------------
## Data Access
- Deforestation data attributed to companies was derived from the earlier stages of analysis in Singh et al. (2025), using DeDuCE and SEI-Trase data (publicly available in input-data)
- Company hierarchies were reconstructed using a combination of LSEG PermID data and desk-based research and are provided in analytical-results
- Financial flows data is only available under licence from LSEG - however the workflow to reproduce our data is available in this repository for those who have a licence.

------------------------------------------------------------
## Running the Analysis

##### create company list
scripts/1-companies.R

##### find initial legal entity matches
scripts/2-companies-legal-entities.py

##### manual construction of historical legal hierarchies
analytical-results/companies_all_years.csv

##### pull financial flows
scripts/3-financial-flows.py

##### map financial institutions involved in financial flows
scripts/4-ultimate-parents-mapping.py

##### manual checks of poor matches of ultimate parents
confidential data saved in intermediate-results

##### pull characteristics of companies and financial institutions
scripts/5-info-by-permid.py

##### clean financial flows, match with deforestation data, and split between financial institutions
scripts/6-clean-financial-data.R

##### match financial flows to deforestation time periods and attribute as high-low confidence
scripts/7-attribute-financial-data.R

##### exploratory data analysis and figures
scripts/8-figures.R

------------------------------------------------------------
## Citation
Marsden (2025) Financial data analysis for: The Amazon's footprint in food and financial systems

