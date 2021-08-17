## README

- `TreatmentProgram_YYYYMMDD.xls` was downloaded from the [SAMHSA Opioid Treatment Program Directory](https://dpt2.samhsa.gov/treatment/directory.aspx) on 2/17/2020 and on 7/4/2020. I opened it in excel and converted it to a csv because it wouldn't open reliably in R.
- `Physician_Locator_2020-02-17T17-59-58.csv` was downloaded from the [SAMHSA Buprenorphine Practioner Locator]([https://www.samhsa.gov/medication-assisted-treatment/practitioner-program-data/treatment-practitioner-locator?distance%5Bpostal_code%5D=94158&distance%5Bsearch_distance%5D=8000&distance%5Bsearch_units%5D=mile&field_bup_physician_city_value=&field_bup_physician_us_state_value=All](https://www.samhsa.gov/medication-assisted-treatment/practitioner-program-data/treatment-practitioner-locator?distance[postal_code]=94158&distance[search_distance]=8000&distance[search_units]=mile&field_bup_physician_city_value=&field_bup_physician_us_state_value=All)) on 2/17/2020.
- All files in `Physician_Locator_by_state` were downloaded from the (new) [SAMHSA Buprenorphine Practioner Locator](https://www.samhsa.gov/medication-assisted-treatment/practitioner-program-data/treatment-practitioner-locator/results/94158/8000/_none/_none?page=1014) on 7/4/2020. Due to API limitations, we had to download each state individually and we will combine them later. 
- `ruralurbancodes2013.xls` was downloaded from the [USDA website](https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx) on 2/18/2020.
  - Note: Decided not to use these because the JAMA letter uses NCHS version.
- `NCHSURCodes2013.xlsx` was downloaded from the [NCHS website](https://www.cdc.gov/nchs/data_access/urban_rural.htm#Data_Files_and_Documentation) on 2/18/2020.



