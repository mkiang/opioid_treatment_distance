## Imports ----
library(tidyverse)
library(tidycensus)
library(here)
library(fs)
source(here("code", "secrets.R"))
source(here("code", "utils.R"))

## Data ---- 
urban_rural_codes <-
    readxl::read_excel(here::here("data_raw", "NCHSURCodes2013.xlsx")) %>%
    janitor::clean_names(.) %>%
    dplyr::transmute(
        fips = sprintf("%05d", fips_code),
        county_name = county_name,
        urc_code = x2013_code
    )

## Constants ----
car_ownership = list(
    "B08201_001" = "total_households",
    "B08201_002" = "zero_vehicle_hh"
)

## Pull ACS ----
if (!file_exists(here::here("data", "acs_car_ownership_long.RDS"))) {
    car_df <- tidycensus::get_acs(
        variables = names(car_ownership), 
        state = acs_states,
        year = 2019,
        geography = "county",
        cache_table = TRUE,
        geometry = FALSE
    )
    
    saveRDS(car_df, here::here("data", "acs_car_ownership_long.RDS"))
} else {
    car_df <- readRDS(here::here("data", "acs_car_ownership_long.RDS"))
}

## Reshape to wide ----
car_wide <- car_df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(var_name = car_ownership[[variable]]) %>% 
    dplyr::select(-variable, -moe) %>% 
    tidyr::spread(var_name, estimate)

## Merge
car_wide <- car_wide %>% 
    left_join(
        urban_rural_codes,
        by = c("GEOID" = "fips")
    )

car_wide <- expand_by_urc_codes(car_wide)

print_table <- car_wide %>%
    group_by(urc_code, urc_cat) %>%
    summarize(
        total_hh = sum(total_households),
        total_zero_vehicle_hh = sum(zero_vehicle_hh)
    ) %>% 
    mutate(prop_zero_v = total_zero_vehicle_hh / total_hh)

print_table