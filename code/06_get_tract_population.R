## Imports ----
library(tidyverse)
library(tidycensus)
library(here)
library(fs)
source(here("code", "secrets.R"))

## Constants ----
acs_var_list <- list(
    "B01001_001"  = "pop_total",
    "B01001A_001" = "pop_white",
    "B01001B_001" = "pop_black",
    "B01001H_001" = "pop_nonhispwhite",
    "B01001I_001" = "pop_hispanic",
    "B06011_001"  = "income_median"
)
acs_states <- c("DC", datasets::state.abb)

## Check first ----
if (!fs::file_exists(here::here("data", "acs_vars_long.RDS"))) {
    ## Import ACS data for each tract ----
    acs_df <- tidycensus::get_acs(
        variables = names(acs_var_list), 
        state = acs_states,
        year = 2017,
        geography = "tract",
        cache_table = TRUE,
        geometry = FALSE
    )
} else {
    acs_df <- readRDS(here::here("data", "acs_vars_long.RDS"))
}

## Convert to wide but keep original (for MOE) ----
acs_wide <- acs_df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(var_name = acs_var_list[[variable]]) %>% 
    dplyr::select(-variable, -moe) %>% 
    tidyr::spread(var_name, estimate)

## Save ----
saveRDS(acs_df, here::here("data", "acs_vars_long.RDS"))
saveRDS(acs_wide, here::here("data", "acs_vars_wide.RDS"))
