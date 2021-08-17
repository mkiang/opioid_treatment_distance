## 90_sensitivity_analysis.R ----

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(readxl)
library(janitor)
library(matrixStats)
source(here::here("code", "utils.R"))

## Data ----
urban_rural_codes <-
    readxl::read_excel(here("data_raw", "NCHSURCodes2013.xlsx")) %>%
    janitor::clean_names(.) %>%
    transmute(
        fips = sprintf("%05d", fips_code),
        county_name = county_name,
        urc_code = x2013_code
    )
acs_df <- readRDS(here::here("data", "acs_vars_wide.RDS"))
cleaned_df <- read_csv(here::here("data", "cleaned_provider_list.csv"))
distances_df <- bind_rows(readRDS(here("data", "tract_distances_all.RDS")),
                          readRDS(here("data", "tract_distances_bupe.RDS")),
                          readRDS(here("data", "tract_distances_otp.RDS")))

## Join with population and urban/rural codes ----
distances_df <- distances_df %>%
    left_join(urban_rural_codes)

## For each location (lat/lon), get the number of providers
provs_per_location <- cleaned_df %>%
    filter(download == "2020-07-04") %>% 
    group_by(latitude, longitude) %>% 
    summarize(n_providers = n_distinct(name)) %>% 
    rename(prov_lat = latitude,
           prov_lon = longitude) %>% 
    ungroup()
distances_df <- distances_df %>% 
    left_join(provs_per_location)

## Now expand the distance matrix with a duplicate row for duplicate addresses
distances_df <- distances_df %>% 
    uncount(n_providers, .remove = FALSE) %>% 
    group_by(providers, st_fips, geoid, fips, metric) %>% 
    arrange(distance, .by_group = TRUE) %>% 
    mutate(new_rank = 1:n(),
           abs_diff_1 = distance - min(distance, na.rm = TRUE),
           rel_diff_1 = (distance - min(distance, na.rm = TRUE)) / 
               min(distance, na.rm = TRUE)) %>% 
    ungroup()

## Create a tract-specific dataframe ----
## Each row is a census tract with one of the top 20 nearest facilities
tract_df <- distances_df %>% 
    filter(new_rank <= 20) %>% 
    dplyr::left_join(acs_df %>%
                         dplyr::rename(geoid = GEOID) %>%
                         select(-NAME)) %>%
    select(providers,
           fips,
           county_name,
           urc_code,
           st_name,
           metric,
           distance,
           rank,
           new_rank, 
           everything())

## Summarize into counties ----
## Going to collapse tracts into counties and summarize the mean, median, and 
## sd using unweighted estimates, population-weights, and population multiplier.
## We are also interested in the actual distance, but we want to know the
## relative and absolute distances compared to the nearest facility. So type =
## {distance, absolute_difference, and relative_difference} provide different
## metric types. 
county_df <- bind_rows(
    tract_df %>%
        group_by(
            providers,
            fips,
            county_name,
            urc_code,
            metric,
            new_rank,
            st_name,
            st_fips
        ) %>%
        summarize(
            w_mean = w_mean(distance, pop_total),
            w_sd = w_sd(distance, pop_total),
            w_median = w_median(distance, pop_total),
            mean = mean(distance, na.rm = TRUE),
            sd = stats::sd(distance, na.rm = TRUE),
            median = stats::median(distance, na.rm = TRUE),
            pop_mean = mean(distance * pop_total, na.rm = TRUE),
            pop_sd = stats::sd(distance * pop_total, na.rm = TRUE),
            pop_median = stats::median(distance * pop_total, na.rm = TRUE)
        ) %>%
        mutate(type = "distance"),
    tract_df %>%
        group_by(
            providers,
            fips,
            county_name,
            urc_code,
            metric,
            new_rank,
            st_name,
            st_fips
        ) %>%
        summarize(
            w_mean = w_mean(rel_diff_1, pop_total),
            w_sd = w_sd(rel_diff_1, pop_total),
            w_median = w_median(rel_diff_1, pop_total),
            mean = mean(rel_diff_1, na.rm = TRUE),
            sd = stats::sd(rel_diff_1, na.rm = TRUE),
            median = stats::median(rel_diff_1, na.rm = TRUE),
            pop_mean = mean(rel_diff_1 * pop_total, na.rm = TRUE),
            pop_sd = stats::sd(rel_diff_1 * pop_total, na.rm = TRUE),
            pop_median = stats::median(rel_diff_1 * pop_total, na.rm = TRUE)
        ) %>%
        mutate(type = "relative_diff"),
    tract_df %>%
        group_by(
            providers,
            fips,
            county_name,
            urc_code,
            metric,
            new_rank,
            st_name,
            st_fips
        ) %>%
        summarize(
            w_mean = w_mean(abs_diff_1, pop_total),
            w_sd = w_sd(abs_diff_1, pop_total),
            w_median = w_median(abs_diff_1, pop_total),
            mean = mean(abs_diff_1, na.rm = TRUE),
            sd = stats::sd(abs_diff_1, na.rm = TRUE),
            median = stats::median(abs_diff_1, na.rm = TRUE),
            pop_mean = mean(abs_diff_1 * pop_total, na.rm = TRUE),
            pop_sd = stats::sd(abs_diff_1 * pop_total, na.rm = TRUE),
            pop_median = stats::median(abs_diff_1 * pop_total, na.rm = TRUE)
        ) %>%
        mutate(type = "absolute_diff")
)

## Reorganize the columns 
county_df <- county_df %>%
    ungroup() %>%
    select(providers,
           fips,
           county_name,
           urc_code,
           type,
           metric,
           new_rank,
           everything())

## Save ----
saveRDS(tract_df %>% 
            filter(providers == "All"),
        here::here("data", "sensitivity_data_tract_all.RDS"),
        compress = "xz")
saveRDS(tract_df %>% 
            filter(providers == "Bupe"),
        here::here("data", "sensitivity_data_tract_bupe.RDS"),
        compress = "xz")
saveRDS(tract_df %>% 
            filter(providers == "OTP"),
        here::here("data", "sensitivity_data_tract_otp.RDS"),
        compress = "xz")
saveRDS(county_df,
        here::here("data", "sensitivity_data_county_all.RDS"),
        compress = "xz")
