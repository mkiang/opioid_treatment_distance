## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

fs::dir_create("./output")

## Data ----
county_df <-
    readRDS(here::here("data", "sensitivity_data_county_all.RDS"))
tract_df <-
    bind_rows(readRDS(here::here("data", "sensitivity_data_tract_all.RDS")),
              readRDS(here::here("data", "sensitivity_data_tract_bupe.RDS")),
              readRDS(here::here("data", "sensitivity_data_tract_otp.RDS"))) %>%
    rename(pop = pop_total) %>%
    mutate(provider_cat = factor(
        providers,
        levels = c("All", "Bupe", "OTP"),
        labels = c(
            "Both",
            "Buprenorphine Practitioner",
            "Opioid Treatment Program"
        ),
        ordered = TRUE
    ))

## Expand by URC ----
## We want to expand by URC code so we can make tables that include all or
## urban/rural as well as each individual code.
super_county <- expand_by_urc_codes(county_df)
super_tract <- expand_by_urc_codes(tract_df)

## County-level summary ----
county_summary <- super_tract %>%
    filter(new_rank == 1,
           urc_code < 10) %>%
    group_by(providers,
             fips,
             county_name,
             urc_code,
             urc_cat,
             st_name,
             abbrev,
             metric,
             new_rank) %>% 
    summarize(
        n_tracts = n_distinct(geoid),
        n_missing = sum(is.na(distance)),
        pop_weighted_mean = w_mean(distance, pop),
        pop = sum(pop, na.rm = TRUE),
        mean = mean(distance, na.rm = TRUE),
        sd = sd(distance, na.rm = TRUE),
        p025 = quantile(distance, .025, na.rm = TRUE),
        p250 = quantile(distance, .25, na.rm = TRUE),
        median = median(distance, na.rm = TRUE),
        p750 = quantile(distance, .75, na.rm = TRUE),
        p975 = quantile(distance, .975, na.rm = TRUE),
        min = min(distance, na.rm = TRUE),
        max = max(distance, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(providers, metric, fips) %>%
    select(
        providers,
        fips,
        county_name,
        urc_code,
        urc_cat,
        st_name,
        abbrev,
        metric,
        n_tracts,
        n_missing,
        pop,
        mean,
        pop_weighted_mean,
        everything()
    )

urc_summary <- super_tract %>%
    filter(!is.na(new_rank)) %>% 
    group_by(providers,
             urc_code,
             urc_cat,
             metric, 
             new_rank) %>% 
    summarize(
        n_tracts = n_distinct(geoid),
        n_counties = n_distinct(fips),
        n_missing = sum(is.na(distance)),
        pop_weighted_mean = w_mean(distance, pop),
        pop = sum(pop, na.rm = TRUE),
        mean = mean(distance, na.rm = TRUE),
        sd = sd(distance, na.rm = TRUE),
        p025 = quantile(distance, .025, na.rm = TRUE),
        p250 = quantile(distance, .25, na.rm = TRUE),
        median = median(distance, na.rm = TRUE),
        p750 = quantile(distance, .75, na.rm = TRUE),
        p975 = quantile(distance, .975, na.rm = TRUE),
        min = min(distance, na.rm = TRUE),
        max = max(distance, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(providers, metric, new_rank, urc_cat) %>%
    select(
        providers,
        urc_code,
        urc_cat,
        metric,
        new_rank, 
        n_tracts,
        n_counties, 
        n_missing,
        pop,
        mean,
        pop_weighted_mean,
        everything()
    )

## Summary tables ----
write_csv(county_summary,
          "./output/sensitivity_raw_tableS01_county_summary.csv")
write_csv(urc_summary,
          "./output/sensitivity_raw_tableS02_urban_rural_summary_by_new_rank.csv")

print_urc <- urc_summary %>%
    filter(new_rank == 1, metric == "driving_time") %>%
    transmute(
        providers, 
        urc_cat,
        n_tracts,
        n_counties,
        pop,
        mean_sd = sprintf("%0.1f (%0.1f)",
                          round(pop_weighted_mean, 1),
                          round(sd, 1)),
        median_iqr = sprintf(
            "%0.1f (%0.1f, %0.1f)",
            round(median, 1),
            round(p250, 1),
            round(p750, 1)
        )
    )

write_csv(print_urc,
          "./output/sensitivity_table1_summary.csv")
