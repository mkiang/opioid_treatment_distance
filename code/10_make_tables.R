## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

fs::dir_create("./output")

## Data ----
county_df <-
    readRDS(here::here("data", "analytic_data_county_all.RDS"))
tract_df <-
    dplyr::bind_rows(readRDS(here::here("data", "analytic_data_tract_all.RDS")),
                     readRDS(here::here("data", "analytic_data_tract_bupe.RDS")),
                     readRDS(here::here("data", "analytic_data_tract_otp.RDS"))) %>%
    dplyr::rename(pop = pop_total) %>%
    dplyr::mutate(provider_cat = factor(
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
    dplyr::filter(rank == 1,
                  urc_code < 10) %>%
    dplyr::group_by(providers,
                    fips,
                    county_name,
                    urc_code,
                    urc_cat,
                    st_name,
                    abbrev,
                    metric,
                    rank) %>%
    dplyr::summarize(
        n_tracts = dplyr::n_distinct(geoid),
        n_missing = sum(is.na(distance)),
        pop_weighted_mean = w_mean(distance, pop),
        pop = sum(pop),
        mean = mean(distance),
        sd = stats::sd(distance),
        p025 = stats::quantile(distance, .025),
        p250 = stats::quantile(distance, .25),
        median = stats::median(distance),
        p750 = stats::quantile(distance, .75),
        p975 = stats::quantile(distance, .975),
        min = min(distance),
        max = max(distance)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(providers, metric, fips) %>%
    dplyr::select(
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
        dplyr::everything()
    )

urc_summary <- super_tract %>%
    dplyr::filter(!is.na(rank)) %>%
    dplyr::group_by(providers,
                    urc_code,
                    urc_cat,
                    metric,
                    rank) %>%
    dplyr::summarize(
        n_tracts = dplyr::n_distinct(geoid),
        n_counties = dplyr::n_distinct(fips),
        n_missing = sum(is.na(distance)),
        pop_weighted_mean = w_mean(distance, pop),
        pop = sum(pop, na.rm = TRUE),
        mean = mean(distance, na.rm = TRUE),
        sd = stats::sd(distance, na.rm = TRUE),
        p025 = stats::quantile(distance, .025, na.rm = TRUE),
        p250 = stats::quantile(distance, .25, na.rm = TRUE),
        median = stats::median(distance, na.rm = TRUE),
        p750 = stats::quantile(distance, .75, na.rm = TRUE),
        p975 = stats::quantile(distance, .975, na.rm = TRUE),
        min = min(distance, na.rm = TRUE),
        max = max(distance, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(providers, metric, rank, urc_cat) %>%
    dplyr::select(
        providers,
        urc_code,
        urc_cat,
        metric,
        rank,
        n_tracts,
        n_counties,
        n_missing,
        pop,
        mean,
        pop_weighted_mean,
        dplyr::everything()
    )

## Summary tables ----
readr::write_csv(county_summary,
                 "./output/raw_tableS01_county_summary.csv")
readr::write_csv(urc_summary,
                 "./output/raw_tableS02_urban_rural_summary_by_rank.csv")

print_urc <- urc_summary %>%
    dplyr::filter(rank == 1, metric == "driving_time") %>%
    dplyr::transmute(
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

readr::write_csv(print_urc,
                 "./output/table1_summary.csv")
