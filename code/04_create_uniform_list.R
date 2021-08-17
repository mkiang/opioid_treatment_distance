## Imports ----
library(here)
library(tidyverse)
source(here::here("code", "utils.R"))

## Data ----
bupe_df <- readRDS(here::here("data", "cleaned_bupe_prov_list.RDS"))
otp_df <- readRDS(here::here("data", "cleaned_otp_list.RDS"))
query_list <- readRDS(here::here("data", "gmap_queried_objects_cache.RDS"))

## Extract queries ----
## Formatted addresses
query_list$formatted_address <-
    unlist(lapply(query_list$result_obj, extract_address))

## Location type
query_list$location_type <-
    unlist(lapply(query_list$result_obj, extract_location_type))

## Latitude / longitude
query_list$latitude <-
    unlist(lapply(query_list$result_obj, extract_location_lat))
query_list$longitude <-
    unlist(lapply(query_list$result_obj, extract_location_lon))

## Query status
query_list$status_returned <-
    unlist(lapply(query_list$result_obj, extract_query_status))

## Make a single uniform dataframe ----
## Need to fix some columns from OTP data and Bupe Provider data to make
## a single analytic dataframe.
cleaned_df <- dplyr::bind_rows(
    bupe_df %>%
        dplyr::mutate(name = paste(title, first, last, deg)) %>%
        dplyr::mutate(name = gsub("\\<NA ", "", name)) %>%
        dplyr::mutate(name = gsub(" NA\\>", "", name)) %>%
        dplyr::mutate(address_original = paste(address_original, address_complement)) %>% 
        dplyr::mutate(type = "Buprenorphine Provider") %>%
        dplyr::select(
            download, 
            type,
            name,
            gmap_query,
            address,
            address_original,
            city,
            state,
            postal_code,
            phone = tele
        ),
    otp_df %>%
        dplyr::rename(state_abbrev = state) %>%
        dplyr::left_join(dplyr::tibble(
            state_abbrev = datasets::state.abb, state = datasets::state.name
        )) %>%
        dplyr::mutate(type = paste("OTP", certification)) %>%
        dplyr::select(
            download, 
            type,
            name = program_name,
            gmap_query,
            address,
            address_original,
            city,
            state,
            postal_code,
            phone,
            full_certification
        )
)

cleaned_df <- cleaned_df %>%
    dplyr::left_join(
        query_list %>%
            dplyr::select(
                gmap_query,
                formatted_address,
                location_type,
                latitude,
                longitude,
                status_returned
            ),
        by = "gmap_query"
    ) %>%
    dplyr::select(
        download, 
        type,
        name,
        formatted_address,
        address_original,
        location_type,
        latitude,
        longitude,
        status_returned,
        dplyr::everything()
    ) %>%
    dplyr::arrange(state, city, postal_code)

## Save ----
readr::write_csv(cleaned_df, here::here("data", "cleaned_provider_list.csv"))
