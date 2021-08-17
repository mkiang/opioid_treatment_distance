## 03_geocode_locations.R ----
##
## Given a list of cleaned addresses, we want to ping the Google Maps API to
## get a geocode (lat/lon).

## Imports ----
library(ggmap)
library(here)
library(tidyverse)
source(here::here("code", "secrets.R"))

## Data ----
bupe_df <- readRDS(here::here("data", "cleaned_bupe_prov_list.RDS"))
otp_df <- readRDS(here::here("data", "cleaned_otp_list.RDS"))
cached_raw <- here::here("data", "gmap_queried_objects_cache.RDS")

## Query Google Maps ----
## Querying Google Maps is a costly operation. There is a limit to the number
## of queries that can be done and each query above the limit costs money. In
## addition, there's a limit to the number of queries per minute. We're going
## to query each *unique* address and save the raw result object.
##
## Saving the raw results object will allow us to extract what we need (and
## come back to extract again later). After extraction, we will merge it with
## the original data, and then finish verifying by hand.
##
## We'll also save each set of queries and then only ping addresses with a
## missing results object.
query_list <- dplyr::bind_rows(bupe_df %>%
                            dplyr::select(gmap_query),
                        otp_df %>%
                            dplyr::select(gmap_query)) %>%
    dplyr::distinct()

## Check cached queries
## We just do a left join here and skip over non-NA and non-null lines later
## because we want to resave the new cache file. 
if (fs::file_exists(cached_raw)) {
    cached_queries <- readRDS(cached_raw) %>% 
        dplyr::distinct(gmap_query, .keep_all = TRUE) %>% 
        dplyr::select(gmap_query, result_obj)
    
    query_list <- query_list %>% 
        dplyr::left_join(cached_queries, by = "gmap_query")
}

## Ping the API ----
## Note that we need to set a timer or we'll get rate-limited. 
for (i in 1:NROW(query_list)) {
    ## Skip if we already have a results object for this
    if (is.na(query_list$result_obj[i]) | 
        is.null(query_list$result_obj[i][[1]])) {
        print(i)
        ## Set a sleep timer to stay below API limit
        Sys.sleep(1 / 10)
        temp_x <- ggmap::geocode(query_list$gmap_query[i], output = "all")
        query_list$result_obj[i] <- list(temp_x)
    }
}

## Save a cached version
saveRDS(query_list, cached_raw, compress = "xz")
