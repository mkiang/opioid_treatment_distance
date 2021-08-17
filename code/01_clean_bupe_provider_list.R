## 01_clean_bupe_provider_list.R ----
##
## Take in the raw SAMHSA buprenorphine provider list and clean it up so that
## we can make reasonable Google Map queries to get a geocode.

## Imports ----
library(here)
library(fs)
library(tidyverse)
library(janitor)

## Clean up and munge the bupe provider list ----
old_prov <- readr::read_csv(here::here("data_raw", "Physician_Locator_2020-02-17T17-59-58.csv")) %>%
    janitor::clean_names() %>%
    dplyr::rename(title = x1) %>%
    dplyr::mutate(download = "2020-02-17")

new_prov <- purrr::map_df(
    .x = fs::dir_ls("./data_raw/Physician_Locator_by_state/", glob = "*.csv"),
    .f = ~ readr::read_csv(.x,
                           col_types = readr::cols(.default = readr::col_character())) %>%
        janitor::clean_names()
) %>%
    dplyr::rename(title = name_prefix,
                  tele = telephone) %>%
    dplyr::mutate(download = "2020-07-04")

N_BUPE_RAW <- NROW(new_prov)

## Make unique identifier for each row
bupe_provs <- dplyr::bind_rows(old_prov, new_prov) %>%
    dplyr::mutate(row_id = 1:dplyr::n())

## Drop duplicates
bupe_provs <- bupe_provs %>%
    dplyr::distinct(
        title,
        first,
        last,
        address,
        city,
        county,
        state,
        postal_code,
        tele,
        fax,
        download,
        .keep_all = TRUE
    )

N_BUPE_DROP_DUPES <- bupe_provs %>%
    dplyr::filter(download == max(download)) %>%
    NROW()

## Drop territories
bupe_provs <- bupe_provs %>%
    dplyr::filter(
        state != "Virgin Islands",
        state != "Puerto Rico",
        state != "Guam",
        state != "Northern Mariana Islands"
    )

N_BUPE_DROP_TERR <- bupe_provs %>%
    dplyr::filter(download == max(download)) %>%
    NROW()

## Drop PO Boxes
bupe_provs <- bupe_provs %>%
    dplyr::filter(
        substr(address, 1, 4) != "PO B",
        substr(address, 1, 6) != "PO box",
        substr(address, 1, 6) != "Po Box",
        substr(address, 1, 5) != "POBox",
        substr(address, 1, 4) != "P.O.",
        substr(address, 1, 5) != "P O B",
        substr(address, 1, 6) != "P. O. "
    )

N_BUPE_DROP_POBOXES <- bupe_provs %>%
    dplyr::filter(download == max(download)) %>%
    NROW()

## Split out addresses from building names
## Note that sometimes (e.g., `row_id == 3`) an address will have a building
## name in front of it. Let's remove that by dropping all non-numeric
## characters up until the first digit.
bupe_provs <- bupe_provs %>%
    dplyr::rename(address_original = address) %>%
    dplyr::mutate(address = sub("^\\D*(\\d)", "\\1", address_original))

## Manually fix some addresses
bupe_provs <- bupe_provs %>%
    dplyr::mutate(
        address = dplyr::case_when(
            grepl("\\<California Men", address_original) ~ "Colony Dr",
            address == "8101" ~ "California Mens Colony",
            TRUE ~ address
        ),
        postal_code = dplyr::case_when(
            grepl("\\<California Men", address_original) ~ "93409",
            TRUE ~ postal_code
        )
    )

## Add in spaces
## In the csv, it appears they removed line breaks so some addresses show up
## as 21900 Burbank BlvdSuite 300 instead of 21900 Burbank Blvd Suite 300. Add
## in spaces.
bupe_provs <- bupe_provs %>%
    ## Replace "Unit"
    dplyr::mutate(address = gsub("(\\S)([Uu]{1}nit)", "\\1 \\2", address)) %>%
    ## Replace "Suite"
    dplyr::mutate(address = gsub("(\\S)([Ss]{1}uite)", "\\1 \\2", address)) %>%
    ## Replace STE
    dplyr::mutate(address = gsub("(\\S)(STE|ste)", "\\1 \\2", address))

## Create a query column with cleaned info
bupe_provs <- bupe_provs %>%
    dplyr::mutate(gmap_query = paste(address, city, state, postal_code))

## Replace all hashes with "Suite"
## There's a bug in `ggmap` where if the query has a hash (`#`), it will
## return an error (see https://github.com/dkahle/ggmap/issues/290). We
## replace all of them with "Suite" because it seems Google Maps is
## smart enough to adjust Suite appropriately.
bupe_provs$gmap_query <-
    gsub("\\s#", " Suite ", bupe_provs$gmap_query)
bupe_provs$gmap_query <- gsub("#", " Suite ", bupe_provs$gmap_query)

saveRDS(bupe_provs, here::here("data", "cleaned_bupe_prov_list.RDS"))

N_BUPE_GMAP_QUERIES <- bupe_provs %>%
    dplyr::filter(download == max(download)) %>%
    dplyr::pull(gmap_query) %>%
    dplyr::n_distinct()

## Sample size flow chart
cat(
    sprintf(
        paste(
            "Raw files: %s.\n",
            "After removing duplicates: %s (%s).\n",
            "After removing territories: %s (%s).\n",
            "After removing PO Boxes: %s (%s).\n",
            "Unique Google Map Queries: %s (%s).\n"
        ),
        N_BUPE_RAW,
        N_BUPE_DROP_DUPES,
        N_BUPE_RAW - N_BUPE_DROP_DUPES,
        N_BUPE_DROP_TERR,
        N_BUPE_DROP_DUPES - N_BUPE_DROP_TERR,
        N_BUPE_DROP_POBOXES,
        N_BUPE_DROP_TERR - N_BUPE_DROP_POBOXES,
        N_BUPE_GMAP_QUERIES,
        N_BUPE_DROP_POBOXES - N_BUPE_GMAP_QUERIES
    )
)
