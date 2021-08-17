## 02_clean_otp_list.R ----
##
## Take in the raw SAMHSA opioid treatment provider list and clean it up so that
## we can make reasonable Google Map queries to get a geocode.

## Imports ----
library(here)
library(tidyverse)
library(janitor)

## Clean up and munge the bupe provider list ----
old_otp <- readr::read_csv(here::here("data_raw", "TreatmentProgram_20200217.csv")) %>%
    janitor::clean_names() %>%
    dplyr::mutate(download = "2020-02-17")

new_otp <-  readr::read_csv(here::here("data_raw", "TreatmentProgram_20200704.csv"))  %>%
    janitor::clean_names() %>%
    dplyr::mutate(download = "2020-07-04")

N_OTP_RAW <- NROW(new_otp)

otp_prov <- dplyr::bind_rows(old_otp, new_otp)

## Make unique identifier for each row
otp_prov <- otp_prov %>%
    dplyr::mutate(row_id = 1:dplyr::n())

## Drop duplicates (if any)
otp_prov <- otp_prov %>%
    dplyr::distinct(street,
                    city,
                    state,
                    zipcode,
                    phone,
                    download,
                    .keep_all = TRUE)

N_OTP_DROP_DUPES <- otp_prov %>%
    dplyr::filter(download == max(download)) %>%
    NROW()

## Drop territories
otp_prov <- otp_prov %>%
    dplyr::filter(
        state != "Virgin Islands",
        state != "Puerto Rico",
        state != "Guam",
        state != "Northern Mariana Islands"
    )

N_OTP_DROP_TERR <- otp_prov %>%
    dplyr::filter(download == max(download)) %>%
    NROW()

## Split out addresses from building names
## Note that sometimes (e.g., `row_id == 3`) an address will have a building
## name in front of it. Let's remove that by dropping all non-numeric
## characters up until the first digit.
otp_prov <- otp_prov %>%
    dplyr::rename(address_original = street,
                  postal_code = zipcode) %>%
    dplyr::mutate(address = sub("^\\D*(\\d)", "\\1", address_original))

## Drop PO Boxes
otp_prov <- otp_prov  %>%
    dplyr::filter(
        substr(address_original, 1, 4) != "PO B",
        substr(address_original, 1, 6) != "PO box",
        substr(address_original, 1, 6) != "Po Box",
        substr(address_original, 1, 5) != "POBox",
        substr(address_original, 1, 4) != "P.O.",
        substr(address_original, 1, 5) != "P O B",
        substr(address_original, 1, 6) != "P. O. "
    )

N_OTP_DROP_POBOXES <- otp_prov %>%
    dplyr::filter(download == max(download)) %>%
    NROW()

## Manually fix some addresses
otp_prov <- otp_prov %>%
    dplyr::mutate(
        address = dplyr::case_when(
            address == "6-E, 5200 Eastern Ave" ~ "5200 Eastern Ave",
            address == "8-300 and 9300, 760 Broadway" ~ "760 Broadway",
            address == "347, Building #151" ~ "151 County Center Rd",
            address_original == "One Veterans Dr., Outpatient Methadone Progr. 116-A" ~ "1 Veterans Dr",
            address == "1001-11th St." ~ "1001 11th St.",
            address_original == "American Lake Dr., Bldg. 61" ~ "9600 Veterans Drive",
            TRUE ~ address
        )
    )

## Create a query column with cleaned info
otp_prov <- otp_prov %>%
    dplyr::mutate(gmap_query = paste(address, city, state, postal_code))

## Replace all hashes with "Suite"
## There's a bug in `ggmap` where if the query has a hash (`#`), it will
## return an error (see https://github.com/dkahle/ggmap/issues/290). We
## replace all of them with "Suite" because it seems Google Maps is
## smart enough to adjust Suite appropriately.
otp_prov$gmap_query <-
    gsub("\\s#", " Suite ", otp_prov$gmap_query)
otp_prov$gmap_query <-
    gsub("#", " Suite ", otp_prov$gmap_query)

saveRDS(otp_prov, here::here("data", "cleaned_otp_list.RDS"))

N_OTP_GMAP_QUERIES <- otp_prov %>%
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
        N_OTP_RAW,
        N_OTP_DROP_DUPES,
        N_OTP_RAW - N_OTP_DROP_DUPES,
        N_OTP_DROP_TERR,
        N_OTP_DROP_DUPES - N_OTP_DROP_TERR,
        N_OTP_DROP_POBOXES,
        N_OTP_DROP_TERR - N_OTP_DROP_POBOXES,
        N_OTP_GMAP_QUERIES,
        N_OTP_DROP_POBOXES - N_OTP_GMAP_QUERIES
    )
)
