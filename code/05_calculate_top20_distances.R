## Imports ----
library(tidyverse)
library(here)
library(fs)
library(osrm)
library(rgdal)
library(foreach)
library(doParallel)
options(osrm.server = "http://0.0.0.0:5000/", osrm.profile = "driving")
source(here::here("code", "utils.R"))

## Constants ----
N_CORES <- 14 ## Remember that OSRM takes up some of your cores.
DOWNLOADS <- rev(c("2020-07-04"))

## Import all geocoded provider locations ----
provider_locs_orig <-
    readr::read_csv(here::here("data", "cleaned_provider_list.csv"))

## Remove bad google matches and duplicated lat/lon ----
provider_locs_x <- provider_locs_orig %>%
    dplyr::filter(location_type %in% c("RANGE_INTERPOLATED", "ROOFTOP"),
                  state != "Guam") %>%
    dplyr::select(download, type, formatted_address, latitude, longitude) %>%
    dplyr::distinct()

## Get distance ----
## Ok. This is highly inefficient, but we're going to just loop through every
## census tract, take the top 20 closest (by geodesic distance) facilities,
## route the top 20, then find the closest (by time and by road distance),
## then append it to the original dataframe.

## Loop through all states and find distances ----
for (d in DOWNLOADS) {
    provider_locs <- provider_locs_x %>%
        dplyr::filter(download == lubridate::ymd(d))
    
    for (s in c("DC", datasets::state.abb)) {
        dist_file <- here::here(
            "intermediate_objects",
            sprintf("top20_distances_%s", gsub("-", "_", d)),
            sprintf("%s_top20_distance.RDS", s)
        )
        
        if (!dir.exists(dirname(dist_file))) {
            dir.create(dirname(dist_file),
                       showWarnings = FALSE,
                       recursive = TRUE)
        }
        
        ## Check if we've already done this state
        if (!file.exists(dist_file)) {
            print(sprintf("%s: %s", Sys.time(), dist_file))
            
            ## Names and constants
            state_fips <- st_fips_map %>%
                dplyr::filter(abbrev == s) %>%
                dplyr::pull(fips)
            
            state_file <- sprintf("tl_2017_%s_tract", state_fips)
            
            ## Shapefile and dataframes
            if (!file.exists(sprintf("./data_raw/shp_files/%s/%s.shp", state_file,
                                     state_file))) {
                unzip(
                    zipfile = sprintf("./data_raw/shp_files/%s.zip", state_file),
                    exdir = sprintf("./data_raw/shp_files/%s/", state_file)
                )
                
                state_shp <-
                    rgdal::readOGR(sprintf("./data_raw/shp_files/%s", state_file),
                                   state_file)
                
                dir_delete(sprintf("./data_raw/shp_files/%s/", state_file))
            } else {
                state_shp <-
                    rgdal::readOGR(sprintf("./data_raw/shp_files/%s", state_file),
                                   state_file)
            }
            
            state_shpdf <- dplyr::as_tibble(state_shp) %>%
                dplyr::mutate(
                    longitude = sp::coordinates(state_shp)[, 1],
                    latitude = sp::coordinates(state_shp)[, 2]
                )
            
            ## Route all census tracts
            doParallel::registerDoParallel(cores = N_CORES)
            distance_df <- foreach::foreach(
                i = 1:NROW(state_shpdf),
                .inorder = FALSE,
                .combine = dplyr::bind_rows
            ) %dopar% {
                dplyr::bind_rows(
                    return_n_facility(
                        state_shpdf = state_shpdf,
                        ix = i,
                        provider_locs = provider_locs,
                        topn = 20
                    ) %>%
                        dplyr::mutate(providers = "All"),
                    return_n_facility(
                        state_shpdf = state_shpdf,
                        ix = i,
                        provider_locs = provider_locs %>%
                            dplyr::filter(grepl("Bupre", type)),
                        topn = 20
                    ) %>%
                        dplyr::mutate(providers = "Bupe"),
                    return_n_facility(
                        state_shpdf = state_shpdf,
                        ix = i,
                        provider_locs = provider_locs %>%
                            dplyr::filter(grepl("OTP", type)),
                        topn = 20
                    ) %>%
                        dplyr::mutate(providers = "OTP")
                )
            }
            doParallel::stopImplicitCluster()
            
            ## Save
            saveRDS(distance_df, dist_file, compress = "xz")
        } else {
            print(sprintf("Skipping: %s (%s)", dist_file, Sys.time()))
        }
    }
}

## Combine data ----
distances_df <- purrr::map_df(.x = fs::dir_ls(
    here::here("intermediate_objects", "top20_distances_2020_07_04"),
    glob = "*.RDS"
),
.f = ~ readRDS(.x))

distances_df <- distances_df %>%
    dplyr::mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
    dplyr::mutate(geodesic_dist = geodesic_dist / 1000) %>%
    dplyr::select(
        providers,
        st_fips = STATEFP,
        geoid = GEOID,
        fips,
        tract_lon,
        tract_lat,
        prov_address = formatted_address,
        prov_lon = longitude,
        prov_lat = latitude,
        geodesic_km = geodesic_dist,
        road_km = roadkm_dist,
        driving_time = timemin_dist
    ) %>%
    tidyr::gather(metric, distance, geodesic_km:driving_time) %>%
    dplyr::group_by(providers, geoid, metric) %>%
    dplyr::mutate(rank = dplyr::row_number(distance)) %>%
    dplyr::mutate(
        abs_diff_1 = distance - min(distance, na.rm = TRUE),
        rel_diff_1 = (distance - min(distance, na.rm = TRUE)) / min(distance, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

distances_df <- distances_df %>%
    dplyr::arrange(providers, geoid, metric, rank)

## Join with state information ----
distances_df <- distances_df %>%
    dplyr::left_join(st_info %>%
                         dplyr::rename(st_name = name))

## Save ----
## Because we want to share these data on github, we need the files to be
## <100 MB. Split by provider to get smaller files.
saveRDS(
    distances_df %>%
        dplyr::filter(providers == "All"),
    here::here("data", "tract_distances_all.RDS"),
    compress = "xz"
)
saveRDS(
    distances_df %>%
        dplyr::filter(providers == "Bupe"),
    here::here("data", "tract_distances_bupe.RDS"),
    compress = "xz"
)
saveRDS(
    distances_df %>%
        dplyr::filter(providers == "OTP"),
    here::here("data", "tract_distances_otp.RDS"),
    compress = "xz"
)
