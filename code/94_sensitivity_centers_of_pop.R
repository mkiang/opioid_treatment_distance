## 94_sensitvity_centers_of_pop.R ----
##
## Sensitivity analysis where we use centers of population rather than
## geographic center of census tracts. The data come from 2010 so things have
## likely shifted but we can still see how it would impact our results.

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

## Data ----
urban_rural_codes <-
    readxl::read_excel(here::here("data_raw", "NCHSURCodes2013.xlsx")) %>%
    janitor::clean_names(.) %>%
    dplyr::transmute(
        fips = sprintf("%05d", fips_code),
        county_name = county_name,
        urc_code = x2013_code
    )
acs_df <- readRDS(here::here("data", "acs_vars_wide.RDS"))

## Import all geocoded provider locations ----
provider_locs_orig <-
    readr::read_csv(here::here("data", "cleaned_provider_list.csv"))

## Remove bad google matches and duplicated lat/lon ----
provider_locs_x <- provider_locs_orig %>%
    dplyr::filter(location_type %in% c("RANGE_INTERPOLATED", "ROOFTOP"),
                  state != "Guam") %>%
    dplyr::select(download, type, formatted_address, latitude, longitude) %>%
    dplyr::distinct()

## Download 2010 US Census centers of population at tract level ----
if (!file_exists(here("data_raw", "uscb_2010_centers_of_population_tract.csv"))) {
    cop_tracts <- read_csv(
        paste0(
            "https://www2.census.gov/geo/docs/reference/",
            "cenpop2010/tract/CenPop2010_Mean_TR.txt"
        )
    )
    
    write_csv(cop_tracts,
              here("data_raw", "uscb_2010_centers_of_population_tract.csv"))
} else {
    cop_tracts <- read_csv(here("data_raw", "uscb_2010_centers_of_population_tract.csv"))
}

## Load it up and clean some names ----
centers_of_population <- cop_tracts %>% 
    transmute(STATEFP,
              COUNTYFP,
              TRACTCE, 
              GEOID = paste0(STATEFP, COUNTYFP, TRACTCE),
              POP_2010 = POPULATION,
              tract_lat_pop = LATITUDE,
              tract_lon_pop = LONGITUDE)

## Get road distances ----
## Loop through all states and find distances ----
for (d in DOWNLOADS) {
    provider_locs <- provider_locs_x %>%
        dplyr::filter(download == lubridate::ymd(d))
    
    for (s in c("DC", datasets::state.abb)) {
        dist_file <- here::here(
            "intermediate_objects",
            sprintf("top20_center_of_pop_distances_%s", gsub("-", "_", d)),
            sprintf("%s_top20_center_of_pop_distance.RDS", s)
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
                    geo_longitude = sp::coordinates(state_shp)[, 1],
                    geo_latitude = sp::coordinates(state_shp)[, 2]
                ) %>% 
                left_join(centers_of_population %>% 
                              rename(longitude = tract_lon_pop,
                                     latitude = tract_lat_pop))
            
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
distances_df <- purrr::map_df(
    .x = fs::dir_ls(
        here::here(
            "intermediate_objects",
            "top20_center_of_pop_distances_2020_07_04"
        ),
        glob = "*.RDS"
    ),
    .f = ~ readRDS(.x)
)

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

## Join with population and urban/rural codes ----
distances_df <- distances_df %>%
    dplyr::left_join(urban_rural_codes)

## Create a tract-specific dataframe ----
## Each row is a census tract with one of the top 20 nearest facilities
tract_df <- distances_df %>%
    dplyr::left_join(acs_df %>%
                         dplyr::rename(geoid = GEOID) %>%
                         dplyr::select(-NAME)) %>%
    dplyr::select(
        providers,
        fips,
        county_name,
        urc_code,
        st_name,
        metric,
        distance,
        rank,
        dplyr::everything()
    )

## Summarize into counties ----
## Going to collapse tracts into counties and summarize the mean, median, and
## sd using unweighted estimates, population-weights, and population multiplier.
## We are also interested in the actual distance, but we want to know the
## relative and absolute distances compared to the nearest facility. So type =
## {distance, absolute_difference, and relative_difference} provide different
## metric types.
county_df <- dplyr::bind_rows(
    tract_df %>%
        dplyr::group_by(
            providers,
            fips,
            county_name,
            urc_code,
            metric,
            rank,
            st_name,
            st_fips
        ) %>%
        dplyr::summarize(
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
        dplyr::mutate(type = "distance"),
    tract_df %>%
        dplyr::group_by(
            providers,
            fips,
            county_name,
            urc_code,
            metric,
            rank,
            st_name,
            st_fips
        ) %>%
        dplyr::summarize(
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
        dplyr::mutate(type = "relative_diff"),
    tract_df %>%
        dplyr::group_by(
            providers,
            fips,
            county_name,
            urc_code,
            metric,
            rank,
            st_name,
            st_fips
        ) %>%
        dplyr::summarize(
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
        dplyr::mutate(type = "absolute_diff")
)

county_df <- county_df %>%
    dplyr::ungroup() %>%
    dplyr::select(providers,
                  fips,
                  county_name,
                  urc_code,
                  type,
                  metric,
                  rank,
                  dplyr::everything())

## Save ---- 
saveRDS(
    tract_df %>%
        dplyr::filter(providers == "All"),
    here::here("data", "sensitivity_centers_of_pop_tract_all.RDS"),
    compress = "xz"
)
saveRDS(
    tract_df %>%
        dplyr::filter(providers == "Bupe"),
    here::here("data", "sensitivity_centers_of_pop_tract_bupe.RDS"),
    compress = "xz"
)
saveRDS(
    tract_df %>%
        dplyr::filter(providers == "OTP"),
    here::here("data", "sensitivity_centers_of_pop_tract_otp.RDS"),
    compress = "xz"
)
saveRDS(county_df,
        here::here("data", "sensitivity_centers_of_pop_analytic_data_county_all.RDS"),
        compress = "xz")
