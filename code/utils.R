## utils.R ----

## Imports ----
library(tidyverse)
library(osrm)
library(geosphere)
library(usmap)

## This helper function takes the centroid of a census tract (lonlat1) and a
## list of all possible provider lonlat and returns a subset of them (topn).
## We don't want to route every single pair of addresses so we'll just
## route the top 20 (or whatever) and then select the shortest from that
## list (assuming the closest by road distance is within the top 20 closest
## by geodesic distance --- which I think is a safe assumption).
return_topn_closest <-
    function(lonlat1, provider_lonlat_df, topn = 20) {
        provider_lonlat_df %>%
            dplyr::mutate(
                source_lon = lonlat1$longitude[1],
                source_lat = lonlat1$latitude[1],
                geodesic_dist = as.vector(geosphere::distm(provider_lonlat_df[, c("longitude", "latitude")],
                                                           lonlat1))
            ) %>%
            dplyr::arrange(geodesic_dist) %>%
            dplyr::slice(1:topn)
    }

## This takes the output of return_topn_closest() and calculates the topn
## routes returning a new dataframe with distance and duration.
calc_routes <- function(topn_df) {
    topn_df <- topn_df %>%
        dplyr::mutate(roadkm_dist = NA_real_,
                      timemin_dist = NA_real_)
    
    for (i in 1:NROW(topn_df)) {
        suppressMessages(suppressWarnings(
            x <- osrm::osrmRoute(
                src = topn_df[i , c("source_lon", "source_lat")],
                dst = topn_df[i , c("longitude", "latitude")],
                overview = FALSE
            )
        ))
        
        if (!is.null(x)) {
            topn_df$roadkm_dist[i] <- x["distance"]
            topn_df$timemin_dist[i] <- x["duration"]
        } else {
            topn_df$roadkm_dist[i] <- NA
            topn_df$timemin_dist[i] <- NA
        }
    }
    return(topn_df)
}

extract_address <- function(query_result_object) {
    x <- try(query_result_object[["results"]][[1]]$formatted_address,
             silent = TRUE)
    if (!is.null(x)) {
        return(x)
    } else {
        return(NA)
    }
}

extract_location_type <- function(query_result_object) {
    x <-
        try(query_result_object[["results"]][[1]]$geometry$location_type,
            silent = TRUE)
    if (!is.null(x)) {
        return(x)
    } else {
        return(NA)
    }
}

extract_location_lat <- function(query_result_object) {
    x <-
        try(query_result_object[["results"]][[1]]$geometry$location$lat,
            silent = TRUE)
    if (!is.null(x)) {
        return(x)
    } else {
        return(NA)
    }
}

extract_location_lon <- function(query_result_object) {
    x <-
        try(query_result_object[["results"]][[1]]$geometry$location$lng,
            silent = TRUE)
    if (!is.null(x)) {
        return(x)
    } else {
        return(NA)
    }
}

extract_query_status <- function(query_result_object) {
    x <- try(query_result_object[["status"]][[1]],
             silent = TRUE)
    if (!is.null(x)) {
        return(x)
    } else {
        return(NA)
    }
}

st_info <- tibble(
    abbrev   = state.abb,
    division = as.character(state.division),
    st_lat   = state.center$y,
    st_lon   = state.center$x
) %>%
    ## We have to add DC because it's not a state
    add_row(
        abbrev = "DC",
        division = "South Atlantic",
        st_lat = 38.9072,
        st_lon = -77.0369
    ) %>%
    left_join(narcan::st_fips_map) %>%
    rename(st_fips = fips)

plot_counties <-
    function(county_df, fill_var = "trunc_weighted_time") {
        plot_usmap(
            regions = "counties",
            data = county_df,
            values = fill_var,
            color = NA
        ) +
            geom_polygon(
                data = usmap::us_map(regions = "states"),
                aes(x = x, y = y, group = group),
                color = "black",
                size = .3,
                fill = NA
            )
    }

return_nearest_facility <-
    function(state_shpdf, ix, provider_locs, topn) {
        top20geo <- return_topn_closest(state_shpdf[ix, c("longitude", "latitude")],
                                        provider_locs,
                                        topn = topn)
        
        top20road <- calc_routes(top20geo)
        
        ## Take the minimums (I slice to break ties).
        min_road <- top20road %>%
            dplyr::filter(roadkm_dist == min(roadkm_dist, na.rm = TRUE)) %>%
            dplyr::arrange(roadkm_dist) %>%
            dplyr::slice(1)
        
        min_time <- top20road %>%
            dplyr::filter(timemin_dist == min(timemin_dist, na.rm = TRUE)) %>%
            dplyr::arrange(timemin_dist) %>%
            dplyr::slice(1)
        
        ## Return a dataframe with info we need
        if (NROW(min_road) == 0 & NROW(min_time) == 0) {
            state_shpdf[ix,] %>%
                mutate(
                    near_fac_road_address = NA,
                    near_fac_road_longitude = NA,
                    near_fac_road_latitude = NA,
                    near_fac_road_dist_km = NA,
                    near_fac_time_address = NA,
                    near_fac_time_longitude = NA,
                    near_fac_time_latitude = NA,
                    near_fac_time_dist_min = NA
                )
        } else {
            state_shpdf[ix,] %>%
                mutate(
                    near_fac_road_address = min_road$formatted_address,
                    near_fac_road_longitude = min_road$longitude,
                    near_fac_road_latitude = min_road$latitude,
                    near_fac_road_dist_km = min_road$roadkm_dist,
                    near_fac_time_address = min_time$formatted_address,
                    near_fac_time_longitude = min_time$longitude,
                    near_fac_time_latitude = min_time$latitude,
                    near_fac_time_dist_min = min_time$timemin_dist
                )
        }
    }

return_second_nearest_facility <-
    function(state_shpdf, ix, provider_locs, topn) {
        top20geo <- return_topn_closest(state_shpdf[ix, c("longitude", "latitude")],
                                        provider_locs,
                                        topn = topn)
        
        top20road <- calc_routes(top20geo)
        
        ## Take the minimums (I slice to break ties).
        min_road <- top20road %>%
            dplyr::arrange(roadkm_dist) %>%
            dplyr::slice(2)
        
        min_time <- top20road %>%
            dplyr::arrange(timemin_dist) %>%
            dplyr::slice(2)
        
        ## Return a dataframe with info we need
        if (NROW(min_road) == 0 & NROW(min_time) == 0) {
            state_shpdf[ix,] %>%
                mutate(
                    second_fac_road_address = NA,
                    second_fac_road_longitude = NA,
                    second_fac_road_latitude = NA,
                    second_fac_road_dist_km = NA,
                    second_fac_time_address = NA,
                    second_fac_time_longitude = NA,
                    second_fac_time_latitude = NA,
                    second_fac_time_dist_min = NA
                )
        } else {
            state_shpdf[ix,] %>%
                mutate(
                    second_fac_road_address = min_road$formatted_address,
                    second_fac_road_longitude = min_road$longitude,
                    second_fac_road_latitude = min_road$latitude,
                    second_fac_road_dist_km = min_road$roadkm_dist,
                    second_fac_time_address = min_time$formatted_address,
                    second_fac_time_longitude = min_time$longitude,
                    second_fac_time_latitude = min_time$latitude,
                    second_fac_time_dist_min = min_time$timemin_dist
                )
        }
    }

return_n_facility <-
    function(state_shpdf, ix, provider_locs, topn) {
        top20geo <- return_topn_closest(state_shpdf[ix, c("longitude", "latitude")],
                                        provider_locs,
                                        topn = topn)
        
        state_shpdf[ix, ] %>%
            select(STATEFP:INTPTLON,
                   tract_lat = latitude,
                   tract_lon = longitude) %>%
            bind_cols(calc_routes(top20geo))
    }

## Helpers ----
w_mean <- function(x, w) {
    stats::weighted.mean(x, w, na.rm = TRUE)
}

w_sd <- function(x, w) {
    matrixStats::weightedSd(x, w, na.rm = TRUE)
}

w_median <- function(x, w) {
    matrixStats::weightedMedian(x, w, na.rm = TRUE)
}

expand_by_urc_codes <- function(df) {
    bind_rows(
        df,
        df %>%
            mutate(urc_code = 99),
        df %>%
            filter(urc_code %in% 1:4) %>%
            mutate(urc_code = 1234),
        df %>%
            filter(urc_code %in% 5:6) %>%
            mutate(urc_code = 56)
    ) %>%
        mutate(urc_cat = factor(
            urc_code,
            levels = c(99, 1234, 1:4, 56, 5:6),
            labels = c(
                "All",
                "Urban (1-4)",
                "Large central metro (1)",
                "Large fringe metro (2)",
                "Medium metro (3)",
                "Small metro (4)",
                "Rural (5-6)",
                "Micropolitan (5)",
                "Non-core (6)"
            ),
            ordered = TRUE
        ))
}

summarize_county_column <- function(df, column) {
    df %>%
        filter(!is.na({
            {
                column
            }
        })) %>%
        group_by(providers, urc_cat, urc_code) %>%
        summarize(
            n_counties = n(),
            n_tracts = sum(n_tracts),
            pop = sum(pop),
            median = median({
                {
                    column
                }
            }),
            mean = mean({
                {
                    column
                }
            }),
            sd = sd({
                {
                    column
                }
            }),
            p025 = quantile({
                {
                    column
                }
            }, .025),
            p10 = quantile({
                {
                    column
                }
            }, .1),
            p25 = quantile({
                {
                    column
                }
            }, .25),
            p75 = quantile({
                {
                    column
                }
            }, .75),
            p90 = quantile({
                {
                    column
                }
            }, .9),
            p975 = quantile({
                {
                    column
                }
            }, .975),
            min = min({
                {
                    column
                }
            }),
            max = max({
                {
                    column
                }
            })
        ) %>%
        ungroup()
}

summarize_tract_column <- function(df, column) {
    super_tract %>%
        filter(!is.na({
            {
                column
            }
        })) %>%
        group_by(providers, urc_cat, urc_code) %>%
        summarize(
            n_counties = n_distinct(fips),
            n_tracts = n(),
            pop = sum(pop),
            median = median({
                {
                    column
                }
            }, na.rm = TRUE),
            mean = mean({
                {
                    column
                }
            }, na.rm = TRUE),
            sd = sd({
                {
                    column
                }
            }, na.rm = TRUE),
            p025 = quantile({
                {
                    column
                }
            }, .025, na.rm = TRUE),
            p10 = quantile({
                {
                    column
                }
            }, .1, na.rm = TRUE),
            p25 = quantile({
                {
                    column
                }
            }, .25, na.rm = TRUE),
            p75 = quantile({
                {
                    column
                }
            }, .75, na.rm = TRUE),
            p90 = quantile({
                {
                    column
                }
            }, .9, na.rm = TRUE),
            p975 = quantile({
                {
                    column
                }
            }, .975, na.rm = TRUE),
            min = min({
                {
                    column
                }
            }, na.rm = TRUE),
            max = max({
                {
                    column
                }
            }, na.rm = TRUE)
        ) %>%
        ungroup()
}

st_fips_map <- structure(
    ## Taken from narcan. See narcan::st_fips_map on github.com/mkiang/narcan
    list(
        name = c(
            "Alabama",
            "Alaska",
            "American Samoa",
            "Arizona",
            "Arkansas",
            "California",
            "Colorado",
            "Connecticut",
            "Delaware",
            "District of Columbia",
            "Florida",
            "Federated States of Micronesia",
            "Georgia",
            "Guam",
            "Hawaii",
            "Idaho",
            "Illinois",
            "Indiana",
            "Iowa",
            "Kansas",
            "Kentucky",
            "Louisiana",
            "Maine",
            "Marshall Islands",
            "Maryland",
            "Massachusetts",
            "Michigan",
            "Minnesota",
            "Mississippi",
            "Missouri",
            "Montana",
            "Nebraska",
            "Nevada",
            "New Hampshire",
            "New Jersey",
            "New Mexico",
            "New York",
            "North Carolina",
            "North Dakota",
            "Northern Mariana Islands",
            "Ohio",
            "Oklahoma",
            "Oregon",
            "Palau",
            "Pennsylvania",
            "Puerto Rico",
            "Rhode Island",
            "South Carolina",
            "South Dakota",
            "Tennessee",
            "Texas",
            "U.S. Minor Outlying Islands",
            "Utah",
            "Vermont",
            "Virginia",
            "Virgin Islands of the U.S.",
            "Washington",
            "West Virginia",
            "Wisconsin",
            "Wyoming"
        ),
        abbrev = c(
            "AL",
            "AK",
            "AS",
            "AZ",
            "AR",
            "CA",
            "CO",
            "CT",
            "DE",
            "DC",
            "FL",
            "FM",
            "GA",
            "GU",
            "HI",
            "ID",
            "IL",
            "IN",
            "IA",
            "KS",
            "KY",
            "LA",
            "ME",
            "MH",
            "MD",
            "MA",
            "MI",
            "MN",
            "MS",
            "MO",
            "MT",
            "NE",
            "NV",
            "NH",
            "NJ",
            "NM",
            "NY",
            "NC",
            "ND",
            "MP",
            "OH",
            "OK",
            "OR",
            "PW",
            "PA",
            "PR",
            "RI",
            "SC",
            "SD",
            "TN",
            "TX",
            "UM",
            "UT",
            "VT",
            "VA",
            "VI",
            "WA",
            "WV",
            "WI",
            "WY"
        ),
        fips = c(
            "01",
            "02",
            "60",
            "04",
            "05",
            "06",
            "08",
            "09",
            "10",
            "11",
            "12",
            "64",
            "13",
            "66",
            "15",
            "16",
            "17",
            "18",
            "19",
            "20",
            "21",
            "22",
            "23",
            "68",
            "24",
            "25",
            "26",
            "27",
            "28",
            "29",
            "30",
            "31",
            "32",
            "33",
            "34",
            "35",
            "36",
            "37",
            "38",
            "69",
            "39",
            "40",
            "41",
            "70",
            "42",
            "72",
            "44",
            "45",
            "46",
            "47",
            "48",
            "74",
            "49",
            "50",
            "51",
            "78",
            "53",
            "54",
            "55",
            "56"
        )
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA,
                  -60L),
    spec = structure(list(
        cols = list(
            name = structure(list(), class = c("collector_character",
                                               "collector")),
            abbrev = structure(list(), class = c("collector_character",
                                                 "collector")),
            fips = structure(list(), class = c("collector_character",
                                               "collector"))
        ),
        default = structure(list(), class = c("collector_guess",
                                              "collector"))
    ), class = "col_spec")
)