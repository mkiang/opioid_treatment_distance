## Imports ----
library(tidyverse)
library(patchwork)
library(here)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

## Create folder ----
fs::dir_create(here("output"))

## Data ----
county_df <-
    readRDS(here::here("data", "sensitivity_data_county_all.RDS"))

## Constants ----
MIN_TIME <- 5 # truncate below this many minutes
MAX_TIME <- 120 # truncate above this many minutes
MIN_DIST <- 20 # kilometers (12.5 miles)
MAX_DIST <- 160 #~100 miles

## Get abs/rel diff from closest provider ----
county_df <- county_df %>%
    group_by(providers,
             county_name,
             urc_code,
             fips,
             type,
             metric) %>%
    mutate(absolute_diff = median - min(median)) %>%
    mutate(percent_diff = absolute_diff / min(median) * 100) %>%
    ungroup() %>%
    mutate(
        trunc_abs_diff = case_when(
            metric == "driving_time" & absolute_diff < MIN_TIME ~ MIN_TIME,
            metric == "driving_time" &
                absolute_diff > MAX_TIME ~ MAX_TIME,
            metric == "geodesic_km" &
                absolute_diff < MIN_DIST ~ MIN_DIST,
            metric == "geodesic_km" &
                absolute_diff > MAX_DIST ~ MAX_DIST,
            metric == "road_km" &
                absolute_diff < MIN_DIST ~ MIN_DIST,
            metric == "road_km" &
                absolute_diff > MAX_DIST ~ MAX_DIST,
            TRUE ~ absolute_diff
        ),
        trunc_perc_diff = case_when(
            metric == "driving_time" & percent_diff < MIN_TIME ~ MIN_TIME,
            metric == "driving_time" &
                percent_diff > MAX_TIME ~ MAX_TIME,
            metric == "geodesic_km" &
                percent_diff < MIN_DIST ~ MIN_DIST,
            metric == "geodesic_km" &
                percent_diff > MAX_DIST ~ MAX_DIST,
            metric == "road_km" &
                percent_diff < MIN_DIST ~ MIN_DIST,
            metric == "road_km" &
                percent_diff > MAX_DIST ~ MAX_DIST,
            TRUE ~ percent_diff
        )
    ) %>%
    mutate(provider_cat = factor(
        providers,
        levels = c("All", "Bupe", "OTP"),
        labels = c(
            "All\nProviders",
            "Buprenorphine\nPractitioner",
            "Opioid Treatment\nProgram"
        ),
        ordered = TRUE
    ))

## Plots ----
fig1a <- plot_counties(
    county_df %>%
        filter(
            providers == "All",
            !is.na(absolute_diff),
            new_rank == 5,
            type == "distance",
            metric == "driving_time"
        ),
    fill_var = "trunc_abs_diff"
) +
    theme(legend.position = "bottom",
          strip.background = element_rect(fill = NA)) +
    scale_fill_viridis_c(
        "Median increase in driving time, minutes",
        option = "D",
        trans = "log1p",
        direction = -1,
        na.value = "white",
        breaks = c(5, 15, 30, 60, 120),
        labels = c("<5", 15, 30, 60, ">120"),
        guide = guide_colorbar(
            barwidth = unit(8.5, "cm"),
            barheight = unit(.5, "cm"),
            title.position = "top"
        )
    ) +
    labs(title = "All providers")

fig1b <- plot_counties(
    county_df %>%
        filter(
            providers == "Bupe",
            !is.na(absolute_diff),
            new_rank == 5,
            type == "distance",
            metric == "driving_time"
        ),
    fill_var = "trunc_abs_diff"
) +
    theme(legend.position = "bottom",
          strip.background = element_rect(fill = NA)) +
    scale_fill_viridis_c(
        "Median increase in driving time, minutes",
        option = "D",
        trans = "log1p",
        direction = -1,
        na.value = "white",
        breaks = c(5, 15, 30, 60, 120),
        labels = c("<5", 15, 30, 60, ">120"),
        guide = guide_colorbar(
            barwidth = unit(12.5, "cm"),
            barheight = unit(.5, "cm"),
            title.position = "top"
        )
    ) +
    labs(title = "Buprenorphine providers")

fig1c <- plot_counties(
    county_df %>%
        filter(
            providers == "OTP",
            !is.na(absolute_diff),
            new_rank == 5,
            type == "distance",
            metric == "driving_time"
        ),
    fill_var = "trunc_abs_diff"
) +
    theme(legend.position = "bottom",
          strip.background = element_rect(fill = NA)) +
    scale_fill_viridis_c(
        "Median increase in driving time, minutes",
        option = "D",
        trans = "log1p",
        direction = -1,
        na.value = "white",
        breaks = c(5, 15, 30, 60, 120),
        labels = c("<5", 15, 30, 60, ">120"),
        guide = guide_colorbar(
            barwidth = unit(12.5, "cm"),
            barheight = unit(.5, "cm"),
            title.position = "top"
        )
    ) +
    labs(title = "Opioid Treatment Programs")

fig1 <- fig1b + fig1c + 
    plot_layout(ncol = 2, guides = "collect") &
    theme(legend.position = 'bottom')

ggsave(
    "./output/sensitivity_figure2.pdf",
    fig1,
    width = 8,
    height = 4,
    device = cairo_pdf,
    scale = 1
)

ggsave(
    "./output/sensitivity_figure2.jpg",
    fig1,
    width = 8,
    height = 4,
    dpi = 300,
    scale = 1
)
