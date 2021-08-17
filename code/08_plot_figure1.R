## Imports ----
library(tidyverse)
library(here)
library(ggsci)
source(here::here("code", "utils.R"))
source(here::here("code", "mk_nytimes.R"))

fs::dir_create("./output")

## Data ----
tract_df <-
    dplyr::bind_rows(readRDS(here::here("data", "analytic_data_tract_all.RDS")),
                     readRDS(here::here("data", "analytic_data_tract_bupe.RDS")),
                     readRDS(here::here("data", "analytic_data_tract_otp.RDS"))) %>%
    dplyr::rename(pop = pop_total) %>%
    dplyr::mutate(provider_cat = factor(
        providers,
        levels = c("All", "Bupe", "OTP"),
        labels = c(
            "All\nProviders",
            "Buprenorphine\nPractitioner",
            "Opioid Treatment\nProgram"
        ),
        ordered = TRUE
    ))

## Expand by URC ----
## We want to expand by URC code so we can make tables that include all or
## urban/rural as well as each individual code.
super_tract <- expand_by_urc_codes(tract_df)

## Summarize tracts by urban/rural code ----
urban_rural_tract <- super_tract %>%
    dplyr::group_by(providers, provider_cat, urc_code, urc_cat, rank, metric) %>%
    dplyr::summarize(
        mean = mean(distance, na.rm = TRUE),
        median = stats::median(distance, na.rm = TRUE),
        p25 = stats::quantile(distance, .25, na.rm = TRUE),
        p75 = stats::quantile(distance, .75, na.rm = TRUE),
        w_mean = w_mean(distance, pop),
        w_median = w_median(distance, pop)
    ) %>%
    dplyr::ungroup()

## Ok, this plot is trickier than it seems. We want to facet the urc codes but
## we want the aggregated ones on the left and individual ones after and we
## want the colors to be consistent. To do this we need to create new factor
## variables.

urban_rural_tract <- urban_rural_tract %>%
    dplyr::mutate(
        urban_rural_big = dplyr::case_when(
            urc_code %in% c(1:4, 1234) ~ "urban",
            urc_code %in% c(5:6, 56) ~ "rural",
            urc_code == 99 ~ "all",
            TRUE ~ NA_character_
        ),
        urc_code_big = dplyr::case_when(urc_code %in% c(56, 1234) ~ 0,
                                        TRUE ~ urc_code)
    ) %>%
    dplyr::mutate(
        urban_rural_big_cat = factor(
            urban_rural_big,
            levels = c("all", "urban", "rural"),
            labels = c("All", "Urban", "Rural"),
            ordered = TRUE
        ),
        urc_code_big_cat = factor(
            urc_code_big,
            levels = c(0:6, 99),
            labels = c(
                "All Urban/\nAll Rural",
                "Large central\nmetroplitan (1)",
                "Large fringe\nmetropolitan (2)",
                "Medium\nmetropolitan (3)",
                "Small\nmetropolitan (4)",
                "Micropolitan (5)",
                "Non-core (6)",
                ordered = TRUE
            )
        )
    )

p1 <- ggplot2::ggplot(
    urban_rural_tract %>%
        dplyr::filter(
            urc_code != 99,
            metric == "driving_time",
            providers != "All",
            rank <= 15
        ),
    ggplot2::aes(
        x = rank,
        y = median,
        ymax = p75,
        ymin = p25,
        fill = urban_rural_big_cat,
        color = urban_rural_big_cat,
        group = urc_code
    )
) +
    ggplot2::geom_ribbon(alpha = .35, color = NA) +
    ggplot2::geom_point(color = "white", size = 1.5) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1) +
    mk_nytimes(legend.position = "none",
               axis.text.x = element_text(hjust = c(.5, .5, .5, .9))) +
    ggsci::scale_fill_jama(name = "Urban/Rural (NCHS Code)") +
    ggsci::scale_color_jama(name = "Urban/Rural (NCHS Code)") +
    ggplot2::scale_x_continuous("Provider, ordered by increasing distance",
                                expand = c(0, .3),
                                breaks = c(1, 5, 10, 15)) +
    ggplot2::scale_y_continuous("Median (IQR) driving time, minutes",
                                expand = c(0, .1)) +
    ggplot2::facet_grid(provider_cat ~ urc_code_big_cat, scales = "free")

ggplot2::ggsave(
    "./output/figure1.pdf",
    p1,
    width = 7,
    height = 4,
    device = grDevices::cairo_pdf,
    scale = 1.1
)

ggplot2::ggsave(
    "./output/figure1.jpg",
    p1,
    width = 7,
    height = 4,
    dpi = 300,
    scale = 1.1
)

## Save data for the plot
readr::write_csv(
    urban_rural_tract %>%
        dplyr::filter(
            urc_code != 99,
            metric == "driving_time",
            providers != "All",
            rank <= 15
        ) %>%
        dplyr::select(urc_cat, provider_cat, rank, median, p25, p75),
    "./output/figure1_data.csv"
)
