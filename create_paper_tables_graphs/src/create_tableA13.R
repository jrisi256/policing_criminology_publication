library(here)
library(dplyr)
library(readr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data.
################################################################################
individual_shift <-
    read_csv(
        here(
            "create_variables", "output", "2_individual_shift_vars_final.csv.gz"
        )
    ) %>%
    select(
        officer_black, officer_hisp, officer_female, stops_black, stops_n,
        stops_white, stops_hispanic, years_exp, years_exp_sq, mult_officer,
        n_officer_black, n_officer_hisp, n_officer_white, beat_assigned,
        `Month-Year`, weekday, shift, `Police Unit`, `Individual Officer`
    )

stops_df <- individual_shift %>% filter(!is.na(stops_n))
white_civ_stops <- individual_shift %>% filter(!is.na(stops_white))
hisp_civ_stops <- individual_shift %>% filter(!is.na(stops_hispanic))

################################################################################
# Estimate models.
################################################################################
black_civ_stops_model <-
    fenegbin(
        stops_black ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_hisp +
            n_officer_white | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df
    )

all_civ_stops_model <-
    fenegbin(
        stops_n ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = stops_df
    )

white_civ_stops_model <-
    fenegbin(
        stops_white ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = white_civ_stops
    )

hisp_civ_stops_model <-
    fenegbin(
        stops_hispanic ~
            officer_black +
            officer_hisp +
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = hisp_civ_stops
    )

offset_row <-
    tibble(
        term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
        `Model 1` = c("", "X", "X", "X", "X"),
        `Model 2` = c("", "X", "X", "X", "X"),
        `Model 3` = c("", "X", "X", "X", "X"),
        `Model 4` = c("", "X", "X", "X", "X")
    )

attr(offset_row, "position") <- c(1, 13, 14, 15, 15)

modelsummary(
    list(
        "Stops of All Civilians" = all_civ_stops_model,
        "Stops of Black Civilians" = black_civ_stops_model,
        "Stops of White Civilians" = white_civ_stops_model,
        "Stops of Hispanic Civilians" = hisp_civ_stops_model
    ),
    coef_rename =
        c(
            officer_black = "Officer Race/Ethnicity - Black",
            officer_hisp = "Officer Race/Ethniciy - Hispanic",
            officer_female = "Officer Sex - Female",
            years_exp = "Officer Experience (Years)",
            years_exp_sq = "Officer Experience Squared (Years)",
            mult_officer = "Multipe officers assigned to the shift?",
            n_officer_black = "Number of other Black officers on shift",
            n_officer_white = "Number of other White officers on shift",
            n_officer_hisp = "Number of other Hispanic officers on shift"
        ),
    estimate = "{estimate} ({std.error}){stars}",
    exponentiate = T,
    statistic = NULL,
    fmt = 4,
    stars = T,
    output =
        here(
            "create_paper_tables_graphs",
            "output",
            "tableA13_civ_race_individual.txt"
        ),
    add_rows = offset_row,
    coef_omit = ".theta",
    gof_omit = "FE:|RMSE|AIC|R2 Within$",
    notes =
        c(
            "Standard Errors in parentheses.",
            "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"
        )
)
