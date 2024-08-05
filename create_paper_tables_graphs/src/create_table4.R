library(here)
library(readr)
library(dplyr)
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
    )

stops_df <- individual_shift %>% filter(!is.na(stops_n))
white_officer_black_stops_df <- stops_df %>% filter(officer_white == 1)
black_officer_black_stops_df <- stops_df %>% filter(officer_black == 1)

################################################################################
# Estimate models.
################################################################################
stops_all_officers <-
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

white_officer_black_stops <-
    fenegbin(
        stops_black ~
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp| beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = white_officer_black_stops_df
    )

black_officer_black_stops <-
    fenegbin(
        stops_black ~
            officer_female +
            years_exp +
            years_exp_sq +
            mult_officer +
            n_officer_black +
            n_officer_white +
            n_officer_hisp | beat_assigned ^ `Month-Year` ^ weekday ^ shift,
        cluster = ~ `Police Unit` + `Individual Officer` + `Month-Year`,
        data = black_officer_black_stops_df
    )

################################################################################
# Save results.
################################################################################
offset_row <-
    tibble(term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
           `Model 1` = c("", "X", "X", "X", "X"),
           `Model 2` = c("", "X", "X", "X", "X"),
           `Model 3` = c("", "X", "X", "X", "X"))
attr(offset_row, "position") <- c(1, 14, 15, 16, 17)

modelsummary(
    list(
        "All Officers" = stops_all_officers,
        "Black officers" = black_officer_black_stops,
        "White officers" = white_officer_black_stops),
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
    stars = T,
    output =
        here(
            "create_paper_tables_graphs",
            "output",
            "table4_individual_shift.txt"
        ),
    add_rows = offset_row,
    coef_omit = ".theta",
    fmt = 4,
    gof_omit = "FE:|RMSE|AIC|R2 Within$",
    notes =
        c(
            "Standard Errors in parentheses.",
            "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"
        )
    )
