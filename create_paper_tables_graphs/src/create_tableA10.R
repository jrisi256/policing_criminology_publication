library(here)
library(readr)
library(dplyr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data.
################################################################################
read_dir <- here("create_variables", "output")
write_dir <- here("create_paper_tables_graphs", "output")

individual_shift <-
    read_csv(here(read_dir, "2_individual_shift_vars_final.csv.gz"))

arrests_df <- individual_shift %>% filter(!is.na(arrests_n))
force_df <- individual_shift %>% filter(!is.na(force_n))

################################################################################
# Estimate models where the dependent variable is arrests + force.
################################################################################
individual_arrests <-
    fepois(
        arrests_black ~
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
        data = arrests_df
    )

individual_force <-
    fepois(
        force_black ~
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
        data = force_df
    )

################################################################################
# Save model results.
################################################################################
offset_row <-
    tibble(
        term = c("", "FE - Day of the Week", "FE - Month-Year", "FE - Shift Timing", "FE - Beat"),
        `Model 1` = c("Poisson Regression", "X", "X", "X", "X"),
        `Model 2` = c("Poisson Regression", "X", "X", "X", "X")
    )

attr(offset_row, "position") <- c(1, 13, 14, 15, 15)

modelsummary(
    list(
        "Arrests of Black civilians" = individual_arrests,
        "Uses of force against Black civilians" = individual_force
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
    fmt = 3,
    stars = T,
    output = file.path(write_dir, "tableA10_individual_arrest_force.txt"),
    add_rows = offset_row,
    coef_omit = ".theta",
    gof_omit = "FE:|RMSE|AIC|R2 Within$",
    notes =
        c(
            "Standard Errors in parentheses.",
            "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"
        )
)
