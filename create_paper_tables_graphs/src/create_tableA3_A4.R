library(here)
library(dplyr)
library(readr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data
################################################################################
read_dir <- here("create_variables", "output")
write_dir <- here("create_paper_tables_graphs", "output")
police_unit <- read_csv(file.path(read_dir, "1_police_district_vars_final.csv"))

unit_level_model_vars <-
    police_unit %>%
    select(
        year, month, unit, black_stops, prcnt_officer_black, black_ratio, black,
        violent_cr_capita, property_cr_capita, log_total_officers, nr_officers,
        mean_years_worked_unit, black_arrests, black_force, black_stop_rate
    ) %>%
    group_by(unit) %>%
    arrange(unit, year, month) %>%
    mutate(
        property_cr_capita_lag_1m = lag(property_cr_capita),
        violent_cr_capita_lag_1m = lag(violent_cr_capita),
        black_ratio_lag_1m = lag(black_ratio),
        prcnt_officer_black_lag_1m = lag(prcnt_officer_black),
        mean_years_worked_unit_lag_1m = lag(mean_years_worked_unit)
    ) %>%
    ungroup()

################################################################################
# Estimate models with month fixed effects and 1-month lags (using % black).
################################################################################
nb_prcnt_exp <-
    femlm(
        black_stops ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_prcnt_exp_month <-
    femlm(
        black_stops ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year + month,
        family = "negbin",
        cluster = c("unit", "year", "month"),
        data = unit_level_model_vars
    )

nb_prcnt_exp_lag <-
    femlm(
        black_stops ~
            prcnt_officer_black_lag_1m +
            mean_years_worked_unit +
            violent_cr_capita_lag_1m +
            property_cr_capita_lag_1m +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

################################################################################
# Estimate models w/ month FEs and 1-month lags (using racial congruence).
################################################################################
nb_ratio_exp <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

nb_ratio_exp_month <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year + month,
        cluster = c("unit", "year", "month"),
        family = "negbin",
        data = unit_level_model_vars
    )

nb_ratio_exp_lag <-
    femlm(
        black_stops ~
            black_ratio_lag_1m +
            mean_years_worked_unit +
            violent_cr_capita_lag_1m +
            property_cr_capita_lag_1m +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = unit_level_model_vars
    )

################################################################################
# Save lagged models.
################################################################################
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model 3 from Table 2", "Yes"),
        `Model 2` = c("Model 3 (1 month lags)", "Yes"),
        `Model 3` = c("Model 4 from Table 2", "Yes"),
        `Model 4` = c("Model 4 (1 month lags)", "Yes")
    )

attr(offset_row, "position") <- c(1, 13)

rename =
    c(
        black_ratio = "Black Racial Congruence",
        black_ratio_lag_1m = "Black Racial Congruence (1 month lag)",
        prcnt_officer_black = "Percentage of Officers Who Are Black",
        prcnt_officer_black_lag_1m = "Percentage of Officers Who Are Black (1 month lag)",
        mean_years_worked_unit = "Years Worked In Unit (Mean)",
        mean_years_worked_unit_lag_1m = "Years Worked In Unit (Mean) (1 month lag)",
        property_cr_capita = "Property Crime Per 10,000",
        property_cr_capita_lag_1m = "Property Crime Per 10,000 (1 month lag)",
        violent_cr_capita = "Violent Crime Per 10,000",
        violent_cr_capita_lag_1m = "Violent Crime Per 10,000 (1 month lag)",
        log_total_officers = "Log of the Total Number of Officers",
        black_stops = "Number of stops of Black civilians",
        nr_officers = "Total number of officers"
    )

modelsummary(
    list(
        nb_ratio_exp,
        "Stops of Black Civilians" = nb_ratio_exp_lag,
        nb_prcnt_exp,
        nb_prcnt_exp_lag
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(write_dir, "tableA4_stops_aggregate_lag.txt"),
    add_rows = offset_row,
    notes =
        c(
            "Standard Errors in parentheses. Coefficients are incident rate ratios.",
            "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)

################################################################################
# Save models with month fixed effects.
################################################################################
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model 3 from Table 2", "Yes"),
        `Model 2` = c("Model 3 (Month)", "Yes"),
        `Model 3` = c("Model 4 from Table 2", "Yes"),
        `Model 4` = c("Model 4 (Month)", "Yes")
    )

attr(offset_row, "position") <- c(1, 9)

rename =
    c(
        black_ratio = "Black Racial Congruence",
        black_ratio_lag_1m = "Black Racial Congruence (1 month lag)",
        prcnt_officer_black = "Percentage of Officers Who Are Black",
        prcnt_officer_black_lag_1m = "Percentage of Officers Who Are Black (1 month lag)",
        mean_years_worked_unit = "Years Worked In Unit (Mean)",
        mean_years_worked_unit_lag_1m = "Years Worked In Unit (Mean) (1 month lag)",
        property_cr_capita = "Property Crime Per 10,000",
        property_cr_capita_lag_1m = "Property Crime Per 10,000 (1 month lag)",
        violent_cr_capita = "Violent Crime Per 10,000",
        violent_cr_capita_lag_1m = "Violent Crime Per 10,000 (1 month lag)",
        log_total_officers = "Log of the Total Number of Officers",
        black_stops = "Number of stops of Black civilians",
        nr_officers = "Total number of officers"
    )

modelsummary(
    list(
        nb_ratio_exp,
        "Stops of Black Civilians" = nb_ratio_exp_month,
        nb_prcnt_exp,
        nb_prcnt_exp_month
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(write_dir, "tableA3_stops_aggregate_monthFE.txt"),
    add_rows = offset_row,
    notes =
        c(
            "Standard Errors in parentheses. Coefficients are incident rate ratios.",
            "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)
