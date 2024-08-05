library(here)
library(dplyr)
library(readr)
library(fixest)
library(modelsummary)

################################################################################
# Read in police-unit data.
################################################################################
read_dir <- here("create_variables", "output")
write_dir <- here("create_paper_tables_graphs", "output")
police_unit <- read_csv(here(read_dir, "1_police_district_vars_final.csv"))

################################################################################
# Estimate regression models where the dependent variable is arrests.
################################################################################
nb_prcnt_arrest <-
    femlm(
        black_arrests ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = police_unit
    )

nb_ratio_arrest <-
    femlm(
        black_arrests ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = police_unit
    )

nb_prcnt_arrest_stops_indep_var <-
    femlm(
        black_arrests ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            black_stops +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = police_unit
    )

nb_ratio_arrest_stops_indep_var <-
    femlm(
        black_arrests ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            black_stops +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = police_unit
    )

################################################################################
# Save results of arrest models.
################################################################################
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model E", "Yes"),
        `Model 2` = c("Model F", "Yes"),
        `Model 3` = c("Model G", "Yes"),
        `Model 4` = c("Model H", "Yes")
    )

attr(offset_row, "position") <- c(1, 9)

f <- function(x) format(x, digits = 3, nsmall = 1, scientific = F)

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
        nb_prcnt_arrest,
        "Arrests of Black Civilians" = nb_ratio_arrest,
        nb_prcnt_arrest_stops_indep_var,
        nb_ratio_arrest_stops_indep_var
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    fmt = f,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = here(write_dir, "tableA8_arrests.txt"),
    add_rows = offset_row,
    notes =
        c(
            "Standard Errors in parentheses. Coefficients are incident rate ratios.",
            "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)

################################################################################
# Estimate regression models where the dependent variable is use of force.
################################################################################
nb_prcnt_force <-
    femlm(
        black_force ~
              prcnt_officer_black +
              mean_years_worked_unit +
              violent_cr_capita +
              property_cr_capita +
              nr_officers +
              offset(log(black)) | unit + year,
          family = "negbin",
          data = police_unit
    )

nb_ratio_force <-
    femlm(
        black_force ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = police_unit
    )

nb_prcnt_force_stops_indep_var <-
    femlm(
        black_force ~
            prcnt_officer_black +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            black_stops +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = police_unit
    )

nb_ratio_force_stops_indep_var <-
    femlm(
        black_force ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            black_stops +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = police_unit
    )

################################################################################
# Save results of use of force models.
################################################################################
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model I", "Yes"),
        `Model 2` = c("Model J", "Yes"),
        `Model 3` = c("Model K", "Yes"),
        `Model 4` = c("Model L", "Yes")
    )

attr(offset_row, "position") <- c(1, 9)

modelsummary(
    list(
        nb_prcnt_force,
        "Uses of force against Black civilians" = nb_ratio_force,
        nb_prcnt_force_stops_indep_var,
        nb_ratio_force_stops_indep_var
    ),
    fmt = f,
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(write_dir, "tableA9_force.txt"),
    add_rows = offset_row,
    notes =
        c(
            "Standard Errors in parentheses. Coefficients are incident rate ratios.",
            "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)
