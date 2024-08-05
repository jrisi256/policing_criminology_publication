library(here)
library(readr)
library(dplyr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data.
################################################################################
read_path <- here("create_variables", "output")
write_path <- here("create_paper_tables_graphs", "output")
police_units <- read_csv(here(read_path, "1_police_district_vars_final.csv"))

################################################################################
# Estimate models for different racial groups.
################################################################################
nb_ratio_exp_all <-
    femlm(
        stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(total_pop)) | unit + year,
        family = "negbin",
        data = police_units
    )

nb_ratio_exp_white <-
    femlm(
        white_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(white)) | unit + year,
        family = "negbin",
        data = police_units
    )

nb_ratio_exp_hisp <-
    femlm(
        hispanic_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(hispanic)) | unit + year,
        family = "negbin",
        data = police_units
    )

nb_ratio_exp_black <-
    femlm(
        black_stops ~
            black_ratio +
            mean_years_worked_unit +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = police_units
    )

################################################################################
# Save results of models.
################################################################################
offset_row <-
    tibble(
        term = c("", "Offset"),
        `Model 1` = c("Model 1", "Total"),
        `Model 2` = c("Model 2", "Black"),
        `Model 3` = c("Model 3", "White"),
        `Model 4` = c("Model 4", "Hispanic")
    )

attr(offset_row, "position") <- c(1, 9)

rename =
    c(
        black_ratio = "Black Racial Congruence",
        prcnt_officer_black = "Percentage of Officers Who Are Black",
        mean_years_worked_unit = "Years Worked In Unit (Mean)",
        violent_cr_capita = "Violent Crime Per 10,000",
        property_cr_capita = "Property Crime Per 10,000",
        log_total_officers = "Log of the Total Number of Officers",
        nr_officers = "Total number of officers"
    )

modelsummary(
    list(
        "Stops of All Civilians" = nb_ratio_exp_all,
        "Stops of Black Civilians" = nb_ratio_exp_black,
        "Stops of White Civilians" = nb_ratio_exp_white,
        "Stops of Hispanic Civilians" = nb_ratio_exp_hisp
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(write_path, "tableA12_stops_aggregate_all_races.txt"),
    add_rows = offset_row,
    notes =
        c(
            "Standard Errors in parentheses. Coefficients are incident rate ratios.",
            "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)
