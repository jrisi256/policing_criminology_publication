library(here)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data.
################################################################################
read_dir <- here("create_variables", "output")
write_dir <- here("create_paper_tables_graphs", "output")

individual_shift <-
    read_csv(here(read_dir, "2_individual_shift_vars_final.csv.gz")) %>%
    select(
        stops_black, officer_black, officer_hisp, officer_female, years_exp,
        years_exp_sq, mult_officer, n_officer_black, n_officer_white,
        n_officer_hisp, beat_assigned, `Month-Year`, weekday, shift,
        `Police Unit`, arrests_black, force_black, `Individual Officer`
    )

police_unit <-
    read_csv(here(read_dir, "1_police_district_vars_final.csv")) %>%
    select(
        unit, month, year, black_ratio, prcnt_officer_black,
        prcnt_civ_black, black, prcnt_poverty
    ) %>%
    distinct(unit, .keep_all = T) %>%
    mutate(
        officer_black_ntile = ntile(prcnt_officer_black, 3),
        poverty_ntile = ntile(prcnt_poverty, 3),
        black_ratio_ntile = ntile(black_ratio, 3)
    ) %>%
    select(officer_black_ntile, poverty_ntile, black_ratio_ntile, unit) %>%
    pivot_longer(
        cols = matches("ntile"), names_to = "variable", values_to = "values"
    ) %>%
    unite("variable", c("variable", "values"))

################################################################################
# Split by percent of unit that is black and poverty,
################################################################################
individual_shift_tercile <-
    individual_shift %>%
    inner_join(
        police_unit, by = c("Police Unit" = "unit"),
        relationship = "many-to-many",
    ) %>%
    group_split(variable)

names <-
    unlist(
        map(
            individual_shift_tercile,
            function(df) {df %>% pull(variable) %>% unique()}
        )
    )

names[names == "black_ratio_ntile_1"] <- "Low Racial Congruence"
names[names == "black_ratio_ntile_2"] <- "Medium Racial Congruence"
names[names == "black_ratio_ntile_3"] <- "High Racial Congruence"
names[names == "poverty_ntile_1"] <- "Low poverty"
names[names == "poverty_ntile_2"] <- "Medium poverty"
names[names == "poverty_ntile_3"] <- "High poverty"
names[names == "officer_black_ntile_1"] <- "Low % of Black Officers"
names[names == "officer_black_ntile_2"] <- "Medium % of Black Officers"
names[names == "officer_black_ntile_3"] <- "High % of Black Officers"
names(individual_shift_tercile) <- names

################################################################################
# Estimate models for stops.
################################################################################
regression_group_stops <-
    map(
        individual_shift_tercile,
        function(df) {
            fenegbin(
                stops_black ~
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
                data = df
            )
        }
    )

offset_row_stops <-
    tibble(
        term =
            c(
                "Black Stops (Negative Binomial Regression)",
                "FE - Day of the Week",
                "FE - Month-Year",
                "FE - Shift Timing",
                "FE - Beat"
            ),
        `Model 1` = c("", "X", "X", "X", "X"),
        `Model 2` = c("", "X", "X", "X", "X"),
        `Model 3` = c("", "X", "X", "X", "X"),
        `Model 4` = c("", "X", "X", "X", "X"),
        `Model 5` = c("", "X", "X", "X", "X"),
        `Model 6` = c("", "X", "X", "X", "X"),
        `Model 7` = c("", "X", "X", "X", "X"),
        `Model 8` = c("", "X", "X", "X", "X"),
        `Model 9` = c("", "X", "X", "X", "X")
    )

attr(offset_row_stops, "position") <- c(1, 13, 14, 15, 16, 17, 18, 19, 20, 21)

modelsummary(
    regression_group_stops,
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
    output = file.path(write_dir, "table5_A7_individual_stops_terciles.txt"),
    add_rows = offset_row_stops,
    fmt = 4,
    coef_omit = ".theta",
    gof_omit = "FE:|RMSE|AIC|R2 Within$",
    notes =
        c(
            "Standard Errors in parentheses.",
            "P-values are denoted by symbols: + p: 0.1, * p: 0.05, ** p: 0.01, *** p: 0.001"
        )
)

################################################################################
# Are the coefficients statistically significantly different across models?
# Table A5
################################################################################
low_black_model <- regression_group_stops[["Low % of Black Officers"]]
low_black_officer_black_coef <- low_black_model$coefficients[["officer_black"]]
low_black_officer_black_se <- low_black_model$se[["officer_black"]]
low_black_nr_black_officer_coef <- low_black_model$coefficients[["n_officer_black"]]
low_black_nr_black_officer_se <- low_black_model$se[["n_officer_black"]]

med_black_model <- regression_group_stops[["Medium % of Black Officers"]]
med_black_officer_black_coef <- med_black_model$coefficients[["officer_black"]]
med_black_officer_black_se <- med_black_model$se[["officer_black"]]
med_black_nr_black_officer_coef <- med_black_model$coefficients[["n_officer_black"]]
med_black_nr_black_officer_se <- med_black_model$se[["n_officer_black"]]

high_black_model <- regression_group_stops[["High % of Black Officers"]]
high_black_officer_black_coef <- high_black_model$coefficients[["officer_black"]]
high_black_officer_black_se <- high_black_model$se[["officer_black"]]
high_black_nr_black_officer_coef <- high_black_model$coefficients[["n_officer_black"]]
high_black_nr_black_officer_se <- high_black_model$se[["n_officer_black"]]

compare_coefficients <- function(c1, c2, se1, se2) {
    z_statistic <- (c1 - c2) / sqrt(se1 ^ 2 + se2 ^ 2)
    p_value <- 2 * pnorm(abs(z_statistic), lower = F)
    return(list(z_statistic = z_statistic, p_value = p_value))
}

coefficient_comparison_test <-
    pmap(
        list(
            c1 = 
                list(
                    low_black_nr_black_officer_coef,
                    low_black_nr_black_officer_coef,
                    med_black_nr_black_officer_coef,
                    low_black_officer_black_coef,
                    low_black_officer_black_coef,
                    med_black_officer_black_coef
                ),
            c2 =
                list(
                    med_black_nr_black_officer_coef,
                    high_black_nr_black_officer_coef,
                    high_black_nr_black_officer_coef,
                    med_black_officer_black_coef,
                    high_black_officer_black_coef,
                    high_black_officer_black_coef
                ),
            se1 = 
                list(
                    low_black_nr_black_officer_se,
                    low_black_nr_black_officer_se,
                    med_black_nr_black_officer_se,
                    low_black_officer_black_se,
                    low_black_officer_black_se,
                    med_black_officer_black_se
                ),
            se2 = list(
                med_black_nr_black_officer_se,
                high_black_nr_black_officer_se,
                high_black_nr_black_officer_se,
                med_black_officer_black_se,
                high_black_officer_black_se,
                high_black_officer_black_se
            )
        ),
        compare_coefficients
    )
