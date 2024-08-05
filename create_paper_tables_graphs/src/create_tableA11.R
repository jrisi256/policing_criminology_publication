library(here)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(fixest)
library(stringr)
library(modelsummary)

################################################################################
# Read in data.
################################################################################
read_path <- here("create_variables", "output")
write_path <- here("create_paper_tables_graphs", "output")
police_units <- read_csv(here(read_path, "1_police_district_vars_final.csv"))

################################################################################
# Estimate regression models for each type of stop.
################################################################################
police_unit_list <-
    police_units %>%
    pivot_longer(
        cols = matches("^stops$|black_stops|nr_stops"),
        names_to = "group",
        values_to = "dep_var"
    ) %>%
    group_split(group)

names <- map_chr(police_unit_list, function(df) {unique(df[["group"]])})
names(police_unit_list) <- names

police_unit_list <-
    pmap(
        list(police_unit_list, names(police_unit_list)),
        function(df, dep_var_name) {
            df %>%
                mutate(
                    dep_var_name = dep_var_name,
                    offset_var =
                        if_else(
                            str_detect(dep_var_name, "black"),
                            black,
                            total_pop
                        )
                )
        }
    )

regression_groups <-
    map(
        police_unit_list,
        function(df) {
            femlm(
                dep_var ~
                    black_ratio +
                    mean_years_worked_unit +
                    violent_cr_capita +
                    property_cr_capita +
                    nr_officers | unit + year,
                offset  = log(df$offset_var),
                family = "negbin",
                data = df
            )
        }
    )

offset_row <-
    tibble(
        term = c("", "Offset"),
        `Model 1` = c(names(regression_groups)[1], "Black Pop."),
        `Model 2` = c(names(regression_groups)[2], "Total Pop."),
        `Model 3` = c(names(regression_groups)[3], "Black Pop."),
        `Model 4` = c(names(regression_groups)[4], "Total Pop."),
        `Model 5` = c(names(regression_groups)[5], "Black Pop."),
        `Model 6` = c(names(regression_groups)[6], "Total Pop."),
        `Model 7` = c(names(regression_groups)[7], "Black Pop."),
        `Model 8` = c(names(regression_groups)[8], "Total Pop."),
        `Model 9` = c(names(regression_groups)[9], "Black Pop."),
        `Model 10` = c(names(regression_groups)[10], "Total Pop."),
        `Model 11` = c(names(regression_groups)[11], "Black Pop."),
        `Model 12` = c(names(regression_groups)[12], "Total Pop."),
    )

attr(offset_row, "position") <- c(1, 10)

modelsummary(
    regression_groups,
    coef_omit = "(Intercept)|theta",
    coef_rename =
        c(
            black_ratio = "Black Racial Congruence",
            mean_years_worked_unit = "Years Worked In Unit (Mean)",
            prcnt_officer_black = "Percentage of Officers Who Are Black",
            violent_cr_capita = "Violent Crime Per 10,000",
            property_cr_capita = "Property Crime Per 10,000",
            nr_officers = "Total Number of Officers"
        ),
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    fmt = 3,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(write_path, "tableA11_stop_types_aggregate.txt"),
    add_rows = offset_row,
    notes =
        c(
            "Standard Errors in parentheses. Coefficients are incident rate ratios.",
            "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)
