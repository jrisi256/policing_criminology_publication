library(here)
library(dplyr)
library(readr)
library(purrr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data.
################################################################################
read_dir <- here("create_variables", "output")
write_dir <- here("create_paper_tables_graphs", "output")
police_unit <- read_csv(file.path(read_dir, "1_police_district_vars_final.csv"))

################################################################################
# Estimate regression models across different levels of poverty.
################################################################################
police_unit_list_poverty_tercile <-
    police_unit %>%
    distinct(unit, prcnt_poverty) %>%
    mutate(poverty_percentile = as.numeric(ntile(prcnt_poverty, 3))) %>%
    select(-prcnt_poverty) %>%
    full_join(police_unit, by = "unit", multiple = "all") %>%
    group_split(poverty_percentile)

names_poverty_tercile <-
    unlist(
        map(
            police_unit_list_poverty_tercile,
            function(d) {d %>% pull(poverty_percentile) %>% unique()}
        )
    )

names_pvrty_tercile[names_pvrty_tercile == 1] <- "Low Poverty"
names_pvrty_tercile[names_pvrty_tercile == 2] <- "Medium Poverty"
names_pvrty_tercile[names_pvrty_tercile == 3] <- "High Poverty"
names(police_unit_list_poverty_tercile) <- names_poverty_tercile

regression_groups_poverty_ratio_tercile <-
    map(
        police_unit_list_poverty_tercile,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          black_ratio +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          nr_officers +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        }
    )

# Not in paper. Estimate using percent black rather than racial congruence.
regression_groups_poverty_prcnt_tercile <-
    map(
        police_unit_list_poverty_tercile,
        function(df) {
            fixest_cluster_nb_full <-
                femlm(black_stops ~
                          prcnt_officer_black +
                          mean_years_worked_unit +
                          violent_cr_capita +
                          property_cr_capita +
                          nr_officers +
                          offset(log(black)) | unit + year,
                      family = "negbin",
                      data = df)
        }
    )

################################################################################
# Write out results.
################################################################################
offset_row_tercile <-
    tibble(
        term = c("Black Stops", "Offset - Black Pop."),
        `Model 1` = c("", "Yes"),
        `Model 2` = c("", "Yes"),
        `Model 3` = c("", "Yes")
    )

attr(offset_row_tercile, "position") <- c(1, 10)

modelsummary(
    regression_groups_poverty_ratio_tercile,
    coef_omit = "(Intercept)|theta",
    coef_rename =
        c(
            black_ratio = "Black Racial Congruence",
            mean_years_worked_unit = "Years Worked In Unit (Mean)",
            prcnt_officer_black = "Percentage of Officers Who Are Black",
            violent_cr_capita = "Violent Crime Per 10,000",
            property_cr_capita = "Property Crime Per 10,000",
            log_total_officers = "Log of the Total Number of Officers",
            nr_officers = "Total number of officers"
        ),
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    fmt = 3,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(write_dir, "tableA6_poverty_terciles_ratio.txt"),
    add_rows = offset_row_tercile,
    notes =
        c(
            "Standard Errors in parentheses. Coefficients are incident rate ratios.",
            "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)

# Not in paper. Estimate using percent black rather than racial congruence.
modelsummary(
    regression_groups_poverty_prcnt_tercile,
    coef_omit = "(Intercept)|theta",
    coef_rename =
        c(
            black_ratio = "Black Racial Congruence",
            mean_years_worked_unit = "Years Worked In Unit (Mean)",
            prcnt_officer_black = "Percentage of Officers Who Are Black",
            violent_cr_capita = "Violent Crime Per 10,000",
            property_cr_capita = "Property Crime Per 10,000",
            log_total_officers = "Log of the Total Number of Officers",
            nr_officers = "Total number of officers"
        ),
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    fmt = 3,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(write_dir, "table_poverty_terciles_prcnt.txt"),
    add_rows = offset_row_tercile,
    notes =
        c(
            "Standard Errors in parentheses. Coefficients are incident rate ratios.",
            "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
        )
)
