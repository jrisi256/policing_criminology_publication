library(here)
library(readr)
library(dplyr)
library(tidyr)
library(fixest)
library(modelsummary)

################################################################################
# Read in data.
################################################################################
read_path <- here("create_variables", "output")
write_path <- here("create_paper_tables_graphs", "output")

police_units <- read_csv(here(read_path, "1_police_district_vars_final.csv"))

model_vars <-
    police_units %>%
    select(
        year, unit, black_stops, prcnt_officer_black, black_ratio, stops,
        violent_cr_capita, property_cr_capita, black, mean_years_worked_unit,
        nr_officers, white_stops, hispanic_stops, white, hispanic, total_pop
    )

################################################################################
# Estimate models
################################################################################
nb_prcnt <-
    femlm(
        black_stops ~
            prcnt_officer_black +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = model_vars
    )

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
        data = model_vars
    )

nb_ratio <-
    femlm(
        black_stops ~
            black_ratio +
            violent_cr_capita +
            property_cr_capita +
            nr_officers +
            offset(log(black)) | unit + year,
        family = "negbin",
        data = model_vars
    )

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
        data = model_vars
    )

################################################################################
# Save model results. (Table 2)
################################################################################
offset_row <-
    tibble(
        term = c("", "Offset - Black Pop."),
        `Model 1` = c("Model 1", "Yes"),
        `Model 2` = c("Model 2", "Yes"),
        `Model 3` = c("Model 3", "Yes"),
        `Model 4` = c("Model 4", "Yes")
    )
attr(offset_row, "position") <- c(1, 9)
rename =
    c(black_ratio = "Black Racial Congruence",
      prcnt_officer_black = "Percentage of Officers Who Are Black",
      mean_years_worked_unit = "Years Worked In Unit (Mean)",
      violent_cr_capita = "Violent Crime Per 10,000",
      property_cr_capita = "Property Crime Per 10,000",
      nr_officers = "Total number of officers"
    )

modelsummary(
    list(nb_ratio,
         "Stops of Black Civilians" = nb_prcnt,
         nb_ratio_exp,
         nb_prcnt_exp
    ),
    coef_omit = "(Intercept)|theta",
    coef_map = rename,
    estimate = "{estimate} ({std.error}){stars}",
    statistic = NULL,
    stars = T,
    gof_omit = "R2|RMSE",
    exponentiate = T,
    output = file.path(write_path, "table2_stops-aggregate.txt"),
    add_rows = offset_row,
    notes = c("Standard Errors in parentheses. Coefficients are incident rate ratios.",
              "P-values are denoted by symbols: + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
    )
)

################################################################################
# Create table 3 (predictions from racial congruence + experience model).
################################################################################
nr_officers_ratio <-
    police_units %>%
    select(unit, year, black_ratio, nr_officers_black) %>%
    group_by(unit, year) %>%
    mutate(rank = row_number(black_ratio)) %>%
    mutate(
        type =
            case_when(
                rank == min(rank) ~ "min",
                rank == max(rank) ~ "max",
                rank == 4 ~ "p25",
                rank == 6 ~ "med",
                rank == 9 ~ "p75",
                T ~ "drop")
        ) %>%
    ungroup() %>%
    filter(type != "drop") %>%
    select(-black_ratio, -rank)

new_data_ratio <-
    model_vars %>%
    group_by(unit, year) %>%
    summarise(
        mean_years_worked_unit = mean(mean_years_worked_unit),
        violent_cr_capita = mean(violent_cr_capita),
        property_cr_capita = mean(property_cr_capita),
        nr_officers = mean(nr_officers),
        black = mean(black),
        min = min(black_ratio),
        max = max(black_ratio),
        med = median(black_ratio),
        p25 = quantile(black_ratio)[["25%"]],
        p75  = quantile(black_ratio)[["75%"]]
    ) %>%
    pivot_longer(
        c("min", "max", "med", "p25", "p75"),
        names_to = "type",
        values_to = "black_ratio") %>%
    ungroup() %>%
    full_join(nr_officers_ratio, by = c("unit", "year", "type"))

predict_results_ratio <-
    new_data_ratio %>%
    mutate(
        predicted_black_stops =
            predict(nb_ratio_exp, newdata = new_data_ratio),
        type = factor(type, levels = c("min", "p25", "med", "p75", "max")),
        variable = "Black Racial Congruence"
    )

predict_results_diff <-
    predict_results_ratio %>%
    select(
        unit, year, type, black_ratio, nr_officers_black, predicted_black_stops
    ) %>%
    filter(type == "min" | type == "max") %>%
    pivot_wider(
        id_cols = c("unit", "year"),
        names_from = type,
        values_from =
            c("predicted_black_stops", "black_ratio", "nr_officers_black")
    ) %>%
    mutate(
        decrease_stops = predicted_black_stops_min - predicted_black_stops_max,
        chg_officer = nr_officers_black_max - nr_officers_black_min
    )

predict_table <-
    predict_results_diff %>%
    mutate(unit = as.numeric(unit)) %>%
    arrange(unit) %>%
    filter(year == 2014) %>%
    select(-year) %>%
    mutate(
        across(matches("stops"), ~round(.x)),
        across(matches("ratio"), ~round(.x, 2))
    ) %>%
    unite(
        "predicted_stops",
        predicted_black_stops_min:predicted_black_stops_max,
        sep = " to "
    ) %>%
    unite("black_ratio", black_ratio_min:black_ratio_max, sep = " to ") %>%
    unite(
        "nr_officers_black",
        nr_officers_black_min:nr_officers_black_max,
        sep = " to "
    )

write_csv(predict_table, here(write_path, "table3_aggregate_predictions.csv"))
