library(here)
library(readr)
library(dplyr)
library(modelsummary)

read_path <- here("create_variables", "output")
write_path <- here("create_paper_tables_graphs", "output")

police_units <- read_csv(here(read_path, "1_police_district_vars_final.csv"))

police_unit_descriptive_vars <-
    police_units %>%
    select(
        `Total Number of Stops` = stops,
        `Stops of Black Civilians` = black_stops,
        `Stops of Black civilians per 10,000` = black_stop_rate,
        `Stops of White civiilians per 10,000` = white_stop_rate,
        `Stops of Hispanic civilians per 10,000` = hispanic_stop_rate,
        `Total Number of Officers` = nr_officers,
        `Total Number of Black Officers` = nr_officers_black,
        `Percent of Officers - Black` = prcnt_officer_black,
        `Total Population` = total_pop,
        `Black Population` = black,
        `Percent of Population - Black` = prcnt_civ_black,
        `Black Racial Congruence` = black_ratio,
        `Years Worked in Unit (Mean)` = mean_years_worked_unit,
        `Violent Crime Per 10,000 Individuals` = violent_cr_capita,
        `Property Crime Per 10,000 Individuals` = property_cr_capita
    )

datasummary(
    All(police_unit_descriptive_vars) ~ Mean + SD + Min + P25 + Median + P75 + Max,
    fmt = 3,
    data = police_unit_descriptive_vars,
    output = here(write_path, "table1_descriptive-table.txt")
)
