library(here)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(dtplyr)
library(R.utils)
library(stringr)

# .df: The data frame.
# .prefix: Character. Appended to beginning of each column to indicate outcome.
# .id_col: Column used for identifying if an outcome happened during a shift.
# ...: Column(s) to be used for creating outcomes. If left blank, the function
# will simply count the total number of instances of the outcome.
Create_Outcomes <- function(.df, .prefix, .id_col, ...) {
    # Sum up the total number of instances of the outcome.
    outcome <-
        .df %>%
        mutate(count = if_else(is.na({{ .id_col }}), 0, 1)) %>%
        lazy_dt() %>%
        count(assignment_id, officer_id, unit, month, ..., wt = count) %>%
        as_tibble()
    
    # If no other grouping variables were provided, skip this step.
    if(nargs() - 3 != 0) {
        # Since we have other grouping variables, turn each group into a column.
        outcome <-
            outcome %>%
            lazy_dt() %>%
            filter(if_any(c(...), ~ !is.na(.x))) %>%
            pivot_wider(
                id_cols = c(assignment_id, officer_id, unit, month),
                names_from = c(...),
                values_from = "n",
                values_fill = 0
            ) %>%
            as_tibble()
    }

    # Rename the columns so they are clean and have the appropriate outcome.
    outcome <-
        outcome %>%
        rename_with(
            ~ paste0(
                .prefix, "_", str_replace_all(tolower(.x), fixed(" "), "")
            ),
            -c(assignment_id, officer_id, unit, month)
        )

    return(outcome)
}

################################################################################
# Read in data.
################################################################################
stop_assignments <-
    read_csv(here("data_cleaning", "output", "2_stops_assignments.csv.gz"))

arrest_assignments <-
    read_csv(
        here("data_cleaning", "output", "2_arrests_assignments.csv.gz")
    ) %>%
    mutate(
        domestic =
            case_when(
                str_detect(statute_description, "DOMESTIC") &
                    crime_code == "violent" ~
                    "domestic",
                !str_detect(statute_description, "DOMESTIC") &
                    crime_code == "violent" ~
                    "non_domestic",
                T ~ "non_violent"
            )
    )

force_assignments <-
    read_csv(here("data_cleaning", "output", "2_force_assignments.csv.gz"))

################################################################################
# Create stop outcomes.
################################################################################
stop_count <- Create_Outcomes(stop_assignments, "stops", stop_id)
stop_type <- Create_Outcomes(stop_assignments, "stops", stop_id, contact_type)
stop_race <- Create_Outcomes(stop_assignments, "stops", stop_id, civ.race)
stop_type_race <-
    Create_Outcomes(stop_assignments, "stops", stop_id, contact_type, civ.race)

stop_outcomes <-
    reduce(
        list(stop_count, stop_type, stop_race, stop_type_race),
        full_join,
        by = c("assignment_id", "officer_id", "unit", "month")
    ) %>%
    mutate(
        across(
            -c(assignment_id, officer_id, unit, month),
            ~if_else(is.na(.x), 0, .x)
        )
    )

################################################################################
# Create arrest outcomes.
################################################################################
arrest_count <- Create_Outcomes(arrest_assignments, "arrests", arrest_id)
arrest_type <-
    Create_Outcomes(arrest_assignments, "arrests", arrest_id, crime_code)
arrest_type_domestic <-
    Create_Outcomes(arrest_assignments, "arrests", arrest_id, domestic)
arrest_race <-
    Create_Outcomes(arrest_assignments, "arrests", arrest_id, civ.race)

arrest_outcomes <-
    reduce(
        list(arrest_count, arrest_type, arrest_race, arrest_type_domestic),
        full_join,
        by = c("assignment_id", "officer_id", "unit", "month")
    ) %>%
    mutate(
        across(
            -c(assignment_id, officer_id, unit, month),
            ~if_else(is.na(.x), 0, .x)
        )
    )

################################################################################
# Create use of force outcomes.
################################################################################
force_count <- Create_Outcomes(force_assignments, "force", force_id)
force_type <-
    Create_Outcomes(force_assignments, "force_injured", force_id, civ.injured)
force_race <-
    Create_Outcomes(force_assignments, "force", force_id, civ.race)

force_outcomes <-
    reduce(
        list(force_count, force_type, force_race),
        full_join,
        by = c("assignment_id", "officer_id", "unit", "month")
    ) %>%
    mutate(
        across(
            -c(assignment_id, officer_id, unit, month),
            ~if_else(is.na(.x), 0, .x)
        )
    )

outcomes <-
    reduce(
        list(stop_outcomes, arrest_outcomes, force_outcomes),
        full_join,
        by = c("assignment_id", "officer_id", "unit", "month")
    )

write_csv(outcomes, here("data_cleaning", "output", "3_outcomes.csv"))
gzip(here("data_cleaning", "output", "3_outcomes.csv"), overwrite = T)
