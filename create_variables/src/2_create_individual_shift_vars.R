library(here)
library(dplyr)
library(tidyr)
library(readr)
library(R.utils)

################################################################################
# Read in data.
################################################################################
read_dir <- here("data_cleaning", "output")
shift_assignments <-
    read_csv(file.path(read_dir, "1_officer_assignments.csv.gz"))
outcomes <- read_csv(file.path(read_dir, "3_outcomes.csv.gz"))

################################################################################
# Clean data
################################################################################
# Create shift-level variable: racial diversity of officers working the shift
racial_diversity_shift_beat <-
    shift_assignments %>%
    count(unit, beat_assigned, date, shift, officer_race) %>%
    group_by(unit, beat_assigned, date, shift) %>%
    mutate(prcnt = n / sum(n)) %>%
    pivot_wider(
        id_cols = c("unit", "beat_assigned", "shift", "date"),
        names_from = "officer_race",
        values_from = c("n", "prcnt"),
        values_fill = 0
    ) %>%
    ungroup() %>%
    mutate(n_officer = n_officer_black + n_officer_white + n_officer_hisp)

# Merge racial diversity of shifts with other independent variables
full_data_beats <-
    racial_diversity_shift_beat %>%
    full_join(shift_assignments, multiple = "all") %>%
    full_join(outcomes)

# Create dummy variables
full_data_beats <-
    full_data_beats %>%
    mutate(
        dummy = 1,
        officer_gender =
            case_when(
                officer_gender == "MALE" ~ "officer_male",
                officer_gender == "FEMALE" ~ "officer_female"
            ),
        spanish =
            case_when(
                spanish == T ~ "officer_spanish",
                spanish == F ~ "officer_english"
            )
    ) %>%
    pivot_longer(
        cols = c("officer_race", "officer_gender", "spanish"),
        names_to = "column",
        values_to = "value"
    ) %>%
    select(-column) %>%
    pivot_wider(names_from = "value", values_from = dummy, values_fill = 0)

# Filter out observations missing experience, create shift diversity variables
full_data_beats <-
    full_data_beats %>%
    filter(!is.na(months_from_start)) %>%
    mutate(
        mult_officer = if_else(n_officer > 1, 1, 0),
        n_officer_black =
            if_else(officer_black == 1, n_officer_black - 1, n_officer_black),
        n_officer_hisp =
            if_else(officer_hisp == 1, n_officer_hisp - 1, n_officer_hisp),
        n_officer_white =
            if_else(officer_white == 1, n_officer_white - 1, n_officer_white)
    ) %>%
    rename(
        `Police Unit` = unit,
        `Month-Year` = month,
        `Individual Officer` = officer_id
    ) %>%
    mutate(
        years_exp = months_from_start / 12,
        years_exp_sq = years_exp ^ 2,
        stops_black_bin = if_else(stops_black == 0, 0, 1),
        arrests_black_bin = if_else(arrests_black == 0, 0, 1),
        force_black_bin = if_else(force_black == 0, 0, 1),
        `Police Unit` = as.character(`Police Unit`)
    )

################################################################################
# Save data
################################################################################
write_csv(
    full_data_beats,
    here("create_variables", "output", "2_individual_shift_vars_final.csv")
)
gzip(
    here("create_variables", "output", "2_individual_shift_vars_final.csv"),
    overwrite = T
)
