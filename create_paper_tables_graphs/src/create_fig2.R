library(here)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)

################################################################################
# Read in data.
################################################################################
write_dir <- here("create_paper_tables_graphs", "output")

shift_assignments <-
    read_csv(here("data_cleaning", "output", "1_officer_assignments.csv.gz"))

police_units <-
    read_csv(
        here("create_variables", "output", "1_police_district_vars_final.csv")
    )

################################################################################
# Create shift-level variable: racial diversity of officers working the shift.
################################################################################
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

################################################################################
# # Aggregate racial diversity to the unit-month (currently at beat-day).
################################################################################
racial_diversity_unit_month <-
    racial_diversity_shift_beat %>%
    filter(unit != 21 & unit != 23 & unit != 13) %>%
    mutate(month = month(date), year = year(date)) %>%
    filter(year != 2012 & year != 2016) %>%
    mutate(diverse_shift =
               if_else(n_officer_white >= 1 & n_officer_black >= 1, 1, 0)) %>%
    mutate(
        white_only =
            if_else(n_officer_white > 1 & n_officer_white == n_officer, 1, 0),
        black_majority =
            if_else(prcnt_officer_black >= 0.5 & n_officer_white >= 1, 1, 0)
    ) %>%
    group_by(unit, month, year) %>%
    summarise(
        nr_diverse_shifts = sum(diverse_shift),
        nr_white_only = sum(white_only),
        nr_majority_black = sum(black_majority),
        total_nr_shifts = n()
    ) %>%
    ungroup() %>%
    mutate(
        unit = as.character(unit),
        prcnt_diverse = nr_diverse_shifts / total_nr_shifts,
        prcnt_white_only = nr_white_only / total_nr_shifts,
        prcnt_majority_black = nr_majority_black / total_nr_shifts
    )

police_units_diversity <-
    police_units %>%
    mutate(unit = as.character(unit)) %>%
    full_join(racial_diversity_unit_month)

prcnt_diverse <-
    ggplot(
        police_units_diversity, aes(x = prcnt_officer_black, y = prcnt_diverse)
    ) +
    geom_point(aes(color = unit)) +
    theme_bw() +
    theme(text = element_text(size = 10, family = "Times")) +
    labs(color = "Police Unit") +
    xlab("Proportion of unit that is Black") +
    ylab(
        "Proportion of shifts featuring at least one Black officer and one White officer"
    )

prcnt_white_only <-
    ggplot(police_units_diversity, aes(x = black_ratio, y = prcnt_white_only)) +
    geom_point(aes(color = unit)) +
    theme_bw()

prcnt_majority_black <-
    ggplot(
        police_units_diversity, aes(x = black_ratio, y = prcnt_majority_black)
    ) +
    geom_point(aes(color = unit)) +
    theme_bw()

ggsave(
    filename = "Figure 2.pdf",
    plot = prcnt_diverse,
    path = write_dir,
    height = 8,
    width = 12
)

ggsave(
    filename = "Figure Percent Shifts White Only.png",
    plot = prcnt_white_only,
    path = write_dir
)

ggsave(
    filename = "Figure Percent Shifts Black Majority.png",
    plot = prcnt_majority_black,
    path = write_dir
)
