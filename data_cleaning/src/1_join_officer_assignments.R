library(here)
library(readr)
library(dplyr)
library(stringr)
library(R.utils)
library(lubridate)

# Read in police officer roster.
officers <- 
    read_csv(
        here("data_cleaning", "input", "officers.csv.gz"),
        col_types = 
            cols_only(
                birth_year = "d",
                appointed_month = col_date(format = "%Y-%m-%d"),
                officer_id = "c",
                officer_race = "c",
                officer_gender = "c",
                spanish = "l"
            )
    )

# Read in officer-work assignment data.
assignments <-
    read_csv(
        here("data_cleaning", "input", "assignments.csv.gz"),
        col_types = 
            cols_only(
                officer_id = "c",
                month = col_date(format = "%Y-%m-%d"),
                rank = "c",
                unit = "c",
                date = col_date(format = "%Y-%m-%d"),
                shift = "c",
                start_time = "d",
                end_time = "d",
                weekday = "c",
                beat_assigned = "c",
                months_from_start = "d",
                months_from_start_sq = "d",
                duration = "d"
            )
    ) %>%
    mutate(
        assignment_id = row_number()
    )

# Create start and end date times for each shift assignment.
assignments_clean <-
    assignments %>%
    mutate(
        start_date = if_else(is.na(start_time), NA_Date_, date),
        start_hms = hms::hms(
            rep(0, nrow(assignments)),
            rep(0, nrow(assignments)),
            start_time
        ),
        start_datetime = as_datetime(paste0(start_date, " ", start_hms)),
        end_date =
            case_when(
                is.na(end_time) ~ NA_Date_,
                end_time > 24 ~ date + 1,
                TRUE ~ date
            ),
        end_hms = hms::hms(
            rep(0, nrow(assignments)),
            rep(0, nrow(assignments)),
            case_when(
                is.na(end_time) ~ NA_real_,
                end_time > 24 ~ end_time - 24,
                TRUE ~ end_time
            )
        ),
        end_datetime = as_datetime(paste0(end_date, " ", end_hms))
    ) %>%
    select(-start_date, -start_hms, -end_date, -end_hms)

# Join officer roster to officer-work assignments.
officer_assignments <-
    inner_join(assignments_clean, officers, by = "officer_id")

# Final data set.
officer_assignments_final <-
    officer_assignments %>%
    filter(
        officer_race %in% c(paste0("officer_", c("white", "black", "hisp"))),
        rank == "POLICE OFFICER"
    )

# Save results
write_csv(
    officer_assignments_final,
    here("data_cleaning", "output", "1_officer_assignments.csv")
)
gzip(
    here("data_cleaning", "output", "1_officer_assignments.csv"), overwrite = T
)
