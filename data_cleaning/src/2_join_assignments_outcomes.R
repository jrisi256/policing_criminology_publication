library(here)
library(readr)
library(dplyr)
library(dtplyr)
library(R.utils)
library(lubridate)
library(data.table)

# Merges an outcome to shift assignments using the process from Ba et al. 2021.
# Returns the resulting joined data frame.
merge <- function(df1, df2, id_col) {
    id_col <- enquo(id_col)
    
    # Sort for merging.
    setkey(df1, officer_id, date)
    setkey(df2, officer_id, date)
    
    ## Step 1: match officer_id, match stop date to date of shift start.
    merged <- df2[df1]
    merged[, month := i.month]
    merged <- merged[, !"i.month", with = F]
    
    ## Drop if not during shift (keep those shifts with no stops).
    merged <-
        merged[
            is.na(hour) |
                data.table::between(hour, floor(start_time), ceiling(end_time)),
            ]
    
    ## Step 2: match officer_id, deal w/ outcomes past 12am on overnight shifts.
    ## Find overnight shifts, fix start and end time, increment date by one day.
    nextday <- df1[end_time > 24]
    nextday[, new_start_time := 0]
    nextday[, new_end_time := end_time - 24]
    nextday[, date_next := date]
    day(nextday$date_next) <- day(nextday$date_next) + 1
    
    ## Match officer_id and date to the new start date of the overnight shift.
    setkey(nextday, officer_id, date_next)
    merged_nextday <-
        df2[nextday, , on = c(officer_id = 'officer_id', date = 'date_next')]
    
    ## Drop if not during shift (keep those shifts with no stops).
    merged_nextday <-
        merged_nextday[
            is.na(hour) |
                data.table::between(
                    hour, floor(new_start_time), ceiling(new_end_time)
                ),
            ]
    
    ## Revert back to the normal date.
    drop_cols <- c("i.date", "new_end_time", "new_start_time", "i.month")
    merged_nextday[, `:=`(date = i.date, month = i.month)]
    merged_nextday <- merged_nextday[, !drop_cols, with = F]
    
    ## Merge same-day outcomes w/ next-day outcomes on overnight shifts.
    bind_rows(merged, merged_nextday) %>%
        lazy_dt() %>%
        distinct() %>%
        group_by(assignment_id) %>%
        filter((n() > 1 & !is.na(!!id_col)) | n() == 1) %>%
        ungroup() %>%
        as_tibble()
}

# Read in data.
officer_assignments <-
    read_csv(here("data_cleaning", "output", "1_officer_assignments.csv.gz"))

stops <-
    read_csv(here("data_cleaning", "input", "stops.csv.gz")) %>%
    mutate(stop_officer_id = as.character(row_number()))

arrests <-
    read_csv(here("data_cleaning", "input", "arrests.csv.gz")) %>%
    mutate(arrest_officer_id = as.character(row_number()))

force <- read_csv(here("data_cleaning", "input", "force.csv.gz"))

# Convert to data.table.
setDT(officer_assignments)
setDT(stops)
setDT(arrests)
setDT(force)

# Merge outcomes with officer assignments.
stops_merged <- merge(officer_assignments, stops, stop_officer_id)
arrests_merged <- merge(officer_assignments, arrests, arrest_officer_id)
force_merged <- merge(officer_assignments, force, force_id)

# Save results
write_csv(
    stops_merged,
    here("data_cleaning", "output", "2_stops_assignments.csv")
)
gzip(here("data_cleaning", "output", "2_stops_assignments.csv"), overwrite = T)

write_csv(
    arrests_merged,
    here("data_cleaning", "output", "2_arrests_assignments.csv")
)
gzip(
    here("data_cleaning", "output", "2_arrests_assignments.csv"), overwrite = T
)

write_csv(
    force_merged,
    here("data_cleaning", "output", "2_force_assignments.csv")
)
gzip(here("data_cleaning", "output", "2_force_assignments.csv"), overwrite = T)
