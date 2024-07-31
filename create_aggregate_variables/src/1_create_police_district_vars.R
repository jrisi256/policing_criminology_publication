library(sf)
library(here)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(dtplyr)
library(stringr)
library(lubridate)

################################################################################
# Read in monthly police unit membership data and police district shape data.
################################################################################
districts_shape <-
    st_read(
        dsn =
            here(
                "create_aggregate_variables", "input", "police_district_shapes"
            ),
        layer = "geo_export_427fc6e8-19e2-4435-b9e4-0566a20fa39c"
    ) %>%
    filter(dist_num != 31) %>%
    select(dist_num)

police_unit_membership_2002_2016 <-
    read_csv(
        here(
            "create_aggregate_variables",
            "input",
            "monthly_police_unit_membership_2002_2016.csv.gz"
        )
    ) %>%
    mutate(
        race = tolower(race),
        race =
            case_when(
                race == "asian/pacific islander" ~ "aapi",
                race == "hispanic" ~ "hisp",
                race == "native american/alaskan native" ~ "native",
                T ~ race
            ),
        officer_id = as.character(officer_id)
    )

police_unit_membership_1965_2016 <-
    read_csv(
        here(
            "create_aggregate_variables",
            "input",
            "monthly_police_unit_membership_1965_2016.csv.gz"
        )
    ) %>%
    mutate(officer_id = as.character(officer_id))

################################################################################
# Join the panel data together.
################################################################################
police_unit_membership <-
    police_unit_membership_1965_2016 %>%
    lazy_dt() %>%
    full_join(police_unit_membership_2002_2016) %>%
    mutate(unit = as.numeric(unit)) %>%
    filter(
        year(month) >= 2013,
        year(month) <= 2015,
        unit <= 25,
        !(unit %in% c(13, 21, 23))
    ) %>%
    mutate(unit = as.character(unit)) %>%
    mutate(employed = 1) %>%
    as_tibble()

officer_months <-
    expand_grid(
        officer_id = unique(police_unit_membership$officer_id),
        month = ymd("2013-01-01") %m+% months(0:35)
    )

police_unit_membership <-
    police_unit_membership %>%
    lazy_dt() %>%
    full_join(officer_months) %>%
    mutate(employed = if_else(is.na(employed),0, 1)) %>%
    as_tibble()

police_unit_exp <-
    police_unit_membership_1965_2016 %>%
    lazy_dt() %>%
    full_join(police_unit_membership_2002_2016) %>%
    mutate(unit = as.numeric(unit)) %>%
    filter(unit <= 25, !(unit %in% c(13, 21, 23))) %>%
    mutate(unit = as.character(unit)) %>%
    mutate(employed = 1) %>%
    arrange(officer_id, month, unit) %>%
    group_by(officer_id, unit) %>%
    mutate(months_worked_unit = cumsum(employed)) %>%
    group_by(month, unit) %>%
    summarise(mean_months_worked_unit = mean(months_worked_unit)) %>%
    ungroup() %>%
    filter(year(month) >= 2013 & year(month) <= 2015) %>%
    as_tibble()

################################################################################
# Create independent variables
################################################################################
police_unit_size <-
    police_unit_membership %>%
    filter(employed == 1) %>%
    group_by(unit, month) %>%
    summarise(nr_officers = n()) %>%
    ungroup()

police_unit_race <-
    police_unit_membership %>%
    filter(employed == 1) %>% 
    group_by(unit, month, race) %>%
    summarise(nr_officers = n()) %>%
    group_by(unit, month) %>%
    mutate(prcnt_officer = nr_officers / sum(nr_officers)) %>%
    ungroup() %>%
    filter(!is.na(race)) %>%
    pivot_wider(
        id_cols = c("unit", "month"),
        names_from = "race",
        values_from = c("nr_officers", "prcnt_officer")
    )

police_unit_sex <-
    police_unit_membership %>%
    filter(employed == 1) %>% 
    group_by(unit, month, gender) %>%
    summarise(nr_officers = n()) %>%
    group_by(unit, month) %>%
    mutate(prcnt_officer = nr_officers / sum(nr_officers)) %>%
    ungroup() %>%
    filter(!is.na(gender)) %>%
    pivot_wider(
        id_cols = c("unit", "month"),
        names_from = "gender",
        values_from = c("nr_officers", "prcnt_officer")
    )

police_unit_indep_vars <-
    reduce(
        list(
            police_unit_size, police_unit_race, police_unit_sex,
            police_unit_exp
        ),
        full_join,
        by = c("unit", "month")
    )

################################################################################
# Read in dependent variable (police officer-shift outcomes).
################################################################################
outcomes <- read_csv(here("data_cleaning", "output", "3_outcomes.csv.gz"))

################################################################################
# Aggregate officer-shift level outcomes to the police unit-month level.
################################################################################
police_unit_shift_dep_vars <-
    outcomes %>%
    lazy_dt() %>%
    group_by(month, unit) %>%
    summarise(
        stops = sum(stops_n, na.rm = T),
        arrests = sum(arrests_n, na.rm = T),
        force = sum(force_n, na.rm = T),
        black_stops = sum(stops_black, na.rm = T),
        black_arrests = sum(arrests_black, na.rm = T),
        black_force = sum(force_black, na.rm = T),
        hispanic_stops = sum(stops_hispanic, na.rm = T),
        hispanic_arrests = sum(arrests_hispanic, na.rm = T),
        hispanic_force = sum(force_hispanic, na.rm = T),
        white_stops = sum(stops_white, na.rm = T),
        white_arrests = sum(arrests_white, na.rm = T),
        white_force = sum(force_white, na.rm = T),
        other_stops = sum(stops_other, na.rm = T),
        other_arrests = sum(arrests_other, na.rm = T)
    ) %>%
    ungroup() %>%
    filter(
        year(month) >= 2013, year(month) <= 2015, !(unit %in% c(13, 21, 23))
    ) %>%
    mutate(unit = as.character(unit)) %>%
    as_tibble()

################################################################################
# Read in census variables.
################################################################################
# Family structure, immigration status, educational obtainment.
family_immigration <-
    read_csv(
        here(
            "create_aggregate_variables",
            "input",
            "census_family_immigration.csv"
        )
    )

colnames(family_immigration) <-
    c(
        "total_households", "male_single", "female_single", "pop_25older",
        "less9th", "more9thless12th", "total_pop", "foreign_born", "nat_citz",
        "not_citz", "GEOID", "NAME"
    )

family_immigration <-
    family_immigration %>%
    mutate(GEOID = str_replace(GEOID, "1400000US", "")) %>%
    select(-NAME)

# Poverty and unemployment.
economics <-
    read_csv(
        here("create_aggregate_variables", "input", "census_economics.csv")
    )

colnames(economics) <-
    c("pop_16older", "labor_force", "prcnt_poverty", "GEOID", "NAME")

economics <-
    economics %>%
    mutate(
        GEOID = str_replace(GEOID, "1400000US", ""),
        prcnt_poverty =
            if_else(
                prcnt_poverty == "-",
                as.character(0),
                prcnt_poverty
            ),
           prcnt_poverty = as.numeric(prcnt_poverty)
    ) %>%
    select(-NAME)

# Racial demographics.
racial_demog <-
    read_csv(
        here("create_aggregate_variables", "input", "census_population.csv")
    )

colnames(racial_demog) <-
    c("total_pop", "hispanic", "white", "black", "GEOID", "NAME")

racial_demog <-
    racial_demog %>%
    mutate(GEOID = str_replace(GEOID, "1400000US", "")) %>%
    select(-total_pop)

# Census tract shapes. Find the centroid of each census track in Chicago.
census_tract_shapes <-
    st_read(
        here("create_aggregate_variables", "input", "census_tract_shapes")
    ) %>%
    filter(COUNTYFP == "031") %>%
    select(GEOID, NAMELSAD) %>%
    st_centroid()

################################################################################
# Create census independent variables.
################################################################################
census_indep_vars <-
    reduce(
        list(family_immigration, economics, racial_demog, census_tract_shapes),
        full_join, by = "GEOID"
    ) %>%
    mutate(
        across(-c("NAMELSAD", "geometry", "GEOID", "NAME"), ~as.numeric(.x))
    ) %>%
    mutate(nr_poverty = prcnt_poverty / 100 * total_pop) %>%
    select(-prcnt_poverty) %>%
    st_as_sf() %>%
    st_transform(st_crs(districts_shape))

# Join census tracts to police districts based on census tract centroid.
census_police_district_indep_vars <-
    st_join(census_indep_vars, districts_shape) %>%
    filter(!is.na(dist_num)) %>%
    select(-GEOID, -NAMELSAD, -NAME) %>%
    group_by(dist_num) %>%
    summarise(across(-c("geometry"), ~sum(.x))) %>%
    mutate(
        prcnt_single = (male_single + female_single) / total_households,
        prcnt_hs = (less9th + more9thless12th) / pop_25older,
        prcnt_foreign = foreign_born / total_pop,
        prcnt_notlf = 1 - (labor_force / pop_16older),
        prcnt_poverty = nr_poverty / total_pop,
        prcnt_civ_black = black / total_pop,
        prcnt_civ_hispanic = hispanic / total_pop,
        prcnt_civ_white = white / total_pop
    ) %>%
    select(matches("prcnt"), dist_num, black, hispanic, white, total_pop) %>%
    rename(unit = dist_num) %>%
    st_drop_geometry()

# Conduct principal components analysis (although it's not used in final paper).
pca <-
    census_police_district_indep_vars %>%
    select(
        prcnt_single, prcnt_hs, prcnt_foreign, prcnt_notlf, prcnt_poverty
    ) %>%
    prcomp(center = T, scale = T)

census_police_district_indep_vars <-
    census_police_district_indep_vars %>%
    mutate(PCA1 = pca$x[, 1], PCA2 = pca$x[, 2])

################################################################################
# Final police district-level data frame.
################################################################################
crime <-
    read_csv(
        here(
            "create_aggregate_variables", "input", "crime_police_districts.csv"
        )
    ) %>%
    select(district, year, month, violent_cr, property_cr) %>%
    rename(unit = district) %>%
    mutate(
        year = as.character(year),
        month = as.character(month),
        unit = as.character(unit)
    )

final_police_district <-
    full_join(
        police_unit_shift_dep_vars, police_unit_indep_vars,
        by = c("unit", "month")
    ) %>%
    full_join(census_police_district_indep_vars, by = "unit") %>%
    mutate(
        black_diff = prcnt_officer_black - prcnt_civ_black,
        hispanic_diff = prcnt_officer_hisp - prcnt_civ_hispanic,
        white_diff = prcnt_officer_white - prcnt_civ_white,
        black_ratio = prcnt_officer_black / prcnt_civ_black,
        hispanic_ratio = prcnt_officer_hisp / prcnt_civ_hispanic,
        white_ratio = prcnt_officer_white / prcnt_civ_white,
        black_stop_rate = black_stops / black * 10000,
        log_total_officers = log(nr_officers),
        log_black_officers = log(nr_officers_black),
        log_white_officers = log(nr_officers_white),
        log_hispanic_officers = log(nr_officers_hisp),
        year = as.character(year(month)),
        month = as.character(month(month))
    ) %>%
    left_join(crime, by = c("month", "year", "unit")) %>%
    mutate(
        violent_cr_capita = violent_cr / total_pop * 1000,
        property_cr_capita = property_cr / total_pop * 1000
    )
    
write_csv(
    final_police_district,
    here(
        "create_aggregate_variables", "output", "police_district_vars_final.csv"
    )
)
