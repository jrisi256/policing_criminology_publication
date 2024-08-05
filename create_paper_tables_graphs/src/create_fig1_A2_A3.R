library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

################################################################################
# Read in data
################################################################################
read_dir <- here("create_variables", "output")
write_dir <- here("create_paper_tables_graphs", "output")
police_units <-
    read_csv(here(read_dir, "1_police_district_vars_final.csv")) %>%
    mutate(year_month = ym(paste0(year, "_", month)))

################################################################################
# Create figure 1, demonstrating racial congruence
################################################################################
fig1 <-
    police_units %>%
    filter(month == "6") %>%
    ggplot(aes(x = prcnt_civ_black, y = prcnt_officer_black)) +
    geom_point(size = 1) +
    geom_abline() +
    facet_wrap(~year) +
    theme_bw() +
    labs(
        x = "Proportion of Population - Black",
        y = "Proportion of Police Unit - Black",
        title = "Proportion of the population that is Black vs. proportion of the police force that is Black"
    ) +
    theme(text = element_text(size = 12, family = "Times"))

ggsave(
    filename = "Figure 1.pdf", plot = fig1, path = write_dir,
    height = 8, width = 16
)

################################################################################
# Create figs. 2a and 2b demonstrating between + within unit variation in stops.
################################################################################
fig2a <-
    ggplot(police_units, aes(x = year_month, y = black_stops)) +
    facet_wrap(~unit, scales = "free_y") +
    geom_point(size = 1) +
    theme_bw() +
    labs(
        x = "Date",
        y = "Number of Stops Of Black Civilians",
        title = "Number of Stops Within Each Unit Over Time"
    ) +
    theme(text = element_text(size = 12, family = "Times"))

ggsave(
    filename = "Figure A2a.pdf", plot = fig2a, path = write_dir,
    width = 12, height = 8
)

fig2b <-
    ggplot(police_units, aes(x = year_month, y = black_stops)) +
    geom_point(size = 1, aes(color = as.character(unit))) +
    geom_line(linewidth = 0.25, aes(color = as.character(unit), group = unit)) +
    labs(color = "Police Unit") +
    theme_bw() +
    labs(
        x = "Date",
        y = "Number of Stops Of Black Civilians",
        title = "Number of Stops Between Each Unit Over Time"
    ) +
    theme(text = element_text(size = 12, family = "Times"))

ggsave(
    filename = "Figure A2b.pdf",
    plot = fig2b,
    path = write_dir,
    width = 12,
    height = 8
)

################################################################################
# Figs. 3a and 3b, between + within unit variation in racial congruence
################################################################################
fig3a <-
    ggplot(police_units, aes(x = year_month, y = black_ratio)) +
    facet_wrap(~unit, scales = "free_y") +
    geom_point(size = 1) +
    theme_bw() +
    labs(x = "Date",
         y = "Black Racial Congruence",
         title = "Black Racial Congruence Within Each Unit Over Time") +
    theme(text = element_text(size = 12, family = "Times"))

ggsave(
    filename = "Figure A3a.pdf",
    plot = fig3a,
    path = write_dir,
    height = 8,
    width = 12
)

fig3b <-
    ggplot(police_units, aes(x = year_month, y = black_ratio)) +
    geom_point(size = 1, aes(color = as.character(unit))) +
    geom_line(linewidth = 0.25, aes(color = as.character(unit), group = unit)) +
    theme_bw() +
    labs(x = "Date", y = "Black Racial Congruence", color = "Police Unit",
         title = "Black Racial Congruence Between Each Unit Over Time") +
    theme(text = element_text(size = 12, family = "Times"))

ggsave(
    filename = "Figure A3b.pdf",
    plot = fig3b,
    path = write_dir,
    height = 8,
    width = 12
)
