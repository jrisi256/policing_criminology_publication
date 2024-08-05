library(here)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

################################################################################
# Read in data
################################################################################
read_dir <- here("create_variables", "output")
write_dir <- here("create_paper_tables_graphs", "output")
police_units <- read_csv(here(read_dir, "1_police_district_vars_final.csv"))

################################################################################
# Create data frame for Appendix Figure A1
################################################################################
graph_df <-
    police_units %>%
    group_by(unit) %>%
    summarise(
        white_stop_rate = median(white_stop_rate),
        black_stop_rate = median(black_stop_rate),
        hispanic_stop_rate = median(hispanic_stop_rate)
    ) %>%
    pivot_longer(
        cols = matches("rate"),
        names_to = "Race",
        values_to = "Stops"
    ) %>%
    mutate(
        Race =
            case_when(
                Race == "white_stop_rate" ~ "White",
                Race == "black_stop_rate" ~ "Black",
                Race == "hispanic_stop_rate" ~ "Hisp."
            )
    ) %>%
    group_by(unit) %>%
    mutate(
        max_match = Stops == max(Stops),
        max_match_black = if_else(max_match & Race == "Black", T, F),
        max_unit = any(max_match_black)
    ) %>%
    ungroup()

sorting_var <-
    graph_df %>%
    filter(Race == "Black") %>%
    arrange(desc(max_match), desc(Stops))

graph_df <- graph_df %>% mutate(unit = factor(unit, levels = sorting_var$unit))

figa1 <-
    graph_df %>%
    ggplot(aes(x = Race, y = Stops)) +
    geom_bar(stat = "identity", aes(fill = max_unit)) +
    theme_bw() +
    labs(
        x = "Race",
        y = "Stopping Rate Per 10,000",
        title = "Median Stopping Rate By Race For Each Police Unit",
        fill = "Stopping Rate Highest For Black Individuals?"
    ) +
    facet_wrap(~unit, scales = "free_y") +
    theme(
        text = element_text(size = 12, family = "Times"),
        axis.text.x = element_text(angle = 320, hjust = 0, vjust = 1),
        legend.title = element_text(size = 8),
        legend.text=element_text(size = 8)
    )

ggsave(
    filename = "Figure A1.pdf",
    plot = figa1,
    path = write_dir,
    height = 8,
    width = 12
)
