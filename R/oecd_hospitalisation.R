library(tidyverse)
library(showtext)
library(ggtext)

# Set up the Lato font
font_add_google("Lato", "lato")

## Data ----
# Load data from CSV
# (API is also available)
oecd_ind <- readr::read_csv("data/raw/HEALTH_HCQI_17042022170845425.csv") |>
  rename(
    Statistic = Value...12,
    Value = Value...13
  )

# Get labels for the different statistics in the dataset
lab_stat <- oecd_ind |>
  select(Indicator, VAL, Statistic) |>
  distinct()

# For each country, get the first and last year with amputation data
country_minmax <- oecd_ind |>
  group_by(Indicator, Country) |>
  summarise(
    min_year = min(Periods),
    max_year = max(Periods)
  )

# Tidy data
oecd_ind <- oecd_ind |>
  filter(VAL %in% c("AS_STD_RATE_MPOP", "LOW_CI", "UP_CI")) |>
  select(Periods, Country, Indicator, Gender, AGE, VAL, Value) |>
  pivot_wider(names_from = "VAL", values_from = "Value")


# Annotate dataset to mark first and last years for each country
oecd_ind <- oecd_ind |>
  inner_join(country_minmax, by = c("Country", "Indicator")) |>
  mutate(is_min_year = Periods == min_year,
         is_max_year = Periods == max_year)

## Chart ----
oecd_plot <- function(indicator, dat, title = indicator) {
  dat |>
    filter(Indicator == indicator,
           Gender != "Total",
           Periods >= 2017,
           is_max_year) |>
    mutate(country_label = glue::glue("{Country} ({Periods})")) |>
    ggplot() +
    aes(
      x = AS_STD_RATE_MPOP,
      xmin = LOW_CI,
      xmax = UP_CI,
      y = reorder(country_label, AS_STD_RATE_MPOP),
      colour = Gender
    ) +
    geom_point(size = 2) +
    geom_errorbarh(alpha = 0.5) +
    geom_vline(xintercept = 0, colour = "grey80") +
    scale_colour_brewer(type = "qual", palette = "Set1", guide = "none") +
    labs(
      title = paste("OECD Indicator:", title),
      subtitle = "Standardised rates in the
    <span style='color:#E41A1C;'>__female__</span> and
    <span style='color:#377EB8;'>__male__</span> population (age 15+)
    ",
      x = lab_stat$Statistic[lab_stat$VAL == "AS_STD_RATE_MPOP"],
      y = "",
      caption = "https://github.com/edonnachie/oecd-health"
    ) +
    expand_limits(x = 0) +
    coord_cartesian(expand = FALSE) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.justification = "left",
      text = element_text(size = 14,  family="lato"),
      plot.subtitle = element_markdown(size = 14,  family = "lato"),
      plot.caption = element_text(size = 10,  family = "lato", colour = "grey40"),
      axis.title.x = element_text(size = 12, family = "lato")
    )
}

ggsave_oecd <- function(name) {
  ggsave(filename = glue::glue("plots/oecd_{name}.png"),
         type = "cairo", bg = "white",
         width = 20, height = 15, units = "cm", dpi = 300)
}

oecd_plot("Asthma hospital admission", oecd_admission)
ggsave_oecd("asthma")

oecd_plot("Chronic obstructive pulmonary disease hospital admission", oecd_ind,
          title = "COPD hospital admission")
ggsave_oecd("copd")

oecd_plot("Diabetes hospital admission", oecd_ind)
ggsave_oecd("diabetes")

oecd_plot("Hypertension hospital admission", oecd_ind)
ggsave_oecd("hypertension")

oecd_plot("Congestive heart failure hospital admission", oecd_ind,
          title = "Heart failure hospital admission")
ggsave_oecd("heart_failure")
