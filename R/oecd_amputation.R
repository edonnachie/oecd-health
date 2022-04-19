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

# Tidy data
oecd_amputation <- oecd_ind |>
  filter(Indicator == "Diabetes lower extremity amputation using unlinked data",
         VAL %in% c("AS_STD_RATE_MPOP", "LOW_CI", "UP_CI")) |>
  select(Periods, Country, Indicator, Gender, AGE, VAL, Value) |>
  pivot_wider(names_from = "VAL", values_from = "Value")

# Get labels for the different statistics in the dataset
lab_stat <- oecd_ind |>
  select(VAL, Statistic) |>
  distinct()

# For each country, get the first and last year with amputation data
country_minmax <- oecd_amputation |>
  group_by(Country) |>
  summarise(
    min_year = min(Periods),
    max_year = max(Periods)
    )

# Annotate dataset to mark first and last years for each country
oecd_amputation <- oecd_amputation |>
  inner_join(country_minmax, by = "Country") |>
  mutate(is_min_year = Periods == min_year,
         is_max_year = Periods == max_year)

## Chart ----
oecd_amputation |>
  filter(Gender != "Total",
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
    title = "OECD Countries Still Struggle With Diabetes Care",
    subtitle = "Rate of diabetes-related ampuation of the toe, foot and leg<br />in the
    <span style='color:#E41A1C;'>__male__</span> and
    <span style='color:#377EB8;'>__female__</span> population (age 15+)
    ",
    x = lab_stat$Statistic[lab_stat$VAL == "AS_STD_RATE_MPOP"],
    y = "",
    caption = "https://github.com/edonnachie/oecd-health"
  ) +
  coord_cartesian(xlim = c(0, 49), expand = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.justification = "left",
    text = element_text(size = 14,  family="lato"),
    plot.subtitle = element_markdown(size = 14,  family = "lato"),
    plot.caption = element_text(size = 10,  family = "lato", colour = "grey40"),
    axis.title.x = element_text(size = 12, family = "lato")
  )
ggsave(filename = "plots/oecd_amputation.png",
       type = "cairo",
       width = 20, height = 15, units = "cm", dpi = 300)
