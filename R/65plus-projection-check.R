# Checking 65+ projections against expected by ONS
library(readxl)
library(dplyr)

data <- read_excel("output/population_projections.xlsx") %>%
  filter(
    AgeBandSortable >= 65
  ) %>%
  group_by(
    Year, run_index
  ) %>%
  summarize(
    Population = sum(Population)
  ) %>%
  group_by(
    Year
  ) %>%
  summarize(
    Population = mean(Population),
    Pop95Upper = quantile(Population, 0.975),
    Pop95Lower = quantile(Population, 0.025),
  ) %>%
  mutate(
    PercChange = 100*(Population - 176573) / 176573
  )