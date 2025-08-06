source("R/config.R")
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

################################################################################
#               Load data and calculate fall admission rate                    #
################################################################################

fall_rate <- read_excel(
  "data/fall-counts-2022to2024.xlsx", 
  sheet = "data") %>%
  filter(AgeBand != "0-49") %>%
  left_join(
    read_excel("data/Birmingham-GP-reg-pop-Dec24.xlsx") %>% 
      select(-c(Year)),
    by = join_by(AgeBand, Sex)
  ) %>%
  mutate(
    
    FallRate = Num_Admissions / (3 * Population),
    a_prime = Num_Admissions + 1,
    Z = qnorm(0.975),
    FallRateLowerCI95 = Num_Admissions * (1 - 1/(9*Num_Admissions) - Z/3 * sqrt(1/a_prime))**3/(3 * Population),
    FallRateUpperCI95 = a_prime * (1 - 1/(9*a_prime) + Z/3 * sqrt(1/a_prime))**3/(3 * Population),
    
  )


# Load population projections
pop_proj <- read_excel("output/population_projections.xlsx") %>%
  filter(
    AgeBand %in% fall_rate$AgeBand
  )


################################################################################
#                           Plot Fall rates                               #
################################################################################

male_color <- "#9657E0"
female_color <- "#1DAA47"

ggplot(fall_rate, aes(x = AgeBand, y = 1e5*FallRate, fill = Sex)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(x = AgeBand, ymin = 1e5*FallRateLowerCI95, ymax = 1e5*FallRateUpperCI95),
    width = 0.5,
    position = position_dodge(0.9),
  ) +
  theme_bw() +
  labs(
    y = "Average Annual Emergency Admissions\nfor Falls per 100,000 population (2022-24)",
    x = "Age band (Years)",
    fill = ""
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  theme(
    legend.position = "top",
    legend.direction="horizontal"
  )+
  scale_y_continuous(
    limits = c(0, 10e3),
    expand = c(0,0)
  )

ggsave("output/fall_rates_22to24.png", width = 8, height = 4)
