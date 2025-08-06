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
    
    # Calculate confidence intervals
    a_prime = Num_Admissions + 1,
    Z = qnorm(0.975),
    FallRateLowerCI95 = Num_Admissions * (1 - 1/(9*Num_Admissions) - Z/3 * sqrt(1/a_prime))**3/(3 * Population),
    FallRateUpperCI95 = a_prime * (1 - 1/(9*a_prime) + Z/3 * sqrt(1/a_prime))**3/(3 * Population),
    
    # Estimate standard deviation
    sd = (FallRateUpperCI95 - FallRateLowerCI95) / (2 * 1.96)
  ) %>%
  select(
    Sex, AgeBand, FallRate, FallRateUpperCI95, FallRateLowerCI95, sd
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


################################################################################
#          Apply random fall rate draws to population projections              #
################################################################################

run_list <- list()
for (i in 1:1000) {
  set.seed(i)
  
  projection_i <- pop_proj %>%
    filter(
      run_index == i
    ) %>% 
    left_join(
      fall_rate,
      by = join_by(Sex, AgeBand)
    ) %>%
    mutate(
      FallRateRandom = rnorm(n = n(), mean=FallRate, sd=sd),
      EstimatedFallCount = FallRateRandom * Population
    ) %>%
    group_by(
      Year, Sex
    ) %>%
    summarise(
      EstimatedFallCount = sum(EstimatedFallCount),
      Population = sum(Population),
      .groups = "drop"
    ) %>%
    mutate(
      FallRateEstimate = EstimatedFallCount / (Population)
    )
  
  run_list[[i]] <- projection_i
}


################################################################################
#        Combine all runs and calculate mean and confidence intervals          #
################################################################################

# Combine all runs
estimated_rate <- rbindlist(run_list) %>%
  group_by(
    Year, Sex
  ) %>%
  summarise(
    # Dementia prevalence
    FallRateUpperCI95 = quantile(FallRateEstimate, 0.975),
    FallRateLowerCI95 = quantile(FallRateEstimate, 0.025),
    FallRate = mean(FallRateEstimate),
    
    # Dementia count
    FallCountUpperCI95 = quantile(EstimatedFallCount, 0.975),
    FallCountLowerCI95 = quantile(EstimatedFallCount, 0.025),
    FallCount = mean(EstimatedFallCount),
    
    .groups = "drop"
  )


################################################################################
#                         Plot fall rate projection                            #
################################################################################

fall_rate_proj <- ggplot(
  estimated_rate, 
  aes(
    x = Year,
    y = 1e5 * FallRate, 
    color = Sex, 
    fill = Sex
  )
) + 
  geom_ribbon(
    aes(x = Year, ymax = 1e5 * FallRateLowerCI95, ymin = 1e5 * FallRateUpperCI95),
    alpha = 0.5,
    linewidth = 0
  ) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 1500),
    expand = c(0,0),
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  scale_color_manual(values = c(female_color, male_color)) +
  theme(
    legend.position = "top",
    legend.direction="horizontal"
  ) +
  labs(
    y = "Estimated Annual Fall Rate\nper 100,000 people (50+)",
    x = "Year",
    fill = "",
    color = ""
  )
fall_rate_proj

ggsave("output/fall_rate_projection.png",
       fall_rate_proj, width = 5, height = 3)


# Plot fall count projection

fall_count_proj <- ggplot(
  estimated_rate, 
  aes(
    x = Year,
    y = FallCount, 
    color = Sex, 
    fill = Sex
  )
) + 
  geom_ribbon(
    aes(x = Year, ymax = FallCountLowerCI95, ymin = FallCountUpperCI95),
    alpha = 0.5,
    linewidth = 0
  ) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 5000),
    expand = c(0,0),
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  scale_color_manual(values = c(female_color, male_color)) +
  theme(
    legend.position = "top",
    legend.direction="horizontal"
  ) +
  labs(
    y = "Estimated Annual Fall Counts (50+)",
    x = "Year",
    fill = "",
    color = ""
  )
fall_count_proj

ggsave("output/fall_count_projection.png",
       fall_count_proj, width = 5, height = 3)

################################################################################
#                            Save Excel Output                                 #
################################################################################

output <- list(
  "Falls Projection" = estimated_rate
)

writexl::write_xlsx(
  output,
  path = "output/fall-projection.xlsx"
)