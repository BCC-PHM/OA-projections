source("R/config.R")
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

################################################################################
#       Load and process population data (ONS 2023 mid-year estimates)         #
################################################################################
# Using 2023 instead of 2024 for better consistency with mortality data

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales
pop_data_path <- file.path(pop_data_path_prefix,
                           "Population EstimatesMID 2023 LA Level 230725.xlsx")

# Define a function to process each sheet
read_gender_sheet <- function(sheet_name) {
  read_excel(pop_data_path, sheet = sheet_name, skip = 7) %>%
    filter(Name == "Birmingham") %>%
    pivot_longer(cols = 5:95, names_to = "Age", values_to = "Count") %>%
    mutate(Sex = gsub("MYE2 - ", "", sheet_name),
           Sex = gsub("s", "", Sex)) %>%
    select(Age, Count, Sex)
}

# Read and combine male and female data
pop_est <- c("MYE2 - Females", "MYE2 - Males") %>%
  lapply(read_gender_sheet) %>%
  rbindlist() 

pop_grouped <- pop_est%>%
  # aggregate into 5 year bands
  mutate(
    Age = as.numeric(gsub("\\+", "", Age)),
    AgeBand = cut(
      Age,
      breaks = seq(0, 95, 5),
      labels = c("0-4", "5-9", "10-14", "15-19", "20-24",
                 "25-29", "30-34", "35-39", "40-44", "45-49",
                 "50-54", "55-59", "60-64", "65-69", "70-74",
                 "75-79", "80-84", "85-89", "90+"),
      right = FALSE
    )
  ) %>%
  group_by(
    Sex, AgeBand
  ) %>%
  summarise(
    Population = sum(Count),
    .groups = "drop"
  ) %>%
  mutate(
    Year = 2024,
    AgeBandSortable = as.numeric(sub("^([0-9]+).*", "\\1", AgeBand))
  ) %>%
  select(
    Sex, AgeBand, AgeBandSortable, Population, Year
  )

################################################################################
#                      Load and process mortality data                         #
################################################################################

mort_data_path <- file.path(mortality_data_path_prefix,
                           "deaths_by_year_sex_ageband_2019_2023_bham.csv")

NumYears = 3

mort_rates <- read.csv(mort_data_path) %>%
  filter(
    !is.na(Sex),
    # Filter out pandemic
    YearofDeath %in% c(2019, 2022, 2023)
    ) %>%
  group_by(
    Sex, AgeBand
  ) %>%
  summarize(
    Deaths5Years = sum(Deaths),
    .groups = "drop"
  ) %>%
  left_join(
    pop_grouped,
    by = join_by(Sex, AgeBand)
  ) %>%
  mutate(
    AgeBand = factor(AgeBand, levels = unique(pop_grouped$AgeBand)),
    p_hat = Deaths5Years / (NumYears * Population),
    MortRate =  p_hat,
    a_prime = Deaths5Years + 1,
    Z = qnorm(0.975),
    MortRateLowerCI95 = Deaths5Years * (1 - 1/(9*Deaths5Years) - Z/3 * sqrt(1/a_prime))**3/(NumYears * Population),
    MortRateUpperCI95 = a_prime * (1 - 1/(9*a_prime) + Z/3 * sqrt(1/a_prime))**3/(NumYears * Population),
    AgeBandSortable = as.numeric(sub("^([0-9]+).*", "\\1", AgeBand))
  ) %>%
  select(
    Sex, AgeBand, AgeBandSortable, MortRate, MortRateLowerCI95, MortRateUpperCI95
  )


################################################################################
#                           Plot mortality rates                               #
################################################################################

male_color <- "#9657E0"
female_color <- "#1DAA47"

ggplot(mort_rates, aes(x = AgeBand, y = 1e5*MortRate, fill = Sex)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(x = AgeBand, ymin = 1e5*MortRateLowerCI95, ymax = 1e5*MortRateUpperCI95),
    width = 0.5,
    position = position_dodge(0.9),
    ) +
  theme_bw() +
  labs(
    y = "Average annual deathsper 100,000\npopulation (2019, 2022, 2023)",
    x = "Age band (Years)",
    fill = ""
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  theme(
    legend.position = "top",
    legend.direction="horizontal"
  )+
  scale_y_continuous(
    limits = c(0, 26e3),
    expand = c(0,0)
  )

ggsave("output/mortality_rates_19to23.png", width = 8, height = 4)

################################################################################
#                        Project populations                                   #
################################################################################

estimate_next_five <- function(
  current_population,
  mortality_rates
) {
  
  # Get new year
  next_year <- unique(current_population$Year) + 5
  print(Year)
  # Get 0 to 4 population to propagate births
  pop_0to4 <- current_population %>%
    filter(AgeBand == "0-4")
    
  
  # Join mortality
  joined <- current_population %>%
    left_join(
      mortality_rates,
      by = join_by(Sex, AgeBand, AgeBandSortable)
    ) %>%
    arrange(
      Sex, AgeBandSortable
    )
  
  # Apply mortality
  pop_after_deaths <- joined %>%
    mutate(
      Population = Population * (1 - 5 * MortRate),
    )
  
  age_groups <- levels(joined$AgeBand)
  
  shift_lookup <- setNames(
    c(age_groups[-1], "90+"),  # Destination (note last one repeats)
    age_groups                 # Current
  )

  # Apply the shift
  aged_up <- pop_after_deaths %>%
    mutate(
      AgeBand = shift_lookup[as.character(AgeBand)]
    ) %>%
    group_by(
      Sex, AgeBand
      ) %>%
    summarise(
      Population = sum(Population), 
      .groups = "drop"
      ) %>%
    mutate(
      AgeBandSortable = as.numeric(sub("^([0-9]+).*", "\\1", AgeBand)),
      Year := next_year
      ) %>%
    select(
      Sex, AgeBand, AgeBandSortable, Population, Year
    ) %>%
    rbind(
      pop_0to4
    ) %>%
    mutate(
      Year := next_year
    ) %>%
    arrange(
      Sex, AgeBandSortable
    )
  aged_up$AgeBand <- as.factor(aged_up$AgeBand)
  
  return(aged_up)
  
}
 
prop_proj_list <- list("2023" = pop_grouped)
 
for (Year in seq(2028, 2053, 5)) {
  prop_proj_list[[as.character(Year)]] <- estimate_next_five(
    prop_proj_list[[as.character(Year - 5)]],
    mort_rates
  )
}

pop_proj <- rbindlist(prop_proj_list)

proj_plot <- pop_proj %>% 
  filter(AgeBandSortable > 45) %>%
  ggplot(aes(x = Year, y = Population)) +
  geom_line() +
  theme_bw() +
  facet_grid(
    rows = vars(AgeBand), 
    cols = vars(Sex),
    scales="free_y"
  )
proj_plot

ggsave("output/basic-projection.png",
       proj_plot, width = 5, height = 8)