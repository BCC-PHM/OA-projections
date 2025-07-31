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
    Year = 2024
  )

################################################################################
#                      Load and process mortality data                         #
################################################################################

mort_data_path <- file.path(mortality_data_path_prefix,
                           "deaths_by_year_sex_ageband_2019_2023_bham.csv")

mort_data <- read.csv(mort_data_path) %>%
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
    p_hat = Deaths5Years / (5 * Population),
    Value = 1e5 * p_hat,
    a_prime = Deaths5Years + 1,
    Z = qnorm(0.975),
    LowerCI95 = 1e5 * Deaths5Years * (1 - 1/(9*Deaths5Years) - Z/3 * sqrt(1/a_prime))**3/(5 * Population),
    UpperCI95 = 1e5 * a_prime * (1 - 1/(9*a_prime) + Z/3 * sqrt(1/a_prime))**3/(5 * Population)
  )




