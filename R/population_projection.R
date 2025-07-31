source("R/config.R")
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)

# Load population data (ONS 2024 mid-year estimates)
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales
pop_data_path <- file.path(pop_data_path_prefix,
                           "Population EstimatesMID 2024 LA Level.xlsx")

# Define a function to process each sheet
read_gender_sheet <- function(sheet_name) {
  read_excel(pop_data_path, sheet = sheet_name, skip = 7) %>%
    filter(Name == "Birmingham") %>%
    pivot_longer(cols = 5:95, names_to = "Age", values_to = "Count") %>%
    mutate(Gender = gsub("MYE2 - ", "", sheet_name),
           Gender = gsub("s", "", Gender)) %>%
    select(Age, Count, Gender)
}

# Read and combine male and female data
pop_est <- c("MYE2 - Females", "MYE2 - Males") %>%
  lapply(read_gender_sheet) %>%
  rbindlist() 

pop_grouped <- pop_est%>%
  # aggregate into 5 year bands
  mutate(
    Age = as.numeric(gsub("\\+", "", Age)),
    Age_Group = cut(
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
    Gender, Age_Group
  ) %>%
  summarise(
    Count = sum(Count),
    .groups = "drop"
  ) %>%
  mutate(
    Year = 2024
  )