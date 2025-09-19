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

pop_grouped <- read.csv(file.path(
  pop_data_path_prefix,
  "GP_reg_data_full_Dec_24.csv"
)) %>%
  filter(
    # Birmingham only
    Locality %in% c("North", "South", "East", "West", "Central"),
    Gender %in% c("Male", "Female")
  ) %>%
  mutate(
    Sex = Gender,
    AgeBand = case_when(
      AgeBand %in% c("90-94", "95-99", "100-104", "105-109", "110-114") ~ "90+",
      AgeBand == "05-Sep" ~ "5-9",
      AgeBand == "Oct-14" ~ "10-14",
      TRUE ~ AgeBand
    )
  ) %>%
  group_by(
    Sex, AgeBand
  ) %>%
  summarise(
    Population = sum(Observations),
    .groups = "drop"
  ) %>%
  mutate(
    Year = 2024,
    AgeBandSortable = as.numeric(sub("^([0-9]+).*", "\\1", AgeBand))
  ) %>%
  arrange(Sex, AgeBandSortable) %>%
  select(
    Sex, AgeBand, AgeBandSortable, Population, Year
  )

writexl::write_xlsx(pop_grouped, "data/Birmingham-GP-reg-pop-Dec24.xlsx")
  
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
    y = "Average annual deaths per 100,000\npopulation (2019, 2022, 2023)",
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
#                           Load Migration Data                                #
################################################################################

migration_data <- read_excel("data/Bimringham_internal_migration.xlsx")

migration_plot_data <- migration_data %>%
  mutate(
    AgeBandSortable = as.numeric(stringr::str_extract(AgeBand, "^\\d{1,2}")),
    Outward_Migration = - Outward_Migration,
    Net_Migration = Inward_Migration + Outward_Migration
  ) %>%
  tidyr::pivot_longer(
    cols = contains("Migration"),
    values_to = "Yearly_Migration",
    names_to = "Migration_Type"
  ) %>%
  arrange(AgeBandSortable)%>%
  mutate(
    direction = case_when(
      Yearly_Migration < 0 ~ "Out",
      TRUE ~ "In"
    ),
    AgeBand = factor(AgeBand, 
                     levels=unique(AgeBand), 
                     ordered=TRUE)
  ) 

labels = c(
  Female = "Female",
  Male = "Male",
  Inward_Migration = "Inward",
  Net_Migration = "Net",
  Outward_Migration = "Out"
)

mig_plot <- ggplot(
  migration_plot_data, 
  aes(x = AgeBand, y = Yearly_Migration, fill = direction)
  ) +
  geom_col() +
  theme_bw() +
  facet_grid(
    rows = vars(Migration_Type), 
    cols = vars(Sex),
    scales="free_y",
    labeller = as_labeller(labels)
    ) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position = "none"
    ) +
  labs(
    y = "Yearly Internal Migration in Birmingham (2024)",
    x = ""
  )
mig_plot
ggsave("output/migration.png", width = 6, height = 4)

################################################################################
#                        Projehttp://127.0.0.1:40709/graphics/6b00a20a-e55a-4a49-80e0-eab80fa3381e.pngct populations                                   #
################################################################################

estimate_next_five <- function(
  current_population,
  mortality_rates,
  migration_data = NULL
) {
  
  # Get new year
  next_year <- unique(current_population$Year) + 5

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
  
  # Apply migration if data given
  if (!is.null(migration_data)) {
    # Check migration data has the correct column names
    if(!all(
      c("Sex", "AgeBand", "Inward_Migration", "Outward_Migration") %in% colnames(migration_data)
      )
      ) {
      stop("Migration data must have columns: 'Sex' and 'AgeBand'")
    }
    pop_after_migration <- joined %>%
      left_join(
        migration_data,
        by = join_by(Sex, AgeBand)
        ) %>%
      mutate(
        Population = Population + 5 * Inward_Migration - 5 * Outward_Migration
      )
    
    #print(head(pop_after_migration))
  } else {
    pop_after_migration <- joined
   }
  
  # Apply mortality
  pop_after_deaths <- pop_after_migration %>%
    mutate(
      # Approximate standard deviation
      sd = (MortRateUpperCI95 - MortRateLowerCI95) / (2 * 1.96),
      MortRateRandom = rnorm(n = n(), mean=MortRate, sd=sd),
      # Calculate new population
      Population = Population * (1 - 5 * MortRateRandom),
      AgeBand = as.factor(AgeBand)
    )
  
  age_groups <- levels(factor(joined$AgeBand))
  
  #print(age_groups)
  
  shift_lookup <- setNames(
    c(age_groups[-1], "90+"),  # Destination (note last one repeats)
    age_groups                 # Current
  )
  
  #print(shift_lookup)

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

project_population <- function(
    current_population,
    mortality_rates,
    start_year,
    stop_year,
    run_index,
    migration_data = NULL
) {
  
  set.seed(run_index)
  
  prop_proj_list <- setNames(
    list(current_population),
    as.character(start_year)
  )
  
  for (Year in seq(start_year + 5, stop_year, 5)) {
    prop_proj_list[[as.character(Year)]] <- estimate_next_five(
      prop_proj_list[[as.character(Year - 5)]],
      mortality_rates,
      migration_data = migration_data
    )
  }
  
  pop_proj <- rbindlist(prop_proj_list) %>%
    mutate(
      run_index := run_index
    )
  
  return(pop_proj)
  
}

# Run projection 1000 times drawing different values from mortality rate 
# distribution each time
run_list <- list()
for (i in 1:1000) {
  run_list[[i]] <- project_population(
    current_population = pop_grouped,
    mortality_rates = mort_rates,
    start_year = 2024,
    stop_year = 2054,
    run_index = i,
    migration_data = migration_data
  )
}
# Combine all runs
all_runs <- rbindlist(run_list)

# Save population projections
writexl::write_xlsx(
  all_runs,
  "output/population_projections.xlsx"
)

# Visualise population projection by age and sex

mean_val_grouped <- all_runs %>%
  group_by(
    Year, Sex, AgeBand, AgeBandSortable
  ) %>%
  summarise(
    PopUpperCI95 = quantile(Population, 0.975),
    PopLowerCI95 = quantile(Population, 0.025),
    Population = mean(Population),
    .groups = "drop"
  )

proj_plot <- mean_val_grouped %>% 
  filter(AgeBandSortable > 45) %>%
  ggplot(aes(x = Year, y = Population)) +
  geom_ribbon(
    aes(x = Year, ymax = PopUpperCI95, ymin = PopLowerCI95),
    fill = "#5FB3FA",
    alpha = 0.5
  ) +
  geom_line() +
  theme_bw() +
  facet_grid(
    rows = vars(AgeBand), 
    cols = vars(Sex),
    scales="free_y"
  ) +
  labs(
    y = "Projected Population (with migration)"
  )
proj_plot

ggsave("output/projection_with_uncertainty.png",
       proj_plot, width = 5, height = 8)

