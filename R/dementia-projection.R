source("R/config.R")
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

# Load CFAS II reference rates
dementia_prevs <- read_excel("data/dementia-reference-rates.xlsx") %>%
  mutate(
    sd = (DementiaPrevalence95Upper - DementiaPrevalence95Lower) / (2 * 1.96)
  )

# Load population projections
pop_proj <- read_excel("output/population_projections.xlsx") %>%
  filter(
    AgeBand %in% dementia_prevs$AgeBand
  )

################################################################################
#                       Visualise dementia prevalence                          #
################################################################################

male_color <- "#9657E0"
female_color <- "#1DAA47"

dementia_prevs <- ggplot(dementia_prevs, aes(
  x = AgeBand,
  y = DementiaPrevalence,
  fill = Sex
)) +
  geom_col(position = "dodge") +
  geom_errorbar(
    aes(
      ymin = DementiaPrevalence95Lower,
      ymax = DementiaPrevalence95Upper
      ),
    width = 0.5,
    position = position_dodge(0.9),
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.direction="horizontal"
  ) +
  scale_y_continuous(
    limits = c(0, 0.5),
    expand = c(0,0),
    labels = scales::percent
  ) +
  labs(
    y = "Dementia Prevalence Rate\n(CFAS II reference rates)",
    x = "Age Band",
    fill = ""
  )

dementia_prevs

ggsave("output/dementia_prevs.png",
       dementia_prevs,
       height = 3,
       width = 5)


################################################################################
#          Apply random dementia draws to population projections               #
################################################################################

run_list <- list()
for (i in 1:1000) {
  set.seed(i)
  
  projection_i <- pop_proj %>%
    filter(
      run_index == i
    ) %>% 
    left_join(
      dementia_prevs,
      by = join_by(Sex, AgeBand)
      ) %>%
    mutate(
      DementiaPrevalenceRandom = rnorm(n = n(), mean=DementiaPrevalence, sd=sd),
      EstimatedDementiaCount = DementiaPrevalenceRandom * Population
    ) %>%
    group_by(
      Year, Sex
    ) %>%
    summarise(
      EstimatedDementiaCount = sum(EstimatedDementiaCount),
      Population = sum(Population),
      .groups = "drop"
    ) %>%
    mutate(
      DementiaPrevalenceEstimate = EstimatedDementiaCount / Population
    )
    
  run_list[[i]] <- projection_i
}

# Combine all runs
estimated_prevalence <- rbindlist(run_list) %>%
  group_by(
    Year, Sex
  ) %>%
  summarise(
    # Dementia prevalence
    DementiaUpperCI95 = quantile(DementiaPrevalenceEstimate, 0.975),
    DementiaLowerCI95 = quantile(DementiaPrevalenceEstimate, 0.025),
    DementiaPrevalence = mean(DementiaPrevalenceEstimate),
    
    # Dementia count
    DementiaCountUpperCI95 = quantile(EstimatedDementiaCount, 0.975),
    DementiaCountLowerCI95 = quantile(EstimatedDementiaCount, 0.025),
    DementiaCount = mean(EstimatedDementiaCount),
    
    .groups = "drop"
  )

# Plot dementia prevalence projection

dementia_prev_proj <- ggplot(
  estimated_prevalence, 
  aes(
    x = Year,
    y = DementiaPrevalence, 
    color = Sex, 
    fill = Sex
  )
) + 
  geom_ribbon(
    aes(x = Year, ymax = DementiaLowerCI95, ymin = DementiaUpperCI95),
    alpha = 0.5,
    linewidth = 0
  ) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 0.1),
    expand = c(0,0),
    labels = scales::percent
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  scale_color_manual(values = c(female_color, male_color)) +
  theme(
    legend.position = "top",
    legend.direction="horizontal"
  ) +
  labs(
    y = "Estimated Dementia Prevalence Rate (65+)",
    x = "Year",
    fill = "",
    color = ""
  )
dementia_prev_proj

ggsave("output/dementia_prev_projection.png",
       dementia_prev_proj, width = 5, height = 3)


# Plot dementia prevalence projection

dementia_count_proj <- ggplot(
  estimated_prevalence, 
  aes(
    x = Year,
    y = DementiaCount, 
    color = Sex, 
    fill = Sex
  )
) + 
  geom_ribbon(
    aes(x = Year, ymax = DementiaCountLowerCI95, ymin = DementiaCountUpperCI95),
    alpha = 0.5,
    linewidth = 0
  ) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(
    limits = c(0, 1e4),
    expand = c(0,0),
  ) +
  scale_fill_manual(values = c(female_color, male_color)) +
  scale_color_manual(values = c(female_color, male_color)) +
  theme(
    legend.position = "top",
    legend.direction="horizontal"
  ) +
  labs(
    y = "Estimated Dementia Counts (65+)",
    x = "Year",
    fill = "",
    color = ""
  )
dementia_count_proj

ggsave("output/dementia_count_projection.png",
       dementia_count_proj, width = 5, height = 3)
