source("R/config.R")
library(readxl)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

# Load CFAS II reference rates
dementia_prevs <- read_excel("data/dementia-reference-rates.xlsx")

# Load population projections
pop_proj <- read_excel("output/population_projections.xlsx")

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