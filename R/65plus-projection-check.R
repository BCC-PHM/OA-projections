# Checking 65+ projections against expected by ONS
library(readxl)
library(dplyr)
library(ggplot2)

older_adult_pop <- read_excel("output/population_projections.xlsx") %>%
  mutate(
    Type = "With Migration"
  ) %>%
  rbind(
    read_excel("output/population_projections_no_migration.xlsx")%>%
      mutate(
        Type = "No Migration"
      )
  ) %>%
  filter(
    AgeBandSortable >= 65
  ) %>%
  group_by(
    Year, Type, run_index
  ) %>%
  summarize(
    Population = sum(Population)
  ) %>%
  group_by(
    Year, Type
  ) %>%
  summarize(
    PopUpperCI95 = quantile(Population, 0.975),
    PopLowerCI95 = quantile(Population, 0.025),
    Population = mean(Population)
  ) %>%
  mutate(
    PercChange = 100*(Population - 176573) / 176573
  )

ONS_projection <- data.frame(
  Year = c(2025, 2030, 2035, 2040, 2045),
  Population = c(155800, 166100, 176500, 182300, 187500),
  Type = "Scaled ONS Projection*"
)

proj_plot_comb <- older_adult_pop %>%
  ggplot(aes(x = Year, y = Population/1e5, color = Type)) +
  geom_line() +
  geom_point() +
  geom_point(
    data = ONS_projection, 
    aes(x = Year, y = 1.155 * Population/1e5)
  ) +
  theme_bw() +
  labs(
    y = "Projected Birmingham Registered\n Population Aged 65+ (100,000)",
    fill = "",
    color = "",
    x = ""
  ) +
  theme(
    legend.position = "top"
  ) +
  ylim(0, 3.5) + 
  xlim(2024, 2050) +
  scale_color_manual(
    breaks=c('No Migration', 'With Migration', 'Scaled ONS Projection*'),
    values = c("purple", "darkgreen", "black")
  ) +
  annotate(
    "text",
    x = 2024,
    y = 3.3,
    size = 2,
    hjust = "left",
    label  = "*Scaled to account for difference between registered and resident populations"
  )

proj_plot_comb
ggsave("output/projection_65plus.png",
       proj_plot_comb, width = 5, height = 3.2)


## Save data
proj_output <- 
  list(
    "BCC Projection" = older_adult_pop %>% arrange(Type, Year),
    "ONS Projection" = ONS_projection
  )

writexl::write_xlsx(proj_output, "output/OA-projection.xlsx")