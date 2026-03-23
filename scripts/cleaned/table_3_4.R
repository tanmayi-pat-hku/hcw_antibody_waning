source("scripts/cleaned/figure2.R")
source("scripts/cleaned/power_law.R")

# Table 3 

# Get sample sizes
sample_sizes <- bind_rows(
  three_dose_waning_svnt %>% 
    group_by(permutation) %>% 
    summarise(n = n_distinct(id)) %>%
    mutate(Dose = "After dose 2"),
  four_dose_waning_svnt %>% 
    group_by(permutation) %>% 
    summarise(n = n_distinct(id)) %>%
    mutate(Dose = "After dose 3")
)

# TABLE: WANING RATES BY VACCINE SERIES
waning_rates_table <- bind_rows(
  after_dose2_adj$waning_rates %>% mutate(Dose = "After dose 2"),
  after_dose3_adj$waning_rates %>% mutate(Dose = "After dose 3")
) %>%
  group_by(Dose, permutation) %>%
  summarise(
    waning_rate = mean(waning_rate),
    lower_ci = mean(lower_ci),
    upper_ci = mean(upper_ci),
    .groups = 'drop'
  ) %>%
  left_join(sample_sizes, by = c("Dose", "permutation")) %>%
  mutate(
    `Group (n)` = paste0(permutation, " (n=", n, ")"),
    `Waning Rate` = round(waning_rate, 2),
    `95% CI` = paste0("[", round(lower_ci, 2), ", ", round(upper_ci, 2), "]"),
    `% at 90 days` = round(100 * (90 ^ waning_rate), 1),
    `% at 180 days` = round(100 * (180 ^ waning_rate), 1),
    `Half-life (days)` = case_when(
      waning_rate > -0.05 ~ "Stable",
      TRUE ~ as.character(round(0.5^(1/waning_rate), 0))
    )
  ) %>%
  group_by(Dose) %>%
  mutate(
    ref_lower = first(lower_ci),
    ref_upper = first(upper_ci),
    `Significant` = case_when(
      permutation == first(permutation) ~ "Ref",
      upper_ci < ref_lower ~ "Yes",
      lower_ci > ref_upper ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  ungroup() %>%
  select(Dose, `Group (n)`, `Waning Rate`, `95% CI`, 
         `% at 90 days`, `% at 180 days`, `Half-life (days)`, `Significant`)

# DEMOGRAPHIC EFFECTS

# Table: Interaction effects (does waning differ by demographic)
interaction_effects <- bind_rows(
  after_dose2_adj$interaction_effects %>% mutate(Dose = "After dose 2"),
  after_dose3_adj$interaction_effects %>% mutate(Dose = "After dose 3")
) %>%
  mutate(
    Variable = case_when(
      term == "log_days:male1" ~ "Sex (Male vs Female)",
      term == "log_days:age_binAdults (40-65)" ~ "Age (40-65 vs 21-40)",
      term == "log_days:age_binOlder Adults (65+)" ~ "Age (65+ vs 21-40)",
      TRUE ~ term
    ),
    `Difference in Waning` = round(estimate, 3),
    `95% CI` = paste0("[", round(lower_ci, 3), ", ", round(upper_ci, 3), "]"),
    `Significant` = ifelse(significant, "Yes", "No")
  ) %>%
  select(Dose, Variable, `Difference in Waning`, `95% CI`, `Significant`)

# Save table file 
write.csv(waning_rates_table, "table3_waning_rates.csv", row.names = FALSE)
write.csv(interaction_effects, "table4_waning_interactions.csv", row.names = FALSE)