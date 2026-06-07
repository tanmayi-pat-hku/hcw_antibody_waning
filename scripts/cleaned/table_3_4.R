#Set Working Directory (For Wey Wen Dropbox)
#setwd("~/Desktop/Shared COVID HCW antibody waning/2025_10_hcw_abwaning")

# Source Functions
source("scripts/cleaned/figure2.R")
#source("output/scripts/cleaned/figure2.R") #For Wey Wen Dropbox
source("scripts/cleaned/power_law.R") 
#source("output/scripts/cleaned/power_law.R") #For Wey Wen Dropbox
source("scripts/helper/fc_power_law_bootstrap.R") 
#source("output/scripts/helper/fc_power_law_bootstrap.R") #For Wey Wen Dropbox

# Table 3 

# Get sample sizes
sample_sizes <- bind_rows(
  two_dose_waning_svnt %>% 
    group_by(permutation) %>% 
    summarise(n = n_distinct(id)) %>%
    mutate(Dose = "After dose 2"),
  three_dose_waning_svnt %>% 
    group_by(permutation) %>% 
    summarise(n = n_distinct(id)) %>%
    mutate(Dose = "After dose 3")
)

# TABLE: WANING RATES BY VACCINE SERIES

marginal_rates_dose2 <- get_marginal_waning(after_dose2_adj, n_boot = 500)
marginal_rates_dose3 <- get_marginal_waning(after_dose3_adj, n_boot = 500)


waning_rates_table <- bind_rows(
  marginal_rates_dose2 %>% mutate(Dose = "After dose 2"),
  marginal_rates_dose3 %>% mutate(Dose = "After dose 3")
) %>%
  left_join(sample_sizes, by = c("Dose", "permutation")) %>%
  mutate(
    `Group (n)` = paste0(permutation, " (n=", n, ")"),
    `Waning Rate` = round(waning_rate, 2),
    `95% CI` = paste0("[", round(lower_ci, 2), ", ", round(upper_ci, 2), "]"),
    `% at 90 days` = round(100 * (90 ^ waning_rate), 1),
    `% at 180 days` = round(100 * (180 ^ waning_rate), 1),
    `Days to 50% of day 1` = ifelse(waning_rate > -0.05, "Stable (>500 days)",
                                    as.character(round(0.5^(1/waning_rate), 0))),
    `Significant (vs reference)` = significant
  ) %>%
  select(Dose, `Group (n)`, `Waning Rate`, `95% CI`, 
         `% at 90 days`, `% at 180 days`, `Days to 50% of day 1`, 
         `Significant (vs reference)`)

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