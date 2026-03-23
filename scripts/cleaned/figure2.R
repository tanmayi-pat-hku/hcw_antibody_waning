# Power law Waning 

source("scripts/cleaned/source2.R")
source("scripts/helper/fc_power_law_bootstrap.R")

library(librarian)
shelf(lme4, 
      tidyverse, 
      patchwork,
      boot)

# Formula: 
#   log_weight ~ log_days * permutation + male + age_bin + 
#               log_days:male + log_days:age_bin + (1 | id)

# Coefficient = power law exponent (waning rate)
#   Negative value = antibodies decrease over time
#   The exponent tells you HOW FAST antibodies decline (more negative = faster waning)

# Fixed Effects Structure:
#   1. log_days:                    Base waning rate (reference group: B-B, female, age 21-40)
#   2. log_days:permutation:        Allows waning rate to differ by vaccine series
#   3. male + age_bin:              Baseline differences by sex and age group
#   4. log_days:male:               Tests if waning rate differs by sex
#   5. log_days:age_bin:            Tests if waning rate differs by age group

# Random Effects:
#   (1 | id): Random intercept for each participant
#     - Accounts for repeated measurements within individuals
#     - Different starting antibody levels for each person
#     - Captures individual variation beyond fixed effects


# Run adjusted models with interactions
after_dose2_adj <- fit_powerlaw_with_boot_adj(three_dose_waning_svnt, n_boot = 500)
after_dose3_adj <- fit_powerlaw_with_boot_adj(four_dose_waning_svnt, n_boot = 500)


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

# TABLE 1: WANING RATES BY VACCINE SERIES
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

# TABLE 2: DEMOGRAPHIC EFFECTS

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


# CREATE MARGINAL PREDICTIONS FOR PLOTS across age bins and sex 
dose2_marginal <- create_marginal_predictions(after_dose2_adj)
dose3_marginal <- create_marginal_predictions(after_dose3_adj)

# MAKE PLOTS
# After dose 2 plot
three_dose_powerlaw_plot <- ggplot() +
  geom_point(data = three_dose_waning_svnt, 
             aes(x = days_since_dose1, y = weight, color = permutation),
             size = 3.5, alpha = 0.5) +
  geom_ribbon(data = dose2_marginal,
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = permutation, group = permutation),
              alpha = 0.2, show.legend = FALSE) +
  geom_line(data = dose2_marginal, 
            aes(x = days_since_dose1, y = pred_weight, color = permutation, group = permutation),
            linewidth = 1.2, show.legend = FALSE) +
  scale_color_manual(
    name = "Vaccine Series",
    values = c("1-1" = "#1F77B4", "4-4" = "#FF7F0E"),
    labels = c("1-1" = "B-B", "4-4" = "S-S")
  ) +
  scale_fill_manual(
    values = c("1-1" = "#1F77B4", "4-4" = "#FF7F0E")
  ) +
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 30)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "Days since dose 2", y = "WT sVNT Inhibition (%)", 
       title = "After the second dose") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.line = element_line(size = 0.5, color = "black")
  )

# After dose 3 plot
four_dose_powerlaw_plot <- ggplot() +
  geom_point(data = four_dose_waning_svnt, 
             aes(x = days_since_dose1, y = weight, color = permutation),
             size = 3.5, alpha = 0.5) +
  geom_ribbon(data = dose3_marginal %>% filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1")),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = permutation, group = permutation),
              alpha = 0.2, show.legend = FALSE) +
  geom_line(data = dose3_marginal %>% filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1")),
            aes(x = days_since_dose1, y = pred_weight, color = permutation, group = permutation),
            linewidth = 1.2, show.legend = FALSE) +
  scale_color_manual(
    name = "Vaccine Series",
    values = c("4-4-4" = "#FF7F0E", "1-1-1" = "#1F77B4", "4-4-1" = "darkgreen"),
    labels = c("4-4-4" = "S-S-S", "1-1-1" = "B-B-B", "4-4-1" = "S-S-B")
  ) +
  scale_fill_manual(
    values = c("4-4-4" = "#FF7F0E", "1-1-1" = "#1F77B4", "4-4-1" = "darkgreen")
  ) +
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 30)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "Days since dose 3", y = "WT sVNT Inhibition (%)", 
       title = "After the third dose") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.line = element_line(size = 0.5, color = "black")
  )

# Combine plots
figure_2_powerlaw <- (three_dose_powerlaw_plot + four_dose_powerlaw_plot) +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = "A")

print(figure_2_powerlaw)

# SAVE RESULTS
ggsave("figure_2_powerlaw.pdf", plot = figure_2_powerlaw, width = 15, height = 10)
write.csv(waning_rates_table, "table1_waning_rates.csv", row.names = FALSE)
write.csv(interaction_effects, "table2b_waning_interactions.csv", row.names = FALSE)