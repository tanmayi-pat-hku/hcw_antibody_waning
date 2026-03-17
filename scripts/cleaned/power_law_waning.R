# Power law Waning 
library(librarian)
shelf(lme4, 
      tidyverse, 
      patchwork,
      boot)

source("scripts/cleaned/source2.R")
source("scripts/helper/fc_power_law_bootstrap.R")

# Formula: Power law mixed effects model
  #log_weight ~ log_days * permutation + (1 | id)

# Coefficient = power law exponent (waning rate)
  # Negative value = antibodies decrease over time

#Random Intercept 
  # Different intercepts (starting levels) for each regimen
  # Random intercept for each participant [(1 | id)]

#Random Slope 
  # different waning rate for each regimen [log_days * permutation]

# Model results
after_dose2_results <- fit_powerlaw_with_boot(three_dose_waning_svnt, n_boot = 500)
after_dose3_results <- fit_powerlaw_with_boot(four_dose_waning_svnt, n_boot = 500)


## Display results
power_law_results <- bind_rows(
  after_dose2_results$waning_rates %>% mutate(Dose = "After dose 2"),
  after_dose3_results$waning_rates %>% mutate(Dose = "After dose 3")
) %>%
  mutate(
    `Waning Rate` = round(waning_rate, 2),
    `95% CI` = paste0("[", round(lower_ci, 2), ", ", round(upper_ci, 2), "]"),
    `% at 30 days` = round(100 * (30 ^ waning_rate), 1),
    `% at 90 days` = round(100 * (90 ^ waning_rate), 1),
    `% at 180 days` = round(100 * (180 ^ waning_rate), 1),
    `% at 365 days` = round(100 * (365 ^ waning_rate), 1),
    `Half-life (days)` = round(0.5^(1/waning_rate), 0)
  ) %>%
  select(Dose, permutation, `Waning Rate`, `95% CI`, 
         `% at 30 days`, `% at 90 days`, `% at 180 days`, `% at 365 days`, `Half-life (days)`)

print(power_law_results)

write.csv(power_law_results, "power_law_results.csv", row.names = FALSE)


# Make Plots

# After dose 2 plot
three_dose_powerlaw_plot <- ggplot() +
  geom_point(data = three_dose_waning_svnt, 
             aes(x = days_since_dose1, y = weight, color = permutation),
             size = 3.5, alpha = 0.5) +
  geom_ribbon(data = after_dose2_results$predictions,
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = permutation, group = permutation),
              alpha = 0.2, show.legend = FALSE) +
  geom_line(data = after_dose2_results$predictions, 
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
       title = paste0("After the second dose")) +
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
  geom_ribbon(data = after_dose3_results$predictions %>% filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1")),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = permutation, group = permutation),
              alpha = 0.2, show.legend = FALSE) +
  geom_line(data = after_dose3_results$predictions %>% filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1")),
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
       title = paste0("After the third dose")) +
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

figure_2_powerlaw <- (three_dose_powerlaw_plot + four_dose_powerlaw_plot) +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = "A")

print(figure_2_powerlaw)

ggsave("figure_2_powerlaw.pdf", plot = figure_2_powerlaw, width = 15, height = 10)