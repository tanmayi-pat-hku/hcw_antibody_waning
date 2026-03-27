library(librarian)
shelf(lme4, 
      tidyverse, 
      patchwork,
      boot)

# Get age-specific predictions
dose2_age <- after_dose2_adj$predictions %>%
  group_by(days_since_dose1, permutation, age_bin) %>%
  summarise(
    pred_weight = mean(pred_weight, na.rm = TRUE),
    lower = mean(lower, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    age_group = case_when(
      age_bin == "Younger Adults (21-35)" ~ "21-35",
      age_bin == "Adults (35-55)" ~ "35-55",
      age_bin == "Older Adults (55+)" ~ "55+"
    ),
    age_group = factor(age_group, levels = c("21-35", "35-55", "55+"))
  )

dose3_age <- after_dose3_adj$predictions %>%
  group_by(days_since_dose1, permutation, age_bin) %>%
  summarise(
    pred_weight = mean(pred_weight, na.rm = TRUE),
    lower = mean(lower, na.rm = TRUE),
    upper = mean(upper, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    age_group = case_when(
      age_bin == "Younger Adults (21-35)" ~ "21-35",
      age_bin == "Adults (35-55)" ~ "35-55",
      age_bin == "Older Adults (55+)" ~ "55+"
    ),
    age_group = factor(age_group, levels = c("21-35", "35-55", "55+"))
  )

# Define colors for age groups
age_colors <- c("21-35" = "#2CA02C", "35-55" = "#FF7F0E", "55+" = "#D62728")

# After dose 2 plot - faceted by vaccine regimen
three_dose_powerlaw_age_plot <- ggplot() +
  geom_point(data = three_dose_waning_svnt, 
             aes(x = days_since_dose1, y = weight),
             color = "gray70", size = 1.5, alpha = 0.3) +
  geom_ribbon(data = dose2_age %>% filter(permutation %in% c("1-1", "4-4")),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = age_group, group = interaction(permutation, age_bin)),
              alpha = 0.15) +
  geom_line(data = dose2_age %>% filter(permutation %in% c("1-1", "4-4")),
            aes(x = days_since_dose1, y = pred_weight, 
                color = age_group, group = interaction(permutation, age_bin)),
            linewidth = 1.2) +
  facet_wrap(~ permutation, ncol = 2,
             labeller = labeller(permutation = c("1-1" = "B-B", "4-4" = "S-S"))) +
  scale_color_manual(name = "Age Group", values = age_colors) +
  scale_fill_manual(name = "Age Group", values = age_colors) +
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 60)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "Days since dose 2", y = "WT sVNT Inhibition (%)", 
       title = "After the second dose") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.line = element_line(size = 0.5, color = "black"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "lightgray", color = NA)
  )

# After dose 3 plot - faceted by vaccine regimen
four_dose_powerlaw_age_plot <- ggplot() +
  geom_point(data = four_dose_waning_svnt, 
             aes(x = days_since_dose1, y = weight),
             color = "gray70", size = 1.5, alpha = 0.3) +
  geom_ribbon(data = dose3_age %>% filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1")),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = age_group, group = interaction(permutation, age_bin)),
              alpha = 0.15) +
  geom_line(data = dose3_age %>% filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1")),
            aes(x = days_since_dose1, y = pred_weight, 
                color = age_group, group = interaction(permutation, age_bin)),
            linewidth = 1.2) +
  facet_wrap(~ permutation, ncol = 3,
             labeller = labeller(permutation = c("1-1-1" = "B-B-B", 
                                                 "4-4-4" = "S-S-S", 
                                                 "4-4-1" = "S-S-B"))) +
  scale_color_manual(name = "Age Group", values = age_colors) +
  scale_fill_manual(name = "Age Group", values = age_colors) +
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 60)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "Days since dose 3", y = "WT sVNT Inhibition (%)", 
       title = "After the third dose") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold", size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.line = element_line(size = 0.5, color = "black"),
    strip.text = element_text(face = "bold", size = 12),
    strip.background = element_rect(fill = "lightgray", color = NA)
  )

# Combine plots
figure_2_powerlaw_age <- (three_dose_powerlaw_age_plot / four_dose_powerlaw_age_plot) +
  plot_layout(ncol = 1) + 
  plot_annotation(tag_levels = "A")

print(figure_2_powerlaw_age)
ggsave("figure_2_powerlaw_age.pdf", plot = figure_2_powerlaw_age, width = 12, height = 12)