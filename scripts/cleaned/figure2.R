# Power law Waning 
source("scripts/cleaned/power_law.R")

library(librarian)
shelf(lme4, 
      tidyverse, 
      patchwork,
      boot)

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