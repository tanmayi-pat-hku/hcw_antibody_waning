# Source Code 
source("scripts/cleaned/source2.R")

library(librarian)
shelf(ggnewscale)
shelf(dplyr)

##Three Dose sVNT Plot
three_dose_waning_svnt_plot <- ggplot(three_dose_waning_svnt, aes(x = days_since_dose1, y = weight, color = permutation)) +
  geom_point(size = 3.5, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, aes(group = permutation), show.legend = FALSE) +
  scale_color_manual(
    name = "Vaccine Series",
    values = c("1-1" = "#1F77B4", "4-4" = "#FF7F0E"),
    labels = c("1-1" = "B-B", "4-4" = "S-S")
  ) +
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 30)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "Days since dose 2", y = "WT sVNT Inhibition (%)", title = "After the second dose") +
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

##Four Dose sVNT Plot 
four_dose_waning_svnt_plot <- ggplot(four_dose_waning_svnt, aes(x = days_since_dose1, y = weight, color = permutation)) +
  geom_point(size = 3.5, alpha = 0.5) +
  geom_smooth(data = four_dose_waning_svnt %>% filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1")),
              method = "lm", se = TRUE, linewidth = 1.2, aes(group = permutation), show.legend = FALSE) +
  scale_color_manual(
    name = "Vaccine Series",
    values = c("4-4-4" = "#FF7F0E", "1-1-1" = "#1F77B4", "4-4-1" = "darkgreen"),
    labels = c("4-4-4" = "S-S-S", "1-1-1" = "B-B-B", "4-4-1" = "S-S-B")
  ) +
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 30)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "Days since dose 3", y = "WT sVNT Inhibition (%)", title = "After the third dose") +
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

figure_2 <- (
  (three_dose_waning_svnt_plot + four_dose_waning_svnt_plot) +
    plot_layout(ncol = 2)) + plot_annotation(tag_levels = "A")
print(figure_2)

#Save 
ggsave("figure_2.pdf", plot = figure_2, width = 15, height = 10)



