
#Set Working Directory for Stand alone (For Wey Wen Dropbox)
#setwd("~/Desktop/Shared COVID HCW antibody waning/2025_10_hcw_abwaning")


# Power law Waning 
source("scripts/cleaned/source2.R")
#source("output/scripts/cleaned/source2.R") #For Wey Wen Dropbox
source("scripts/cleaned/power_law.R")
#source("output/scripts/cleaned/power_law.R") #For Wey Wen Dropbox

# Load Packages
library(librarian)
shelf(lme4, 
      tidyverse, 
      patchwork,
      boot)

# Label positions for panel A (after dose 2)
labels_dose2 <- data.frame(
  permutation = c("1-1", "4-4"),
  x = c(450, 450),              
  y = c(58, 18),                 
  label = c("Homologous, mRNA", "Homologous, inactivated"),
  color = c("#00468B", "#AD002A")
)

# Label positions for panel B (after dose 3)
labels_dose3 <- data.frame(
  permutation = c("1-1-1", "4-4-4", "4-4-1"),
  x = c(450, 450, 445),        
  y = c(92, 42, 95),          
  label = c("Homologous, mRNA", "Homologous, inactivated", "Heterologous"),
  color = c("#00468B", "#AD002A", "#925E9F")
)

# MAKE PLOTS
# After dose 2 plot
two_dose_powerlaw_summary <- ggplot() +
  geom_line(
    data = dose2_marginal %>% filter(days_since_dose1 >= 0, days_since_dose1 <= 360),
    aes(x = days_since_dose1, y = pred_weight, color = permutation, group = permutation),
    linewidth = 1.2, show.legend = FALSE
  ) +
  geom_point(
    data = two_dose_waning_svnt %>% filter(days_since_dose1 >= 0, days_since_dose1 <= 360),
    aes(x = days_since_dose1, y = weight, color = permutation),
    size = 3.5, alpha = 0.35
  ) +
  geom_text(
    data = labels_dose2,
    aes(x = x, y = y, label = label),
    color = labels_dose2$color,
    hjust = 1, fontface = "bold", size = 3.5,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Vaccination history",
    values = c("1-1" = "#00468B", "4-4" = "#AD002A"),
    labels = c("1-1" = "Homologous, mRNA", "4-4" = "Homologous, inactivated")
  ) +
  scale_fill_manual(
    values = c("1-1" = "#00468B", "4-4" = "#AD002A")
  ) +
  scale_x_continuous(
    breaks = c(0, seq(30, 360, by = 30)),
    labels = c("0", 1:12),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    expand = c(0, 0)
  ) +
  coord_cartesian(xlim = c(0, 360), ylim = c(0, 100), clip = "off") +
  labs(
    x = "Months since vaccination",
    y = "WT sVNT Inhibition (%)",
    title = "After the second dose"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "plain", size = 13, color = "black"),
    axis.text.y = element_text(face = "plain", size = 13, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 100, b = 10, l = 10),  # enough white space on right
    axis.line = element_line(linewidth = 0.5, color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

# After dose 3 plot
three_dose_powerlaw_plot_summary <- ggplot() +
  geom_point(
    data = three_dose_waning_svnt %>% filter(days_since_dose1 >= 0, days_since_dose1 <= 360),
    aes(x = days_since_dose1, y = weight, color = permutation),
    size = 3.5, alpha = 0.35
  ) +
  geom_line(
    data = dose3_marginal %>% filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1"),
                                     days_since_dose1 >= 0, days_since_dose1 <= 360),
    aes(x = days_since_dose1, y = pred_weight, color = permutation, group = permutation),
    linewidth = 1.2, show.legend = FALSE
  ) +
  geom_text(
    data = labels_dose3,
    aes(x = x, y = y, label = label),
    color = labels_dose3$color,
    hjust = 1, fontface = "bold", size = 3.5,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Vaccination history",
    values = c("1-1-1" = "#00468B", "4-4-4" = "#AD002A", "4-4-1" = "#925E9F"),
    labels = c("1-1-1" = "Homologous, mRNA", "4-4-4" = "Homologous, inactivated", "4-4-1" = "Heterologous")
  ) +
  scale_fill_manual(
    values = c("1-1-1" = "#00468B", "4-4-4" = "#AD002A", "4-4-1" = "#925E9F")
  ) +
  scale_x_continuous(
    breaks = c(0, seq(30, 360, by = 30)),
    labels = c("0", 1:12),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    expand = c(0, 0)
  ) +
  coord_cartesian(xlim = c(0, 360), ylim = c(0, 100), clip = "off") +
  labs(
    x = "Months since vaccination",
    y = "WT sVNT Inhibition (%)",
    title = "After the third dose"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 15),
    axis.text.x = element_text(face = "plain", size = 13, color = "black"),
    axis.text.y = element_text(face = "plain", size = 13, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 10, r = 100, b = 10, l = 10),
    axis.line = element_line(linewidth = 0.5, color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

# Combine plots
figure_2_powerlaw_summary <- (two_dose_powerlaw_summary + three_dose_powerlaw_plot_summary) +
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = "A")

print(figure_2_powerlaw_summary)

# SAVE RESULTS
ggsave("figure_2_powerlaw_summary.pdf", plot = figure_2_powerlaw_summary, width = 15, height = 10)
