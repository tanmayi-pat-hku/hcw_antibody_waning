# Source Code 
source("scripts/cleaned/source2.R")

library(librarian)
shelf(ggnewscale)
shelf(dplyr)

#Three Dose sVNT Plot
three_dose_waning_svnt_plot <- ggplot(three_dose_waning_svnt, aes(x = days_since_dose1, y = weight)) +
  geom_point(aes(color = as.factor(dose2_brand), 
                 shape = ifelse(permutation %in% c("4-4", "1-1"), "Homologous", "Heterologous")), 
             size = 3.5, alpha = 0.5) +  
  scale_color_manual(
    name = "Last Dose Brand", 
    values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
    labels = c("1" = "B", "4" = "S")
  ) +
  new_scale_color() +
  geom_smooth(aes(color = permutation, 
                  group = permutation,
                  linetype = ifelse(permutation %in% c("4-4", "1-1"), "Homologous", "Heterologous")), 
              method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(
    name = "Vaccine Series",
    values = c("4-4" = "#FF7F0E", 
               "1-1" = "#1F77B4", 
               "4-1" = "#D62728",  
               "1-4" = "#9467BD"), 
    labels = c("4-4" = "S-S", 
               "1-1" = "B-B",
               "4-1" = "S-B", 
               "1-4" = "B-S")
  ) +
  scale_linetype_manual(
    name = "Series Type", 
    values = c("Homologous" = "solid", "Heterologous" = "dashed")
  ) + 
  scale_shape_manual(
    name = "Series Type", 
    values = c("Homologous" = 16, "Heterologous" = 17)
  ) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  scale_x_continuous(limits = c(0, 420), breaks = seq(0, 420, by = 30)) +
  labs(x = "Days Since Dose", y = "WT sVNT Inhibition (%)", 
       title = "After the second dose") +  
  theme_minimal() + 
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(),
    legend.position = "none",
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  )

##Four Dose SVNT Plot 
four_dose_waning_svnt_plot <- ggplot(four_dose_waning_svnt, aes(x = days_since_dose1, y = weight)) +
  geom_point(aes(color = as.factor(dose3_brand), 
                 shape = ifelse(permutation %in% c("4-4-4", "1-1-1"), "Homologous", "Heterologous")), 
             size = 3.5, alpha = 0.5) +  
  scale_color_manual(
    name = "Last Dose Brand", 
    values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
    labels = c("1" = "B", "4" = "S")
  ) +
  new_scale_color() +
  geom_smooth(data = four_dose_waning_svnt %>% filter(permutation %in% c("4-4-4", "1-1-1")),
              aes(color = permutation, group = permutation), 
              method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(
    name = "Vaccine Series",
    values = c("4-4-4" = "#FF7F0E",     
               "1-1-1" = "#1F77B4"),    
    labels = c("4-4-4" = "S-S-S", 
               "1-1-1" = "B-B-B")
  ) +
  scale_linetype_manual(
    name = "Vaccine Series", 
    values = c("Homologous" = "solid", "Heterologous" = "dashed")
  ) + 
  scale_shape_manual(
    name = "Vaccine Series", 
    values = c("Homologous" = 16, "Heterologous" = 17)
  ) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  scale_x_continuous(limits = c(0, 420), breaks = seq(0, 420, by = 30)) +
  labs(x = "Days Since Dose", y = "WT sVNT Inhibition (%)", title = "After the third dose") +  
  theme_minimal() +
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(),
    legend.position = "right",
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  ) + 
  guides(shape = "none", linetype = "none")  

custom_labels <- c("0 - 3", "3 - 6", "6 - 9", "9 - 12", "12 - 15") 

three_dose_facet_box <- ggplot(four_dose_waning_svnt_intervals, 
                               aes(x = time_interval, y = weight)) +
  geom_boxplot(position = position_dodge(width = 0.9), aes(fill = permutation), alpha = 0.7, outlier.shape = NA) + 
  geom_jitter(aes(shape = permutation_type, 
                  color = permutation), 
              width = 0.1, alpha = 0.5, size = 3.5) +
  scale_fill_manual(
    name = "Vaccine Series",
    values = c("1-1-1" = "#1F77B4",     
               "4-4-4" = "#FF7F0E",     
               "4-4-1" = "darkgreen",     
               "1-1-4" = "#9467BD"),    
    labels = c("1-1-1" = "B-B-B", 
               "4-4-4" = "S-S-S",
               "4-4-1" = "S-S-B", 
               "1-1-4" = "B-B-S")
  ) +
  scale_color_manual(
    name = "Vaccine Series",
    values = c("1-1-1" = "#1F77B4",    
               "4-4-4" = "#FF7F0E",     
               "4-4-1" = "darkgreen",    
               "1-1-4" = "#9467BD"),    
    labels = c("1-1-1" = "B-B-B", 
               "4-4-4" = "S-S-S",
               "4-4-1" = "S-S-B", 
               "1-1-4" = "B-B-S")
  ) +
  scale_shape_manual(
    name = "Series Type",
    values = c("Homologous" = 16, "Heterologous" = 17, "Other" = 18)
  ) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_x_discrete(labels = custom_labels) +
  labs(x = "Months Since Third Dose", y = "WT sVNT Inhibition (%)", 
       title = "After the third dose") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(),
    legend.position = "right",  
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid"),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.title = element_text(size = 12, face = "bold")
  ) + 
  facet_wrap(~ permutation_type) +
  guides(
    fill = guide_legend(override.aes = list(shape = 22, size = 5, linetype = 0)),  
    color = guide_legend(override.aes = list(shape = 22, size = 5)),
    shape = guide_legend(override.aes = list(size = 4))
  )

figure_2 <- (
  (three_dose_waning_svnt_plot + plot_spacer() + four_dose_waning_svnt_plot) +
    plot_layout(ncol = 3, widths = c(1, 0, 1))
) /
  (three_dose_facet_box) +
  plot_layout(heights = c(1, 1)) 

print(figure_2)

#Save 
ggsave("figure_2.pdf", plot = figure_2, width = 15, height = 10)