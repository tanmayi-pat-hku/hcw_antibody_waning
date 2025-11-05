#Waning Analysis: ELISA

# Source Code 
source("scripts/cleaned/source2.R")


three_dose_waning_elisa_plot <- ggplot(three_dose_waning_elisa, aes(x = days_since_dose1, y = weight, 
                                                                    color = as.factor(dose3_brand), 
                                                                    shape = ifelse(permutation %in% c("4-4-4", "1-1-1"), "1", "4"))) +
  geom_point(size = 5, alpha = 0.5) +  
  geom_smooth(aes(group = interaction(dose3_brand, type), linetype = type), method = "lm", se = TRUE) + 
  
  scale_color_manual(name = "Vaccine Type", 
                     values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
                     labels = c("1" = "B", "4" = "S")) + 
  scale_linetype_manual(name = "Type", 
                        values = c("homologous" = "solid", "heterologous" = "dashed"),
                        labels = c("homologous" = "Homologous", "heterologous" = "Heterologous")) + 
  scale_shape_manual(name = "Dose 3 Brand", 
                     values = c("1" = 16, "4" = 17),  
                     labels = c("1" = "Homologous", "4" = "Heterologous")) + 
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) + 
  labs(x = NULL, y = "WT RBD ELISA (OD value)", title = "After the second dose") +  
  theme_minimal() +  # Use a minimal theme
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  # Center the title
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks = element_line(),
    legend.position = "right",  # Show the legend
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  )

print(three_dose_waning_elisa_plot)

ggsave("figure2a_sup.pdf", plot = three_dose_waning_elisa_plot, width = 15, height = 8)


# Low Sample Size for After the Third Dose 

##Four Dose ELISA Plot 
four_dose_waning_elisa_plot <- ggplot(four_dose_waning_elisa, aes(x = days_since_dose1, y = weight, color = as.factor(dose4_brand), shape = ifelse(permutation %in% c("4-4-4-4", "1-1-1-1"), "1", "4"))) +
  geom_point(size = 5, alpha = 0.5) + 
  geom_smooth(aes(group = interaction(dose4_brand, type), linetype = type), method = "lm", se = TRUE) +  
  scale_color_manual(name = "Vaccine Type", values = c("1" = "#1F77B4", "4" = "#FF7F0E"), labels = c("1" = "B", "4" = "S")) +
  scale_linetype_manual(name = "Type", 
                        values = c("homologous" = "solid", "heterologous" = "dashed"),
                        labels = c("homologous" = "Homologous", "heterologous" = "Heterologous")) + 
  scale_shape_manual(name = "Dose 4 Brand", 
                     values = c("1" = 16, "4" = 17),  
                     labels = c("1" = "Homologous", "4" = "Heterologous")) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +  # Y-axis limits and breaks
  labs(x = NULL, y = "WT RBD ELISA (OD value)", title = "After the third dose") +  # Axis labels
  theme_minimal() + 
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  # Center the title
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks = element_line(),
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid"),
    legend.position = c(0.98, 0.98),  # Top-right corner
    legend.justification = c("right", "top"),  # Anchor legend to top-right
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # White background with border
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 8),  
    legend.key.size = unit(0.2, "cm"),  # Smaller legend keys for a compact look
    legend.key = element_rect(fill = "white", color = "transparent")
  ) + guides(shape = "none")


#ELISA PANELS
elisa_wt_combined_waning_plot <- (three_dose_waning_elisa_plot + four_dose_waning_elisa_plot) + 
  plot_layout(widths = c(2.0, 2.0, 2.0), ncol = 2) + plot_annotation(title = "WT RBD ELISA", 
                                                                     theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) +
  theme(plot.background = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.border = element_blank(), 
        plot.margin = margin(0, 0, 0, 0))

print(elisa_wt_combined_waning_plot)