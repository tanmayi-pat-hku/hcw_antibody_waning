# Time of Booster Delivery: ELISA

# Source Code 
source("scripts/cleaned/source3.R")


# Between Dose 2 - 3 Plot 
#ELISA
dose_2_3_elisa_plot <- ggplot(dose_2_3_elisa, aes(x = days_between_doses, y = v3_elisa_wt, color = factor(dose3_brand))) +
  geom_point() + 
  #geom_smooth(aes(group = factor(dose3_brand)), method = "lm", se = TRUE) +  # Add separate linear trendlines for each brand
  scale_color_manual(name = "Vaccine Type", 
                     values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
                     labels = c("1" = "B", "4" = "S")) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +  # Y-axis limits and breaks
  labs(x = "Days Between Doses", y = "WT RBD ELISA (OD value)", title = "Third Dose Date") +  # Axis labels
  theme_minimal() + 
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  # Center the title
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(), 
    axis.ticks = element_line(),
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid"),
    legend.position = c(0.98, 0.98),  
    legend.justification = c("right", "top"),  
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # White background with border
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 8),  
    legend.key.size = unit(0.2, "cm"),  
    legend.key = element_rect(fill = "white", color = "transparent")
  )

dose_2_3_elisa_plot

ggsave("dose_2_3_elisa_plot.pdf", plot = dose_2_3_elisa_plot, width = 15, height = 10)