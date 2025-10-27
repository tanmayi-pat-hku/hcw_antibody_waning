
# Source Code 
source("scripts/cleaned/source2.R")

#Three Dose sVNT Plot
three_dose_waning_svnt_plot <- ggplot(three_dose_waning_svnt, aes(x = days_since_dose1, y = weight, color = as.factor(dose3_brand), shape = ifelse(permutation %in% c("4-4-4", "1-1-1"), "circle", "triangle"))) +
  geom_point() +  
  #geom_smooth(aes(group = dose3_brand), method = "lm", se = TRUE) + 
  scale_color_manual(name = "Vaccine Type", 
                     values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
                     labels = c("1" = "B", "4" = "S")) + 
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) + 
  labs(x = NULL, y = "WT sVNT Inhibition (%)", title = "After the second dose") +  
  theme_minimal() +  # Use a minimal theme
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
four_dose_waning_svnt_plot <- ggplot(four_dose_waning_svnt, aes(x = days_since_dose1, y = weight, color = as.factor(dose4_brand), shape = ifelse(permutation %in% c("4-4-4-4", "1-1-1-1"), "circle", "triangle"))) +
  geom_point() +  
  #geom_smooth(aes(group = dose4_brand), method = "lm", se = TRUE) + 
  scale_color_manual(name = "Vaccine Type", 
                     values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
                     labels = c("1" = "B", "4" = "S")) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(x = NULL, y = "WT sVNT Inhibition (%)", title = "After the third dose") +
  theme_minimal() +  
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),  
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
    legend.key.size = unit(0.4, "cm"),  
    legend.key = element_rect(fill = "white", color = "transparent")
  ) + guides(shape = "none")


#SVNT PANELS
svnt_wt_combined_waning_plot <- (three_dose_waning_svnt_plot + four_dose_waning_svnt_plot) + 
  plot_layout(widths = c(2.0, 2.0, 2.0), ncol = 2) + plot_annotation(title = "WT sVNT", 
                                                                     theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) +
  theme(plot.background = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.border = element_blank(), 
        plot.margin = margin(0, 0, 0, 0))

print(svnt_wt_combined_waning_plot)

ggsave("svnt_wt_combined_waning_plot.pdf", plot = svnt_wt_combined_waning_plot, width = 15, height = 10)