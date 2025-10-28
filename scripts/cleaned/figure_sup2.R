# Time of Booster Delivery: sVNT


# Source Code 
source("scripts/cleaned/source3.R")


#sVNT 
dose_2_3_svnt_plot <- ggplot(dose_2_3_svnt, aes(x = days_between_doses, y = v3_svnt_wt, color = factor(dose3_brand))) +
  geom_point() + 
  #geom_smooth(aes(group = factor(dose3_brand)), method = "lm", se = TRUE) +  # Add separate linear trendlines for each brand
  scale_color_manual(name = "Vaccine Type", 
                     values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
                     labels = c("1" = "B", "4" = "S")) +
  scale_y_continuous(limits = c(90, 100), breaks = seq(90, 100, by = 1)) +
  labs(x = "Days Between Doses", y = "WT sVNT Inhibition (%)", title = "Third Dose Date") +  # Axis labels
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
    legend.position = "none",  
    #legend.justification = c("right", "top"),  
    #legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # White background with border
    #legend.margin = margin(6, 6, 6, 6), 
    #legend.title = element_text(face = "bold", size = 10), 
    #legend.text = element_text(size = 8),  
    #legend.key.size = unit(0.2, "cm"),  
    #legend.key = element_rect(fill = "white", color = "transparent")
  )


#sVNT 
dose_3_4_svnt_plot <- ggplot(dose_3_4_svnt, aes(x = days_between_doses, y = v4_svnt_wt, color = factor(dose4_brand))) +
  geom_point() + 
  #geom_smooth(aes(group = factor(dose4_brand)), method = "lm", se = TRUE) +  # Add separate linear trendlines for each brand
  scale_color_manual(name = "Vaccine Type", 
                     values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
                     labels = c("1" = "B", "4" = "S")) +
  scale_y_continuous(limits = c(90, 100), breaks = seq(90, 100, by = 1)) +
  labs(x = "Days Between Doses", y = "WT sVNT Inhibition (%)", title = "Fourth Dose Date") +  # Axis labels
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


#SVNT PANELS
svnt_wt_between_doses_plot <- (dose_2_3_svnt_plot + dose_3_4_svnt_plot) + 
  plot_layout(widths = c(2.0, 2.0, 2.0), ncol = 2) + plot_annotation(title = "WT sVNT", 
                                                                     theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) +
  theme(plot.background = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.border = element_blank(), 
        plot.margin = margin(0, 0, 0, 0))

print(svnt_wt_between_doses_plot)

ggsave("svnt_wt_between_doses_plot.pdf", plot = svnt_wt_between_doses_plot, width = 15, height = 10)