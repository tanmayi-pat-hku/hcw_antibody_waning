
# Source Code 
source("scripts/cleaned/source2.R")

#Three Dose sVNT Plot
three_dose_waning_svnt_plot <- ggplot(three_dose_waning_svnt, aes(x = days_since_dose1, y = weight, 
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
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  scale_x_continuous(limits = c(0, 420), breaks = seq(0, 420, by = 30)) +
  labs(x = "Days", y = "WT sVNT Inhibition (%)", title = "After the second dose") +  
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
  ) #+
  #coord_cartesian(xlim = c(0, max(three_dose_waning_svnt$days_since_dose1)), ylim = c(0, 100))  

##Four Dose SVNT Plot 
four_dose_waning_svnt_plot <- ggplot(four_dose_waning_svnt, aes(x = days_since_dose1, y = weight, 
                                                                color = as.factor(dose4_brand), 
                                                                shape = ifelse(permutation %in% c("4-4-4-4", "1-1-1-1"), "1", "4"))) +
  geom_point(size = 5, alpha = 0.5) +  
  geom_smooth(aes(group = interaction(dose4_brand, type), linetype = type), method = "lm", se = TRUE) + 
  scale_color_manual(
    name = "Vaccine Type", 
    values = c("1" = "#1F77B4", "4" = "#FF7F0E"), 
    labels = c("1" = "B", "4" = "S")
  ) + 
  scale_linetype_manual(
    name = "Type", 
    values = c("homologous" = "solid", "heterologous" = "dashed"),
    labels = c("homologous" = "Homologous", "heterologous" = "Heterologous")
  ) + 
  scale_shape_manual(
    name = "Last Dose Type", 
    values = c("1" = 16, "4" = 17),  
    labels = c("1" = "Homologous", "4" = "Heterologous")
  ) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  scale_x_continuous(limits = c(0, 420), breaks = seq(0, 420, by = 30)) +
  labs(x = "Days", y = "WT sVNT Inhibition (%)", title = "After the third dose") +  
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
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid"),
    # Legend styling
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.size = unit(1, "lines"),
    legend.spacing = unit(0.5, "cm"),
    legend.margin = margin(0, 0, 0, 0)
  )  + guides(linetype = guide_legend(override.aes = list(linetype = c("dotted", "solid"))))
#+coord_cartesian(xlim = c(0, max(four_dose_waning_svnt$days_since_dose1)), ylim = c(0, 100))

#SVNT PANELS
svnt_wt_combined_waning_plot <- (three_dose_waning_svnt_plot + four_dose_waning_svnt_plot) + 
  plot_layout(widths = c(2.0, 2.0, 2.0), ncol = 2) + plot_annotation(title = "WT sVNT", 
                                                                     theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) +
  theme(plot.background = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.border = element_blank(), 
        plot.margin = margin(0, 0, 0, 0)) +
  plot_annotation(tag_levels = 'A') 

print(svnt_wt_combined_waning_plot)

#Save 
ggsave("figure_2.pdf", plot = svnt_wt_combined_waning_plot, width = 15, height = 10)