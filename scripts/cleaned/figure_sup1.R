
#Supplementary Figure: Boost Analysis Linked by Dose

# Source Code 
source("scripts/cleaned/source_sup1.R")

#Two Dose Plot ELISA 
elisa_wt_time_plot_2 <- ggplot() +
  geom_rect(
    data = shade_df_2,  # Assuming you have a shading DataFrame
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, alpha = alpha),
    fill = "gray40", inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_alpha_identity() +
  geom_line(
    data = result_2_dose_elisa$long_data,  # Long-format data
    aes(x = dose, y = value, group = id, color = !!sym("two_dose_permutation")),
    alpha = 0.25, size = 0.4, show.legend = FALSE
  ) +
  geom_point(
    data = result_2_dose_elisa$long_data,
    aes(x = dose, y = value, color = !!sym("two_dose_permutation")),
    alpha = 0.25, size = 0.8, show.legend = FALSE
  ) +
  geom_line(
    data = result_2_dose_elisa$median_values,
    aes(x = dose, y = median_value, group = !!sym("two_dose_permutation"), color = !!sym("two_dose_permutation")),
    size = 1.4
  ) +
  geom_point(
    data = result_2_dose_elisa$median_values,
    aes(x = dose, y = median_value, color = !!sym("two_dose_permutation")),
    size = 3
  ) +
  scale_color_manual(values = c("1-1" = "#1F77B4", "4-4" = "#FF7F0E")) +
  scale_x_discrete(labels = c("base_elisa" = "Baseline", "v1_elisa_wt" = "Dose 1", "v2_elisa_wt" = "Dose 2")) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  labs(x = NULL, y = "WT RBD ELISA (OD value)") +
  theme_minimal() +
  theme(plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.ticks = element_line(),  
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  ) +
  geom_label(
    aes(x = 0.5, y = 5.8, label = elisa_wt_time_plot_2_sample_counts),  # Ensure this variable is defined
    hjust = 0, vjust = 1, size = 3, fontface = "bold", color = "black",
    fill = "white", label.padding = unit(0.3, "lines"),  
    label.r = unit(0.15, "lines"), label.size = 0.3, family = "sans"
  )


#THREE Dose Plot ELISA
elisa_wt_time_plot_3 <- ggplot() +
  geom_rect(
    data = shade_df_3,  # Assuming you have a shading DataFrame
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, alpha = alpha),
    fill = "gray40", inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_alpha_identity() +
  geom_line(
    data = result_3_dose_elisa$long_data,  # Long-format data
    aes(x = dose, y = value, group = id, color = three_dose_permutation),
    alpha = 0.25, size = 0.4, show.legend = FALSE
  ) +
  geom_point(
    data = result_3_dose_elisa$long_data,
    aes(x = dose, y = value, color = three_dose_permutation),
    alpha = 0.25, size = 0.8, show.legend = FALSE
  ) +
  geom_line(
    data = result_3_dose_elisa$median_values,
    aes(x = dose, y = median_value, group = three_dose_permutation, color = three_dose_permutation),
    size = 1.4
  ) +
  geom_point(
    data = result_3_dose_elisa$median_values,
    aes(x = dose, y = median_value, color = three_dose_permutation),
    size = 3
  ) +
  scale_color_manual(name = "Vaccine Type", values = c("1-1-1" = "#1F77B4", "4-4-4" = "#FF7F0E"), labels = c("1-1-1" = "B", "4-4-4" = "S")) +
  scale_x_discrete(labels = c("base_elisa" = "Baseline", 
                              "v1_elisa_wt" = "Dose 1", 
                              "v2_elisa_wt" = "Dose 2", 
                              "v3_elisa_wt" = "Dose 3")) +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  labs(x = NULL, y = "WT RBD ELISA (OD value)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(),
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid"),
    legend.position = c(0.98, 0.98),  # Top-right corner
    legend.justification = c("right", "top"),  # Anchor legend to top-right
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # White background with border
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(face = "bold", size = 10), 
    legend.text = element_text(size = 8),  
    legend.key.size = unit(0.4, "cm"),  # Smaller legend keys for a compact look
    legend.key = element_rect(fill = "white", color = "transparent")
  ) +
  geom_label(
    aes(x = 0.5, y = 5.8, label = elisa_wt_time_plot_3_sample_counts),
    hjust = 0,  # Left-align
    vjust = 1,  # Top-align
    size = 3, 
    fontface = "bold",  
    color = "black",
    fill = "white",  
    label.padding = unit(0.3, "lines"),  
    label.r = unit(0.15, "lines"),  
    label.size = 0.3, 
    family = "sans"  
  )

##TWO Dose Plot sVNT 
svnt_wt_time_plot_2 <- ggplot() +
  geom_rect(
    data = shade_df_2,  # Assuming you have a shading DataFrame
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, alpha = alpha),
    fill = "gray40", inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_alpha_identity() +
  geom_line(
    data = result_2_dose_svnt$long_data,  # Use long-format data from function
    aes(x = dose, y = value, group = id, color = two_dose_permutation),
    alpha = 0.25, size = 0.4, show.legend = FALSE
  ) +
  geom_point(
    data = result_2_dose_svnt$long_data,
    aes(x = dose, y = value, color = two_dose_permutation),
    alpha = 0.25, size = 0.8, show.legend = FALSE
  ) +
  geom_line(
    data = result_2_dose_svnt$median_values,  # Use median values from function
    aes(x = dose, y = median_value, group = two_dose_permutation, color = two_dose_permutation),
    size = 1.4
  ) +
  geom_point(
    data = result_2_dose_svnt$median_values,
    aes(x = dose, y = median_value, color = two_dose_permutation),
    size = 3
  ) +
  scale_color_manual(values = c("1-1" = "#1F77B4", "4-4" = "#FF7F0E")) +
  scale_x_discrete(labels = c("base_svnt" = "Baseline",
                              "v1_svnt_wt" = "Dose 1",
                              "v2_svnt_wt" = "Dose 2")) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(x = NULL, y = "WT sVNT Inhibition (%)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(),
    legend.position = "none",
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  ) +
  geom_label(
    aes(x = 0.5, y = 105, label = svnt_wt_time_plot_2_sample_counts),
    hjust = 0,  # Left-align
    vjust = 1,  # Top-align
    size = 3, 
    fontface = "bold",  
    color = "black",
    fill = "white",  
    label.padding = unit(0.3, "lines"),  
    label.r = unit(0.15, "lines"),  
    label.size = 0.3, 
    family = "sans"  
  )


##THREE Dose Plot sVNT 
svnt_wt_time_plot_3 <- ggplot() +
  geom_rect(
    data = shade_df_3,  # Assuming you have a shading DataFrame
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, alpha = alpha),
    fill = "gray40", inherit.aes = FALSE, show.legend = FALSE
  ) +
  scale_alpha_identity() +
  geom_line(
    data = result_3_dose_svnt$long_data,  # Use long-format data from function
    aes(x = dose, 
        y = ifelse(dose == "base_svnt", pmax(0, value), value), 
        group = id, 
        color = three_dose_permutation),
    alpha = 0.25, size = 0.4, show.legend = FALSE
  ) +
  geom_point(
    data = result_3_dose_svnt$long_data,
    aes(x = dose, 
        y = ifelse(dose == "base_svnt", pmax(0, value), value), 
        color = three_dose_permutation),
    alpha = 0.25, size = 0.8, show.legend = FALSE
  ) +
  geom_line(
    data = result_3_dose_svnt$median_values,  # Use median values from function
    aes(x = dose, 
        y = ifelse(dose == "base_svnt", pmax(0, median_value), median_value), 
        group = three_dose_permutation, 
        color = three_dose_permutation),
    size = 1.4
  ) +
  geom_point(
    data = result_3_dose_svnt$median_values,
    aes(x = dose, 
        y = ifelse(dose == "base_svnt", pmax(0, median_value), median_value), 
        color = three_dose_permutation),
    size = 3
  ) +
  scale_color_manual(values = c("1-1-1" = "#1F77B4", "4-4-4" = "#FF7F0E")) +
  scale_x_discrete(labels = c("base_svnt" = "Baseline",
                              "v1_svnt_wt" = "Dose 1",
                              "v2_svnt_wt" = "Dose 2",
                              "v3_svnt_wt" = "Dose 3")) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(x = NULL, y = "WT sVNT Inhibition (%)") +
  theme_minimal() +
  theme(
    plot.caption = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(),
    legend.position = "none",
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  ) +
  geom_label(
    aes(x = 0.5, y = 105, label = svnt_wt_time_plot_3_sample_counts),
    hjust = 0,  # Left-align
    vjust = 1,  # Top-align
    size = 3, 
    fontface = "bold",  
    color = "black",
    fill = "white",  
    label.padding = unit(0.3, "lines"),  
    label.r = unit(0.15, "lines"),  
    label.size = 0.3, 
    family = "sans"  
  )

#########################################

##MAKING PANELS 
time_analysis_combined_plot <- (elisa_wt_time_plot_2 + elisa_wt_time_plot_3 + 
                                  svnt_wt_time_plot_2 + 
                                  svnt_wt_time_plot_3) +
  plot_layout(ncol = 2) + 
  plot_annotation(title = "WT Analysis per Dosage ", 
                  theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) + 
  theme(plot.background = element_blank(),
        panel.border = element_blank(),    
        plot.margin = margin(0, 0, 0, 0))

print(time_analysis_combined_plot)

ggsave("figure_sup1.pdf", plot = time_analysis_combined_plot, width = 10, height = 6)