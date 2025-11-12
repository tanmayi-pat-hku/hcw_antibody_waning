# Source Code 
source("scripts/cleaned/source2.R")

library(librarian)
shelf(patchwork)

#Make a function that chnages continuous to discrete
create_time_intervals <- function(data) {
  max_days <- max(data$days_since_dose1, na.rm = TRUE)
  breaks <- seq(0, ceiling(max_days/90)*90, by = 90)
  
  data %>%
    mutate(
      time_interval = cut(days_since_dose1, 
                          breaks = breaks,
                          include.lowest = TRUE),
      permutation_type = case_when(
        permutation %in% c("1-1-1", "4-4-4", "1-1-1-1", "4-4-4-4") ~ "Homologous",
        permutation %in% c("4-4-1", "1-1-4", "4-4-1-1", "4-4-4-1") ~ "Heterologous",
        TRUE ~ "Other"
      )
    ) %>%
    mutate(
      time_interval = factor(time_interval,
                             labels = paste0(breaks[-length(breaks)], "-", breaks[-1] - 1))
    )
}

#Make Data Frames for wach 
three_dose_waning_svnt_intervals <- create_time_intervals(three_dose_waning_svnt)
four_dose_waning_svnt_intervals <- create_time_intervals(four_dose_waning_svnt)


#Three Dose Plot 
three_dose_boxplot <- ggplot(three_dose_waning_svnt_intervals, 
                             aes(x = time_interval, y = weight)) +
  geom_boxplot(aes(fill = as.factor(dose3_brand)), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(shape = ifelse(permutation %in% c("4-4-4", "1-1-1"), "Homologous", "Heterologous"),
                  color = as.factor(dose3_brand)), 
              width = 0.2, alpha = 0.8, size = 3) +
  # Colors already match your scatter plot:
  scale_fill_manual(name = "Last Dose",
                    values = c("1" = "#1F77B4", "4" = "#FF7F0E"),  # Same as scatter plot
                    labels = c("1" = "B", "4" = "S")) +
  scale_color_manual(name = "Last Dose",
                     values = c("1" = "#1F77B4", "4" = "#FF7F0E"),  # Same as scatter plot
                     labels = c("1" = "B", "4" = "S")) +
  # Shapes for homologous/heterologous
  scale_shape_manual(name = "Vaccine Series",
                     values = c("Homologous" = 16, "Heterologous" = 17)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(x = "Days Since First Dose", y = "WT sVNT Inhibition (%)", 
       title = "After the second dose") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(),
    legend.position = "none",  
    panel.border = element_blank(),
    axis.line = element_line(size = 0.5, color = "black", linetype = "solid"),
    legend.key.size = unit(1, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12, face = "bold")
  ) #+ facet_wrap(~ ifelse(permutation %in% c("4-4-4", "1-1-1"), "Homologous", "Heterologous"))

# Four Dose Box Plot
four_dose_boxplot <- ggplot(four_dose_waning_svnt_intervals, 
                            aes(x = time_interval, y = weight)) +
  geom_boxplot(aes(fill = as.factor(dose4_brand)), alpha = 0.7, outlier.shape = NA) +
  geom_jitter(aes(shape = ifelse(permutation %in% c("4-4-4-4", "1-1-1-1"), "Homologous", "Heterologous"),
                  color = as.factor(dose4_brand)), 
              width = 0.2, alpha = 0.8, size = 3) +
  # Colors for last dose (dose4_brand)
  scale_fill_manual(name = "Last Dose",
                    values = c("1" = "#1F77B4", "4" = "#FF7F0E"),
                    labels = c("1" = "B", "4" = "S")) +
  scale_color_manual(name = "Last Dose",
                     values = c("1" = "#1F77B4", "4" = "#FF7F0E"),
                     labels = c("1" = "B", "4" = "S")) +
  scale_shape_manual(name = "Vaccine Series",
                     values = c("Homologous" = 16, "Heterologous" = 17)) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(x = "Days Since First Dose", y = "WT sVNT Inhibition (%)", 
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
    legend.title = element_text(size = 12, face = "bold")
  )


# Combine the plots using patchwork
figure_sup3 <- three_dose_boxplot + four_dose_boxplot +
  plot_layout(ncol = 2) +
  plot_annotation(title = "WT sVNT Waning - 90-Day Interval Analysis",
                  theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) +
  plot_annotation(tag_levels = 'A')


print(figure_sup3)