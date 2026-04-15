library(librarian)
shelf(readxl,
      ggplot2,
      dplyr,
      lubridate,
      scales)

source("scripts/cleaned/post_infection_waning.R")

data_epicurve <- read_excel("data/2026_epicurve_hk.xlsx")

colnames(data_epicurve) <- gsub("\r\n", "", colnames(data_epicurve))

data_epicurve <- data_epicurve %>%
  mutate(
    From = as.Date(From),
    To = as.Date(To),
    cumulative_cases = cumsum(positive_test_detection)
  )

epicurve_plot <- ggplot(data_epicurve, aes(x = From, y = positive_test_detection)) +
  geom_col(fill = "steelblue", color = "steelblue", width = 8, alpha = 0.7) +
  labs(
    x = NULL,
    y = "Number of Cases"
  ) +
  theme_classic() + 
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y",
    expand = c(0.01, 0.01) 
  ) +
  scale_y_continuous(
    expand = c(0, 0),  
    breaks = pretty_breaks(n=10)  
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5),
    axis.ticks.length = unit(0.2, "cm"),
    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 10))
  )

print(epicurve_plot)

ggsave("epicurve_plot.png",
       plot = epicurve_plot)

# Post-Infection and Post-Vaccine (Dose 3) - INFOGRAPHIC VERSION
combined_waning_plot_dose3_infographic <- ggplot() +
  
  # REMOVED all geom_jitter() layers
  
  geom_ribbon(data = post_infection_marginal,
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "Infection", group = 1),
              alpha = 0.25) +
  
  geom_ribbon(data = dose3_marginal %>% filter(permutation == "1-1-1"),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "B-B-B", group = 1),
              alpha = 0.25) +
  
  geom_ribbon(data = dose3_marginal %>% filter(permutation == "4-4-4"),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "S-S-S", group = 1),
              alpha = 0.25) +
  
  geom_ribbon(data = dose3_marginal %>% filter(permutation == "4-4-1"),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "S-S-B", group = 1),
              alpha = 0.25) +
  
  geom_line(data = post_infection_marginal, 
            aes(x = days_since_dose1, y = pred_weight, 
                color = "Infection", group = 1),
            linewidth = 2.2) +
  
  geom_line(data = dose3_marginal %>% filter(permutation == "1-1-1"),
            aes(x = days_since_dose1, y = pred_weight, 
                color = "B-B-B", group = 1),
            linewidth = 2.2) +
  
  geom_line(data = dose3_marginal %>% filter(permutation == "4-4-4"),
            aes(x = days_since_dose1, y = pred_weight, 
                color = "S-S-S", group = 1),
            linewidth = 2.2) +
  
  geom_line(data = dose3_marginal %>% filter(permutation == "4-4-1"),
            aes(x = days_since_dose1, y = pred_weight, 
                color = "S-S-B", group = 1),
            linewidth = 2.2) +
  
  scale_color_manual(
    name = NULL,
    values = c("Infection" = "#CD5C5C", 
               "B-B-B" = "#1F77B4", 
               "S-S-S" = "#FF7F0E",
               "S-S-B" = "darkgreen")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Infection" = "#CD5C5C", 
               "B-B-B" = "#1F77B4", 
               "S-S-S" = "#FF7F0E",
               "S-S-B" = "darkgreen")
  ) +
  
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 60)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  
  # Normal case axis labels (not all caps)
  labs(x = "Days", 
       y = "WT sVNT Inhibition (%)") +
  
  theme_minimal() +
  theme(
    # WAY BIGGER axis titles (normal case)
    axis.title.x = element_text(size = 28, face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(size = 28, face = "bold", margin = margin(r = 20)),
    
    # Larger axis text
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    
    # Cartoon-style thick lines
    axis.line = element_line(size = 1.2, color = "black", lineend = "round"),
    axis.ticks = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(0.3, "cm"),
    
    # Remove gridlines for cleaner look
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Legend styling - no title
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    legend.key.size = unit(1, "cm"),
    
    # Cartoon-style rounded panel
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

###

combined_waning_plot_dose2_infographic <- ggplot() +
  
  # REMOVED all geom_jitter() layers
  
  geom_ribbon(data = post_infection_marginal,
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "Infection", group = 1),
              alpha = 0.25) +
  
  geom_ribbon(data = dose2_marginal %>% filter(permutation == "1-1"),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "B-B", group = 1),
              alpha = 0.25) +
  
  geom_ribbon(data = dose2_marginal %>% filter(permutation == "4-4"),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "S-S", group = 1),
              alpha = 0.25) +
  
  geom_line(data = post_infection_marginal, 
            aes(x = days_since_dose1, y = pred_weight, 
                color = "Infection", group = 1),
            linewidth = 2.2) +
  
  geom_line(data = dose2_marginal %>% filter(permutation == "1-1"),
            aes(x = days_since_dose1, y = pred_weight, 
                color = "B-B", group = 1),
            linewidth = 2.2) +
  
  geom_line(data = dose2_marginal %>% filter(permutation == "4-4"),
            aes(x = days_since_dose1, y = pred_weight, 
                color = "S-S", group = 1),
            linewidth = 2.2) +
  
  scale_color_manual(
    name = NULL,
    values = c("Infection" = "#CD5C5C", 
               "B-B" = "#1F77B4", 
               "S-S" = "#FF7F0E")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Infection" = "#CD5C5C", 
               "B-B" = "#1F77B4", 
               "S-S" = "#FF7F0E")
  ) +
  
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 60)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  
  # REMOVED main title
  labs(x = "Days", 
       y = "WT sVNT Inhibition (%)") +
  
  theme_minimal() +
  theme(
    # WAY BIGGER axis titles
    axis.title.x = element_text(size = 28, face = "bold", margin = margin(t = 20)),
    axis.title.y = element_text(size = 28, face = "bold", margin = margin(r = 20)),
    
    # Larger axis text
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    
    # Cartoon-style thick lines
    axis.line = element_line(size = 1.2, color = "black", lineend = "round"),
    axis.ticks = element_line(size = 1, color = "black"),
    axis.ticks.length = unit(0.3, "cm"),
    
    # Remove gridlines for cleaner look
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Legend styling - no title
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    legend.key.size = unit(1, "cm"),
    
    # Cartoon-style panel
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

print(combined_waning_plot_dose2_infographic)