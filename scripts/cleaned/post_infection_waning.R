source("scripts/cleaned/power_law.R")
source("scripts/cleaned/figure2.R")

library(librarian)
shelf(lme4, 
      tidyverse, 
      patchwork,
      boot)

cat("Post-infection sVNT measurements:", nrow(post_infection_svnt), "\n")
cat("Number of unique individuals:", n_distinct(post_infection_svnt$id), "\n")

post_infection_waning_plot <- ggplot() +
  geom_point(data = post_infection_svnt, 
             aes(x = days_since_dose1, y = weight),
             color = "#CD5C5C", size = 3.5, alpha = 0.5) +
  geom_ribbon(data = post_infection_marginal,
              aes(x = days_since_dose1, ymin = lower, ymax = upper),
              fill = "#CD5C5C", alpha = 0.2, show.legend = FALSE) +
  geom_line(data = post_infection_marginal, 
            aes(x = days_since_dose1, y = pred_weight),
            color = "#CD5C5C", linewidth = 1.2) +
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 30)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "Days since infection", y = "WT sVNT Inhibition (%)", 
       title = "Post-Infection Waning") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.line = element_line(size = 0.5, color = "black")
  )

print(post_infection_waning_plot)

#############################

combined_waning_plot <- ggplot() +
geom_jitter(data = post_infection_svnt, 
            aes(x = days_since_dose1, y = weight),
            color = "#CD5C5C", size = 2, alpha = 0.3, width = 2) +
  
  geom_jitter(data = three_dose_waning_svnt %>% filter(permutation == "1-1"),
              aes(x = days_since_dose1, y = weight),
              color = "#1F77B4", size = 2, alpha = 0.3, width = 2) +
  
  geom_jitter(data = three_dose_waning_svnt %>% filter(permutation == "4-4"),
              aes(x = days_since_dose1, y = weight),
              color = "#FF7F0E", size = 2, alpha = 0.3, width = 2) +
  
geom_ribbon(data = post_infection_marginal,
            aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                fill = "Infection", group = 1),
            alpha = 0.2) +
  
  geom_ribbon(data = dose2_marginal %>% filter(permutation == "1-1"),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "B-B", group = 1),
              alpha = 0.2) +
  
  geom_ribbon(data = dose2_marginal %>% filter(permutation == "4-4"),
              aes(x = days_since_dose1, ymin = lower, ymax = upper, 
                  fill = "S-S", group = 1),
              alpha = 0.2) +
  
geom_line(data = post_infection_marginal, 
          aes(x = days_since_dose1, y = pred_weight, 
              color = "Infection", group = 1),
          linewidth = 1.5) +
  
  geom_line(data = dose2_marginal %>% filter(permutation == "1-1"),
            aes(x = days_since_dose1, y = pred_weight, 
                color = "B-B", group = 1),
            linewidth = 1.5) +
  
  geom_line(data = dose2_marginal %>% filter(permutation == "4-4"),
            aes(x = days_since_dose1, y = pred_weight, 
                color = "S-S", group = 1),
            linewidth = 1.5) +
  
scale_color_manual(
  name = "Group",
  values = c("Infection" = "#CD5C5C", 
             "B-B" = "#1F77B4", 
             "S-S" = "#FF7F0E")
) +
  scale_fill_manual(
    name = "Group",
    values = c("Infection" = "#CD5C5C", 
               "B-B" = "#1F77B4", 
               "S-S" = "#FF7F0E")
  ) +
  
coord_cartesian(xlim = c(0, 420), ylim = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 420, by = 60)) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  
labs(x = "Days since event", y = "WT sVNT Inhibition (%)", 
     title = "Waning: Post Second-Dose Vaccine vs Post-Infection") +
  
theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.line = element_line(size = 0.5, color = "black")
  )

print(combined_waning_plot)

# Save
ggsave("post_infection_waning_plot.pdf", plot = post_infection_waning_plot, width = 8, height = 6)
ggsave("combined_waning_plot.pdf", plot = combined_waning_plot, width = 10, height = 6)

