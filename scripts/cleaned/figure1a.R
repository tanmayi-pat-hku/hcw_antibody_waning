#Boost Analysis: ELISA

# Source Code 
source("scripts/cleaned/source1.R")


cohort_infection_elisa_plot <- ggplot(cohort_data_elisa, aes(x = factor(1), y = i1_elisa_wt)) + 
  geom_jitter(width = 0.2, alpha = 0.8, size = 2.5, colour = "darkred") + 
  geom_crossbar(data = cohort_data_median_elisa, aes(x = factor(1), y = median, ymin = median, ymax = median), 
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = c(""), expand = expansion(add = c(0.5, 0.5))) + 
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) + 
  labs(x = NULL, y = "WT RBD ELISA (OD value)", caption = "Cohort Post-Infection") + 
  theme_bw(base_size = 12) + 
  theme(plot.caption = element_text(face = "bold", size = 10, hjust = 0.5), 
        plot.title = element_text(face = "bold", size = 14), 
        axis.title = element_text(face = "bold"), 
        axis.text.x = element_text(face = "bold"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "none", 
        panel.border = element_blank(), 
        axis.line = element_line(size = 0.5, color = "black"))

#Community Plot for sVNT 2021
community_infection_elisa_plot <- ggplot(community_data_elisa, aes(x = factor(1), y = elisa)) + 
  geom_jitter(width = 0.2, alpha = 0.8, size = 2.5, colour = "deeppink") + 
  geom_crossbar(data = community_data_median_elisa, aes(x = factor(1), y = median, ymin = median, ymax = median), 
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = c(""), expand = expansion(add = c(0.5, 0.5))) + 
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) + 
  labs(x = NULL, y = "WT RBD ELISA (OD value)", caption = "Community Post-Infection") + 
  theme_bw(base_size = 12) + 
  theme(plot.caption = element_text(face = "bold", size = 10, hjust = 0.5), 
        plot.title = element_text(face = "bold", size = 14), 
        axis.title = element_text(face = "bold"), 
        axis.text.x = element_text(face = "bold"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.ticks = element_blank(), 
        legend.position = "none", 
        panel.border = element_blank(), 
        axis.line = element_line(size = 0.5, color = "black"))

#ELISA Baseline Graph 
baseline_elisa$baseline_value <- as.numeric(baseline_elisa$baseline_value)

elisa_baseline_wt <- ggplot() +
  geom_jitter(data = baseline_elisa,
              aes(x = factor(dose1_brand), y = baseline_value, color = factor(dose1_brand)),
              alpha = 0.8, width = 0.18, size = 2.5) + 
  geom_crossbar(data = median_baseline_elisa,
                aes(x = factor(dose1_brand), y = median, 
                    ymin = median, ymax = median),  
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = one_labels, expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +
  scale_color_manual(values = c("1" = "#1F77B4", "4" = "#FF7F0E")) +
  labs(x = NULL,
       y = "WT RBD ELISA (OD value)",
       caption = "Baseline") +  
  theme_bw(base_size = 12) +  
  theme(plot.caption = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.ticks = element_line(),  
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  )


#Plot for ELISA ONE 
response_colq <- "v1_elisa_wt"

one_elisa_wt_plot <- ggplot() +
  geom_jitter(data = one_valid_perms_e,
              aes_string(x = "one_dose_permutation", y = response_colq, color = "one_dose_permutation"),
              alpha = 0.8, width = 0.18, size = 2.5) +
  geom_crossbar(data = one_dose_medians_e,
                aes_string(x = "one_dose_permutation", y = "median",  
                           ymin = "median", ymax = "median"),  
                width = 0.5, colour = "black", size = 0.5) +  
  scale_x_discrete(labels = one_labels, expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +
  scale_color_manual(values = c("1" = "#1F77B4", "4" = "#FF7F0E")) + 
  labs(x = NULL,
       y = "WT RBD ELISA (OD value)",
       caption = "1 Dose") +  
  theme_bw(base_size = 12) +  
  theme(plot.caption = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.ticks = element_line(),  
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  )

#Plot for ELISA TWO 
response_colq <- "v2_elisa_wt"

two_elisa_wt_plot <- ggplot() +
  geom_jitter(data = two_valid_perms_e,
              aes_string(x = "two_dose_permutation", y = response_colq, color = "two_dose_permutation"),
              alpha = 0.8, width = 0.18, size = 2.5) +
  geom_crossbar(data = two_dose_medians_e,
                aes_string(x = "two_dose_permutation", y = "median",  
                           ymin = "median", ymax = "median"),  
                width = 0.5, colour = "black", size = 0.5) +  
  scale_x_discrete(labels = two_labels, limits = c("1-1","4-4"), expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +
  scale_color_manual(values = c("1-1" = "#1F77B4", "4-4" = "#FF7F0E")) + 
  labs(x = NULL,
       y = "WT RBD ELISA (OD value)",
       caption = "2 Doses") +  
  theme_bw(base_size = 12) +  
  theme(plot.caption = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.ticks = element_line(),  
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  )


#Plot for ELISA THREE 
response_colq <- "v3_elisa_wt"

three_elisa_wt_plot <- ggplot() +
  geom_jitter(data = three_valid_perms_e,
              aes_string(x = "three_dose_permutation", y = response_colq, color = "three_dose_permutation"),
              alpha = 0.8, width = 0.18, size = 2.5) +
  geom_crossbar(data = three_dose_medians_e,
                aes_string(x = "three_dose_permutation", y = "median",  
                           ymin = "median", ymax = "median"),  
                width = 0.5, colour = "black", size = 0.5) +  
  scale_x_discrete(labels = three_labels_elisa, limits = c("1-1-1", "4-4-4", "4-4-1"), expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +
  scale_color_manual(values = c("1-1-1" = "#1F77B4", "4-4-4" = "#FF7F0E", "4-4-1" = "#1B5E20", "1-1-4" = "#4A148C")) + 
  labs(x = NULL,  
       y = "WT RBD ELISA (OD value)",
       caption = "3 Doses") +  
  theme_bw(base_size = 12) +  
  theme(plot.caption = element_text(face = "bold", size = 10, hjust = 0.5),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, face = "bold"),  
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        axis.ticks = element_line(),  
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_line(size = 0.5, color = "black", linetype = "solid")
  )


#########################################

#Panel Generation for Boost Analysis 

#ELISA 
y_limits <- c(0, 6) 

elisa_baseline_wt <- elisa_baseline_wt + scale_y_continuous(limits = y_limits, breaks = seq(0, 6, by = 0.5)) +
  labs(y = "WT RBD ELISA (OD value)") + 
  theme(axis.text.y = element_text(size = 10), 
        axis.ticks.y = element_line(),          
        axis.line.y = element_line())  

cohort_infection_elisa_plot <- cohort_infection_elisa_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 6, by = 0.5)) +
  labs(y = NULL) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.y = element_blank())

community_infection_elisa_plot <- community_infection_elisa_plot + scale_y_continuous(limits = y_limits, breaks = seq(0, 6, by = 0.5)) +
  labs(y = NULL) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.y = element_blank())

one_elisa_wt_plot <- one_elisa_wt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 6, by = 0.5)) +
  labs(y = NULL) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.y = element_blank())         

two_elisa_wt_plot <- two_elisa_wt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 6, by = 0.5)) +
  labs(y = NULL) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.y = element_blank())   

three_elisa_wt_plot <- three_elisa_wt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 6, by = 0.5)) +
  labs(y = NULL) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

elisa_wt_combined_plot_with_community <- (elisa_baseline_wt + cohort_infection_elisa_plot + 
                                            community_infection_elisa_plot + one_elisa_wt_plot + 
                                            two_elisa_wt_plot + 
                                            three_elisa_wt_plot) + 
  plot_layout(ncol = 6) + 
  plot_annotation(title = "WT RBD ELISA", 
                  theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) + 
  theme(plot.background = element_blank(),
        panel.border = element_blank(),    
        plot.margin = margin(0, 0, 0, 0))

print(elisa_wt_combined_plot_with_community)

ggsave("elisa_wt_combined_plot_with_community.pdf", plot = elisa_wt_combined_plot_with_community, width = 24, height = 6)
