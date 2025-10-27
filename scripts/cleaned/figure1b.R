
#Boost Analysis: sVNT

# Source Code 
source("scripts/cleaned/source1.R")

#Plot post-infection Cohort sVNT Value for 2022
cohort_infection_svnt_plot <- ggplot(cohort_data_svnt, aes(x = factor(1), y = i1_svnt_wt)) + 
  geom_jitter(width = 0.2, alpha = 0.8, size = 2.5, colour = "darkred") + 
  geom_crossbar(data = cohort_data_median_svnt, aes(x = factor(1), y = median, ymin = median, ymax = median), 
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = c(""), expand = expansion(add = c(0.5, 0.5))) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  labs(x = NULL, y = "WT sVNT Inhibition (%)", caption = "Cohort Post-Infection") + 
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
community_infection_svnt_plot <- ggplot(community_data_svnt, aes(x = factor(1), y = sVNT)) + 
  geom_jitter(width = 0.2, alpha = 0.8, size = 2.5, colour = "deeppink") + 
  geom_crossbar(data = community_data_median_svnt, aes(x = factor(1), y = median, ymin = median, ymax = median), 
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = c(""), expand = expansion(add = c(0.5, 0.5))) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  labs(x = NULL, y = "WT sVNT Inhibition (%)", caption = "Community Post-Infection") + 
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

#sVNT Baseline Graph 
svnt_baseline_wt <- ggplot() +
  geom_jitter(data = baseline_svnt,
              aes(x = factor(dose1_brand), y = baseline_value, color = factor(dose1_brand)),
              alpha = 0.8, width = 0.18, size = 2.5) +
  geom_crossbar(data = median_baseline_svnt,
                aes(x = factor(dose1_brand), y = median, 
                    ymin = median, ymax = median),  
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = one_labels, expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c("1" = "#1F77B4", "4" = "#FF7F0E")) + 
  labs(x = NULL,
       y = "WT sVNT Inhibition (%)",
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


#Plot for sVNT ONE 
response_colq <- "v1_svnt_wt"

one_svnt_wt_plot <- ggplot() +
  geom_jitter(data = one_valid_perms,
              aes_string(x = "one_dose_permutation", y = response_colq, color = "one_dose_permutation"),
              alpha = 0.8, width = 0.18, size = 2.5) +
  geom_crossbar(data = one_dose_medians,
                aes_string(x = "one_dose_permutation", y = "median",  
                           ymin = "median", ymax = "median"),  
                width = 0.5, colour = "black", size = 0.5) +  
  scale_x_discrete(labels = one_labels, expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  scale_color_manual(values = c("1" = "#1F77B4", "4" = "#FF7F0E")) + 
  labs(x = NULL,
       y = "WT sVNT Inhibition (%)",
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

#Plot for sVNT TWO
response_colq <- "v2_svnt_wt"

two_svnt_wt_plot <- ggplot() +
  geom_jitter(data = two_valid_perms,
              aes(x = two_dose_permutation, y = !!sym(response_colq), color = two_dose_permutation),
              alpha = 0.8, width = 0.18, size = 2.5) +
  geom_crossbar(data = two_dose_medians,
                aes(x = two_dose_permutation, y = median, 
                    ymin = median, ymax = median),  
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = two_labels, limits = c("1-1", "4-4"), expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(x = NULL,
       y = "WT sVNT Inhibition (%)",
       caption = "2 Doses") +  
  theme_bw(base_size = 12) +  
  scale_color_manual(values = c("1-1" = "#1F77B4", "4-4" = "#FF7F0E")) +
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


#Plot for sVNT THREE
response_colq <- "v3_svnt_wt"

three_svnt_wt_plot <- ggplot() +
  geom_jitter(data = three_valid_perms,
              aes(x = three_dose_permutation, y = !!sym(response_colq), color = three_dose_permutation),
              alpha = 0.8, width = 0.18, size = 2.5) +
  geom_crossbar(data = three_dose_medians,
                aes(x = three_dose_permutation, y = median, 
                    ymin = median, ymax = median),  
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = three_labels_svnt,limits = c("1-1-1", "4-4-4", "1-1-4", "4-4-1"), expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(x = NULL,
       y = "WT sVNT Inhibition (%)",
       caption = "3 Doses") +  
  scale_color_manual(values = c("1-1-1" = "#1F77B4", "4-4-4" = "#FF7F0E","4-4-1"= "#1B5E20", "1-1-4" = "#4A148C")) +
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


#Plot for sVNT FOUR
four_labels_svnt <- c("1-1-1-1" =" B-B-B-B", "4-4-1-1" =" S-S-B-B", "4-4-4-4" ="S-S-S-S","4-4-4-1" ="S-S-S-B")

response_colq <- "v4_svnt_wt"

four_svnt_wt_plot <- ggplot() +
  geom_jitter(data = four_valid_perms,
              aes(x = four_dose_permutation, y = !!sym(response_colq), color = four_dose_permutation),
              alpha = 0.8, width = 0.18, size = 2.5) +
  geom_crossbar(data = four_dose_medians,
                aes(x = four_dose_permutation, y = median, 
                    ymin = median, ymax = median),  
                width = 0.5, colour = "black", size = 0.5) +
  scale_x_discrete(labels = four_labels_svnt, limits = c("1-1-1-1", "4-4-4-4", "4-4-4-1", "4-4-1-1"), expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(x = NULL,
       y = "WT sVNT Inhibition (%)",
       caption = "4 Doses") + 
  scale_color_manual(values = c(
    "1-1-1-1" = "#1F77B4",  
    "1-1-1-9" = "#2CA02C",  
    "4-4-1-1" = "#D62728",   
    "4-4-1-9" = "#9467BD",   
    "4-4-4-4" = "#8C564B",   
    "4-4-4-1" = "#E377C2"   
  )) +
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

#sVNT 
y_limits <- c(0, 105)

svnt_baseline_wt <- svnt_baseline_wt + scale_y_continuous(limits = y_limits, breaks = seq(0, 100, by = 20)) +
  labs(y = "WT sVNT Inhibition (%)") + 
  theme(axis.text.y = element_text(size = 10), 
        axis.ticks.y = element_line(),          
        axis.line.y = element_line())   

cohort_infection_svnt_plot <- cohort_infection_svnt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 100, by = 20)) +
  labs(y = NULL) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.y = element_blank())

community_infection_svnt_plot <- community_infection_svnt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 100, by = 20)) +
  labs(y = NULL) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.y = element_blank())

one_svnt_wt_plot <- one_svnt_wt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 100, by = 20)) +
  labs(y = NULL) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.y = element_blank())          

two_svnt_wt_plot <- two_svnt_wt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 100, by = 20)) +
  labs(y = NULL) +  
  theme(axis.text.y = element_blank(),  
        axis.ticks.y = element_blank(),  
        axis.line.y = element_blank())   

three_svnt_wt_plot <- three_svnt_wt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 100, by = 20)) +
  labs(y = NULL) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

four_svnt_wt_plot <- four_svnt_wt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 100, by = 20)) +
  labs(y = NULL) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

svnt_wt_combined_plot_with_community <- (svnt_baseline_wt + cohort_infection_svnt_plot + community_infection_svnt_plot + one_svnt_wt_plot + 
                                           two_svnt_wt_plot + 
                                           three_svnt_wt_plot + 
                                           four_svnt_wt_plot) + 
  plot_layout(widths = c(1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0), ncol = 7) + plot_annotation(title = "WT sVNT", 
                                                                                         theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) +
  theme(plot.background = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.border = element_blank(), 
        plot.margin = margin(0, 0, 0, 0))

print(svnt_wt_combined_plot_with_community)

ggsave("svnt_wt_combined_plot_with_community.pdf", plot = svnt_wt_combined_plot_with_community, width = 24 , height = 6)
