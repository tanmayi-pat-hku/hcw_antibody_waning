#Boost Analysis: ELISA

#source code 
source("scripts/cleaned/source1.R")


#Boost Analysis: ELISA
community_cohort_elisa_plot <- ggplot(community_cohort_elisa_data, aes(x = group, y = value, color = group)) + 
  geom_jitter(width = 0.2, alpha = 0.8, size = 2.5) + 
  geom_crossbar(data = community_cohort_elisa_median, aes(x = group, y = median, ymin = median, ymax = median), 
                width = 0.4, colour = "black", size = 0.5) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) + 
  scale_color_manual(values = c("Cohort" = "darkred", "Community" = "deeppink")) +
  labs(x = NULL, y = "WT RBD ELISA (OD value)", caption = "Post-Infection") + 
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

community_cohort_elisa_plot <- community_cohort_elisa_plot + scale_y_continuous(limits = y_limits, breaks = seq(0, 6, by = 0.5)) +
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

elisa_wt_combined_plot_with_community <- (elisa_baseline_wt + community_cohort_elisa_plot + 
                                            one_elisa_wt_plot + 
                                            two_elisa_wt_plot + 
                                            three_elisa_wt_plot) + 
  plot_layout(ncol = 5) + 
  plot_annotation(title = "WT RBD ELISA", 
                  theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) + 
  theme(plot.background = element_blank(),
        panel.border = element_blank(),    
        plot.margin = margin(0, 0, 0, 0))

#Boost Analysis: sVNT 

# Create the combined plot
community_cohort_svnt_plot <- ggplot(community_cohort_svnt_data, aes(x = group, y = value, color = group)) + 
  geom_jitter(width = 0.2, alpha = 0.8, size = 2.5) + 
  geom_crossbar(data = community_cohort_svnt_median, aes(x = group, y = median, ymin = median, ymax = median), 
                width = 0.4, colour = "black", size = 0.5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) + 
  scale_color_manual(values = c("Cohort" = "darkred", "Community" = "deeppink")) +
  labs(x = NULL, y = "WT sVNT Inhibition (%)", caption = "Post-Infection") + 
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

community_cohort_svnt_plot <- community_cohort_svnt_plot + 
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
        axis.line.y = element_blank())

four_svnt_wt_plot <- four_svnt_wt_plot + 
  scale_y_continuous(limits = y_limits, breaks = seq(0, 100, by = 20)) +
  labs(y = NULL) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

svnt_wt_combined_plot_with_community <- (svnt_baseline_wt + community_cohort_svnt_plot + one_svnt_wt_plot + 
                                           two_svnt_wt_plot + 
                                           three_svnt_wt_plot + 
                                           four_svnt_wt_plot) + 
  plot_layout(widths = c(1.0, 1.0, 1.0, 1.0, 2.0, 2.0), ncol = 6) + plot_annotation(title = "WT sVNT", 
                                                                                    theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))) +
  theme(plot.background = element_blank(),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.border = element_blank(), 
        plot.margin = margin(0, 0, 0, 0))

####Make Final Panels 

# Combine the two plots vertically
boost_plot <- (svnt_wt_combined_plot_with_community / elisa_wt_combined_plot_with_community) + 
  plot_annotation(title = "WT sVNT and RBD ELISA", 
                  theme = theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5)))

# Print the combined plot
print(boost_plot)

ggsave("boost_plot.pdf", plot = boost_plot, width = 24, height = 6)
