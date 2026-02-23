# Boost Analysis: ELISA

# source code 
source("scripts/cleaned/source1.R")

# Create combined dataframes for B regimens

#sVNT 

svnt_b_combined_data <- bind_rows(
  #One dose B
  one_valid_perms %>%
    filter(one_dose_permutation == "1") %>%
    select(value = v1_svnt_wt) %>%
    mutate(dose_group = "B", assay = "sVNT"),
  
  #Two doses B-B
  two_valid_perms %>%
    filter(two_dose_permutation == "1-1") %>%
    select(value = v2_svnt_wt) %>%
    mutate(dose_group = "B-B", assay = "sVNT"),
  
  #Three doses B-B-B
  three_valid_perms %>%
    filter(three_dose_permutation == "1-1-1") %>%
    select(value = v3_svnt_wt) %>%
    mutate(dose_group = "B-B-B", assay = "sVNT"),
  
  #Four doses B-B-B-B
  four_valid_perms %>%
    filter(four_dose_permutation == "1-1-1-1") %>%
    select(value = v4_svnt_wt) %>%
    mutate(dose_group = "B-B-B-B", assay = "sVNT")
)

#ELISA
elisa_b_combined_data <- bind_rows(
  #One dose B
  one_valid_perms_e %>%
    filter(one_dose_permutation == "1") %>%
    select(value = v1_elisa_wt) %>%
    mutate(dose_group = "B", assay = "ELISA"),
  
  #Two doses B-B
  two_valid_perms_e %>%
    filter(two_dose_permutation == "1-1") %>%
    select(value = v2_elisa_wt) %>%
    mutate(dose_group = "B-B", assay = "ELISA"),
  
  #Three doses B-B-B
  three_valid_perms_e %>%
    filter(three_dose_permutation == "1-1-1") %>%
    select(value = v3_elisa_wt) %>%
    mutate(dose_group = "B-B-B", assay = "ELISA")
)

#Create combined dataframes for S regimens

#sVNT
svnt_s_combined_data <- bind_rows(
  #One dose S
  one_valid_perms %>%
    filter(one_dose_permutation == "4") %>%
    select(value = v1_svnt_wt) %>%
    mutate(dose_group = "S", assay = "sVNT"),
  
  #Two doses S-S
  two_valid_perms %>%
    filter(two_dose_permutation == "4-4") %>%
    select(value = v2_svnt_wt) %>%
    mutate(dose_group = "S-S", assay = "sVNT"),
  
  #Three doses S-S-S
  three_valid_perms %>%
    filter(three_dose_permutation == "4-4-4") %>%
    select(value = v3_svnt_wt) %>%
    mutate(dose_group = "S-S-S", assay = "sVNT"),
  
  #Four doses S-S-S-S
  four_valid_perms %>%
    filter(four_dose_permutation == "4-4-4-4") %>%
    select(value = v4_svnt_wt) %>%
    mutate(dose_group = "S-S-S-S", assay = "sVNT")
)

#ELISA

elisa_s_combined_data <- bind_rows(
  #One dose S
  one_valid_perms_e %>%
    filter(one_dose_permutation == "4") %>%
    select(value = v1_elisa_wt) %>%
    mutate(dose_group = "S", assay = "ELISA"),
  
  #Two doses S-S
  two_valid_perms_e %>%
    filter(two_dose_permutation == "4-4") %>%
    select(value = v2_elisa_wt) %>%
    mutate(dose_group = "S-S", assay = "ELISA"),
  
  #Three doses S-S-S
  three_valid_perms_e %>%
    filter(three_dose_permutation == "4-4-4") %>%
    select(value = v3_elisa_wt) %>%
    mutate(dose_group = "S-S-S", assay = "ELISA")
)

#Combine data
all_b_data <- bind_rows(svnt_b_combined_data, elisa_b_combined_data)
all_s_data <- bind_rows(svnt_s_combined_data, elisa_s_combined_data)

#Set factor levels for proper ordering
all_b_data$dose_group <- factor(all_b_data$dose_group, 
                                levels = c("B", "B-B", "B-B-B", "B-B-B-B"))
all_b_data$assay <- factor(all_b_data$assay, levels = c("sVNT", "ELISA"))

all_s_data$dose_group <- factor(all_s_data$dose_group, 
                                levels = c("S", "S-S", "S-S-S", "S-S-S-S"))
all_s_data$assay <- factor(all_s_data$assay, levels = c("sVNT", "ELISA"))


## B Regimens

#sVNT plot for B regimens
svnt_b_plot <- ggplot(filter(all_b_data, assay == "sVNT"), 
                      aes(x = dose_group, y = value)) +
  geom_violin(fill = "#1F77B4", alpha = 0.7, width = 0.8, trim = TRUE) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5, color = "#1F77B4") +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(x = NULL, y = "WT sVNT Inhibition (%)", title = "sVNT") +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#ELISA plot for B regimens
elisa_b_plot <- ggplot(filter(all_b_data, assay == "ELISA"), 
                       aes(x = dose_group, y = value)) +
  geom_violin(fill = "#1F77B4", alpha = 0.7, width = 0.8, trim = TRUE) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5, color = "#1F77B4") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +
  labs(x = "B Vaccine Regimens", y = "WT RBD ELISA (OD value)", title = "ELISA") +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

## S Regimens

#sVNT plot for S regimens
svnt_s_plot <- ggplot(filter(all_s_data, assay == "sVNT"), 
                      aes(x = dose_group, y = value)) +
  geom_violin(fill = "#FF7F0E", alpha = 0.7, width = 0.8, trim = TRUE) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5, color = "#FF7F0E") +
  scale_y_continuous(limits = c(0, 110), breaks = seq(0, 100, by = 20)) +
  labs(x = NULL, y = "WT sVNT Inhibition (%)", title = "sVNT") +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#ELISA plot for S regimens
elisa_s_plot <- ggplot(filter(all_s_data, assay == "ELISA"), 
                       aes(x = dose_group, y = value)) +
  geom_violin(fill = "#FF7F0E", alpha = 0.7, width = 0.8, trim = TRUE) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.5, color = "#FF7F0E") +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 0.5)) +
  labs(x = "S Vaccine Regimens", y = "WT RBD ELISA (OD value)", title = "ELISA") +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold", size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


#Combine into panels
s_plot <- (svnt_s_plot / elisa_s_plot) + 
  plot_annotation(title = "S Vaccine Regimens",tag_levels = "A",
                  theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0)))

b_plot <- (svnt_b_plot / elisa_b_plot) + 
  plot_annotation(title = "B Vaccine Regimens",tag_levels = "A",
                  theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0)))

print(s_plot)
print(b_plot)

ggsave("violin_s.pdf", plot = s_plot, width = 15, height = 10)
ggsave("violin_b.pdf", plot = b_plot, width = 15, height = 10)