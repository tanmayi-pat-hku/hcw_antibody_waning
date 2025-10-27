# Source Code 
source("scripts/cleaned/source1.R")

#########################################

# Boost Analysis per Dose 

#Two Dose and Three Dose Shade Df for Aesthetic 
shade_df_2 <- tibble(
  xmin = c(0.5, 1.5, 2.5),
  xmax = c(1.5, 2.5, 3.5),
  alpha = c(0.02, 0.05, 0.08)
)

shade_df_3 <- tibble(
  xmin = c(0.5, 1.5, 2.5, 3.5),
  xmax = c(1.5, 2.5, 3.5, 4.5),
  alpha = c(0.02, 0.05, 0.08, 0.11)
)

#Source Function 
source("scripts/helper/fc_process_dose_data.R")

#FORM NEEDED DATASETS

#sVNT and ELISA
# 2-dose ELISA
result_2_dose_elisa <- process_dose_data(two_dose_data, baseline_elisa, c("elisa"), 2, "two_dose_permutation")

# 3-dose ELISA
result_3_dose_elisa <- process_dose_data(three_dose_data, baseline_elisa, c("elisa"), 3, "three_dose_permutation")

# 2-dose SVNT
result_2_dose_svnt <- process_dose_data(two_dose_data, baseline_svnt, c("svnt"), 2, "two_dose_permutation")

# 3-dose SVNT
result_3_dose_svnt <- process_dose_data(three_dose_data, baseline_svnt, c("svnt"), 3, "three_dose_permutation")

#ELISA ANALYSIS 

#Two Dose ELISA 
elisa_wt_time_plot_2_sample_counts <- result_2_dose_elisa$long_data %>%
  group_by(two_dose_permutation) %>%
  summarise(n = n_distinct(id), .groups = "drop")

elisa_wt_time_plot_2_sample_counts <- with(elisa_wt_time_plot_2_sample_counts, 
                                           paste("B: n =", n[two_dose_permutation == "1-1"], "\n",
                                                 "S: n =", n[two_dose_permutation == "4-4"]))

##THREE Dose ELISA 
elisa_wt_time_plot_3_sample_counts <- result_3_dose_elisa$long_data %>%
  group_by(three_dose_permutation) %>%
  summarise(n = n_distinct(id), .groups = "drop")

# Format the sample counts for the label
elisa_wt_time_plot_3_sample_counts <- with(elisa_wt_time_plot_3_sample_counts, 
                                           paste("B: n =", n[three_dose_permutation == "1-1-1"], "\n",
                                                 "S: n =", n[three_dose_permutation == "4-4-4"]))




#sVNT Analysis 

##TWO Dose sVNT 
svnt_wt_time_plot_2_sample_counts <- result_2_dose_svnt$long_data %>%
  group_by(two_dose_permutation) %>%
  summarise(n = n_distinct(id), .groups = "drop")

svnt_wt_time_plot_2_sample_counts <- with(svnt_wt_time_plot_2_sample_counts, 
                                          paste("B: n =", n[two_dose_permutation == "1-1"], "\n",
                                                "S: n =", n[two_dose_permutation == "4-4"]))