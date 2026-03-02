# For sVNT data
library(dplyr)
library(tidyr)

# Create combined sVNT dataframe
svnt_combined_data <- bind_rows(
  # Baseline
  baseline_svnt %>%
    mutate(dose_group = case_when(
      dose1_brand == "1" ~ "B",
      dose1_brand == "4" ~ "S"
    )) %>%
    select(value = baseline_value, dose_group) %>%
    mutate(dose_count = "Baseline"),
  
  # One dose
  one_valid_perms %>%
    mutate(dose_group = case_when(
      one_dose_permutation == "1" ~ "B",
      one_dose_permutation == "4" ~ "S"
    )) %>%
    select(value = v1_svnt_wt, dose_group) %>%
    mutate(dose_count = "1 Dose"),
  
  # Two doses
  two_valid_perms %>%
    mutate(dose_group = case_when(
      two_dose_permutation == "1-1" ~ "B-B",
      two_dose_permutation == "4-4" ~ "S-S"
    )) %>%
    select(value = v2_svnt_wt, dose_group) %>%
    mutate(dose_count = "2 Doses"),
  
  # Three doses
  three_valid_perms %>%
    mutate(dose_group = case_when(
      three_dose_permutation == "1-1-1" ~ "B-B-B",
      three_dose_permutation == "4-4-4" ~ "S-S-S",
      three_dose_permutation == "4-4-1" ~ "S-S-B",
      three_dose_permutation == "1-1-4" ~ "B-B-S"
    )) %>%
    select(value = v3_svnt_wt, dose_group) %>%
    mutate(dose_count = "3 Doses")
)

# Factor levels for proper ordering
svnt_combined_data$dose_count <- factor(svnt_combined_data$dose_count, 
                                        levels = c("Baseline", "1 Dose", "2 Doses", "3 Doses"))
svnt_combined_data$dose_group <- factor(svnt_combined_data$dose_group,
                                        levels = c("B", "S", "B-B", "S-S", "B-B-B", "S-S-S", "B-B-S", "S-S-B"))

# Create similar for ELISA
elisa_combined_data <- bind_rows(
  # Baseline
  baseline_elisa %>%
    mutate(dose_group = case_when(
      dose1_brand == "1" ~ "B",
      dose1_brand == "4" ~ "S"
    )) %>%
    select(value = baseline_value, dose_group) %>%
    mutate(dose_count = "Baseline"),
  
  # One dose
  one_valid_perms_e %>%
    mutate(dose_group = case_when(
      one_dose_permutation == "1" ~ "B",
      one_dose_permutation == "4" ~ "S"
    )) %>%
    select(value = v1_elisa_wt, dose_group) %>%
    mutate(dose_count = "1 Dose"),
  
  # Two doses
  two_valid_perms_e %>%
    mutate(dose_group = case_when(
      two_dose_permutation == "1-1" ~ "B-B",
      two_dose_permutation == "4-4" ~ "S-S"
    )) %>%
    select(value = v2_elisa_wt, dose_group) %>%
    mutate(dose_count = "2 Doses"),
  
  # Three doses
  three_valid_perms_e %>%
    mutate(dose_group = case_when(
      three_dose_permutation == "1-1-1" ~ "B-B-B",
      three_dose_permutation == "4-4-4" ~ "S-S-S",
      three_dose_permutation == "4-4-1" ~ "S-S-B"
    )) %>%
    select(value = v3_elisa_wt, dose_group) %>%
    mutate(dose_count = "3 Doses")
)

elisa_combined_data$dose_count <- factor(elisa_combined_data$dose_count, 
                                         levels = c("Baseline", "1 Dose", "2 Doses", "3 Doses"))
elisa_combined_data$dose_group <- factor(elisa_combined_data$dose_group,
                                         levels = c("B", "S", "B-B", "S-S", "B-B-B", "S-S-S", "S-S-B"))