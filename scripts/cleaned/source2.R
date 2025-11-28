#Code Organization
# Waning Analysis 

# Source Code 
source("scripts/cleaned/source1.R")

#Source Measure Interval Function 
source("scripts/helper/fc_measure_intervals.R")

#Source Function to Make Continuous to Discete Days (Panel C)
source("scripts/helper/fc_continuous_to_discrete.R")


three_dose_data <- three_dose_data %>%
  mutate(type = ifelse(two_dose_permutation %in% c("1-1", "4-4"), "homologous", "heterologous"))

#Three Dose Data Creation
#sVNT and ELISA
three_dose_waning_elisa <- get_measurements_in_interval(three_dose_data, "dose2_date", "dose3_date", weight_type = "elisa", permutation_col = "two_dose_permutation") %>% filter(dose2_brand %in% c("1", "4"))

three_dose_waning_svnt <- get_measurements_in_interval(three_dose_data, "dose2_date", "dose3_date", weight_type = "svnt", permutation_col = "two_dose_permutation") %>% filter(dose2_brand %in% c("1", "4"))


#Four Dose Data Creation
#sVNT and ELISA

four_dose_data <- four_dose_data %>%
  mutate(type = ifelse(three_dose_permutation %in% c("1-1-1", "4-4-4"), "homologous", "heterologous"))

four_dose_waning_elisa <- get_measurements_in_interval(four_dose_data, "dose3_date", "dose4_date", weight_type = "elisa", permutation_col = "three_dose_permutation") %>%
  filter(dose3_brand %in% c("4", "1"))

four_dose_waning_svnt <- get_measurements_in_interval(four_dose_data, "dose3_date", "dose4_date", weight_type = "svnt", permutation_col = "three_dose_permutation") %>%
  filter(dose3_brand %in% c("4", "1"))


#Discrete Comparison for After Dose 3 (For Heterologous and Homologous)

four_dose_waning_svnt_intervals <- create_time_intervals_discrete(four_dose_waning_svnt) %>%
  filter(permutation != "1-66-1")






