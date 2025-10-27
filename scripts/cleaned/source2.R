#Code Organization
# Waning Analysis 

# Source Code 
source("scripts/cleaned/source1.R")

#Source Measure Interval Function 
source("scripts/helper/fc_measure_intervals.R")


#Three Dose Data Creation
#sVNT and ELISA
three_dose_waning_elisa <- get_measurements_in_interval(three_dose_data, "dose2_date", "dose3_date", weight_type = "elisa", permutation_col = "three_dose_permutation") %>% filter(dose3_brand %in% c("1", "4"))

three_dose_waning_svnt <- get_measurements_in_interval(three_dose_data, "dose2_date", "dose3_date", weight_type = "svnt", permutation_col = "three_dose_permutation") %>% filter(dose3_brand %in% c("1", "4"))

elisa_participant_count_3 <- three_dose_waning_elisa %>%
  summarise(num_participants = n_distinct(id)) %>%
  mutate(type = "ELISA")

svnt_participant_count_3 <- three_dose_waning_svnt %>%
  summarise(num_participants = n_distinct(id)) %>%
  mutate(type = "SVNT")

participant_counts_3 <- bind_rows(elisa_participant_count_3 , svnt_participant_count_3)


#Four Dose Data Creation
#sVNT and ELISA
four_dose_waning_elisa <- get_measurements_in_interval(four_dose_data, "dose3_date", "dose4_date", weight_type = "elisa", permutation_col = "four_dose_permutation") %>%
  filter(dose4_brand %in% c("4", "1"))

four_dose_waning_svnt <- get_measurements_in_interval(four_dose_data, "dose3_date", "dose4_date", weight_type = "svnt", permutation_col = "four_dose_permutation") %>%
  filter(dose4_brand %in% c("4", "1"))

elisa_participant_count_4 <- four_dose_waning_elisa %>%
  summarise(num_participants = n_distinct(id)) %>%
  mutate(type = "ELISA")

svnt_participant_count_4 <- four_dose_waning_svnt %>%
  summarise(num_participants = n_distinct(id)) %>%
  mutate(type = "SVNT")

participant_counts_4 <- bind_rows(elisa_participant_count_4 , svnt_participant_count_4)