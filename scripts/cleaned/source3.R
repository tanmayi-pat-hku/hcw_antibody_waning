
# Time of Booster Delivery
# Source Code 
source("scripts/cleaned/source1.R")

#Source Measure Interval function 
source("scripts/helper/fc_between_doses.R")

#Prepare Datasets 
# Call the function for doses 2 and 3
dose_2_3 <- get_days_between_doses(three_dose_data, first_dose_col = "v2_date", second_dose_col = "v3_date" , permutation_col = "three_dose_permutation")

dose_2_3_elisa <- dose_2_3 %>% filter(!is.na(v3_elisa_wt)) %>%  filter(dose3_brand %in% c(1, 4))

dose_2_3_svnt <- dose_2_3 %>% filter(!is.na(v3_svnt_wt)) %>%  filter(dose3_brand %in% c(1, 4))

# Call the function for doses 3 and 4
dose_3_4 <- get_days_between_doses(four_dose_data, first_dose_col = "v3_date", second_dose_col = "v4_date", permutation_col = "four_dose_permutation")

#No values
dose_3_4_elisa <- dose_3_4 %>% filter(!is.na(v4_elisa_wt)) %>%  filter(dose4_brand %in% c(1, 4))

dose_3_4_svnt <- dose_3_4 %>% filter(!is.na(v4_svnt_wt)) %>%  filter(dose4_brand %in% c(1, 4))