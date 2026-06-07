#Code Organization
# Waning Analysis 

#Set Working Directory for Stand alone (For Wey Wen Dropbox)
#setwd("~/Desktop/Shared COVID HCW antibody waning/2025_10_hcw_abwaning")

# Source Code 
source("scripts/cleaned/source1.R")
#source("output/scripts/cleaned/source1.R") #For Wey Wen Dropbox

#Source Measure Interval Function for Post-Vaccination + Post-Infections
source("scripts/helper/fc_measure_intervals.R")
#source("output/scripts/helper/fc_measure_intervals.R") #For Wey Wen Dropbox
source("scripts/helper/fc_measure_postinfect_intervals.R")
#source("output/scripts/helper/fc_measure_postinfect_intervals.R") #For Wey Wen Dropbox

#Two Dose Waning
#sVNT and ELISA

# Create a 'type' column: homologous if the two‑dose series was B‑B or S‑S
three_dose_data <- three_dose_data %>%
  mutate(type = ifelse(two_dose_permutation %in% c("1-1", "4-4"), "homologous", "heterologous"))

# Extract ELISA measurements taken between dose2 and dose3.
two_dose_waning_elisa <- get_measurements_in_interval(three_dose_data, "dose2_date", "dose3_date", weight_type = "elisa", permutation_col = "two_dose_permutation", min_days = 0) %>% filter(dose2_brand %in% c("1", "4"))

# Extract sVNT measurements taken between dose2 and dose3.
two_dose_waning_svnt <- get_measurements_in_interval(three_dose_data, "dose2_date", "dose3_date", weight_type = "svnt", permutation_col = "two_dose_permutation", min_days = 0) %>% filter(dose2_brand %in% c("1", "4"))


#Three Dose Waning
#sVNT and ELISA

# Add homologous/heterologous classification based on the three‑dose series
four_dose_data <- four_dose_data %>%
  mutate(type = ifelse(three_dose_permutation %in% c("1-1-1", "4-4-4"), "homologous", "heterologous"))

# Extract ELISA measurements between dose3 and dose4.
three_dose_waning_elisa <- get_measurements_in_interval(four_dose_data, "dose3_date", "dose4_date", weight_type = "elisa", permutation_col = "three_dose_permutation", min_days = 0) %>%
  filter(dose3_brand %in% c("4", "1"))

# Extract sVNT measurements between dose3 and dose4,
three_dose_waning_svnt <- get_measurements_in_interval(four_dose_data, "dose3_date", "dose4_date", weight_type = "svnt", permutation_col = "three_dose_permutation", min_days = 0) %>%
  filter(permutation %in% c("4-4-4", "1-1-1", "4-4-1"))

# Post-infection waning
# Extract sVNT measurements taken after the first positive PCR date
post_infection_svnt <- get_infection_waning(data, "posdate_p1", "svnt") %>%
  rename(days_since_dose1 = days_since_infection)


