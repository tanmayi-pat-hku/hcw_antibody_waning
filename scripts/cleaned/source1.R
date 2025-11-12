#Code Organization
##Original Data Cleaning and Organization 

#Boost Analysis Workflow 

#Load Packages 
library(librarian)
shelf(dplyr,
      tidyverse,
      readxl, 
      tidyr, 
      rlang, 
      patchwork  
)


#Load Data
data <- read.csv("./data/20251007_hcw_posvax_abwaning.csv")
data <- data %>%
  mutate(across(contains("date"), \(x) as.Date(x, format = "%Y-%m-%d"))) %>%
  mutate(
    one_dose_permutation = paste(dose1_brand),
    two_dose_permutation = paste(dose1_brand, dose2_brand, sep = "-"),
    three_dose_permutation = paste(dose1_brand, dose2_brand, dose3_brand, sep = "-"),
    four_dose_permutation = paste(dose1_brand, dose2_brand, dose3_brand, dose4_brand, sep = "-"),
    five_dose_permutation = paste(dose1_brand, dose2_brand, dose3_brand, dose4_brand, dose5_brand, sep = "-")
  )

community_data <- read.csv("./data/2021_community_elisa_sVNT.csv")

#Load Label Strings for Permutation 1 and 2 
one_labels <- c("1" = " B", "4" = "S")
two_labels <- c("1-1" =" B-B", "4-4" ="S-S")
three_labels_svnt <- c("4-4-1" ="S-S-B", "4-4-4" ="S-S-S", "1-1-1" = "B-B-B", "1-1-4" = "B-B-S")
three_labels_elisa <- c("4-4-1" ="S-S-B", "4-4-4" ="S-S-S", "1-1-1" = "B-B-B")


## Cohort Post-Infection Analysis 2022 
## Community Post-Infection Analysis 2021

#ELISA Values 
#filters for NA in i1_elisa_wt 
#calculates median ELISA post infection value for crossbar 

# Prepare the cohort and community datasets
cohort_data_elisa <- data %>%
  filter(!is.na(i1_elisa_wt)) %>%
  mutate(group = "Cohort") %>%
  select(value = i1_elisa_wt, group)

cohort_data_svnt <- data %>%
  filter(!is.na(i1_svnt_wt)) %>%
  mutate(group = "Cohort") %>%
  select(value = i1_svnt_wt, group)

community_data_elisa <- community_data %>%
  filter(!is.na(elisa)) %>%
  mutate(group = "Community") %>%
  select(value = elisa, group)

community_data_svnt <- community_data %>%
  filter(!is.na(sVNT)) %>%
  mutate(group = "Community") %>%
  select(value = sVNT, group)

# Combine the datasets
community_cohort_elisa_data <- bind_rows(cohort_data_elisa, community_data_elisa)
community_cohort_svnt_data <- bind_rows(cohort_data_svnt, community_data_svnt)

# Calculate median values for each group
community_cohort_elisa_median <- community_cohort_elisa_data %>%
  group_by(group) %>%
  summarize(median = median(value, na.rm = TRUE), .groups = 'drop')

community_cohort_svnt_median <- community_cohort_svnt_data  %>%
  group_by(group) %>%
  summarize(median = median(value, na.rm = TRUE), .groups = 'drop')

###################################

## Boost Analysis

#Make separate data frames for each vaccine permutation
one_dose_data <- data %>%
  filter(!grepl("NA", one_dose_permutation))

two_dose_data <- data %>%
  filter(!grepl("NA", two_dose_permutation))

three_dose_data <- data %>%
  filter(!grepl("NA", three_dose_permutation))

four_dose_data <- data %>%
  filter(!grepl("NA", four_dose_permutation))

five_dose_data <- data %>%
  filter(!grepl("NA", five_dose_permutation))

#########################################

#Baseline Value Measurements

#Source Helper Functions that calculate appropriate antibody titer measurements (sVNT and ELISA) and baseline value measurements 

#Source Baseline Value Measurements Function 
source("scripts/helper/fc_get_baselines.R")

#Calculate Baseline Values for ELISA AND sVNT 
baseline_elisa <- get_baselines(data, "elisa_wt")  
baseline_svnt <- get_baselines(data, "svnt_wt")    

baseline_elisa$baseline_value <- as.numeric(baseline_elisa$baseline_value)
baseline_svnt$baseline_value <- as.numeric(baseline_svnt$baseline_value)

#Calculate Median Values for Baselines 
median_baseline_elisa <- baseline_elisa %>%
  group_by(dose1_brand) %>%
  summarise(median = median(baseline_value, na.rm = TRUE), .groups = 'drop')

median_baseline_svnt <- baseline_svnt %>%
  group_by(dose1_brand) %>%
  summarise(median = median(baseline_value, na.rm = TRUE), .groups = 'drop')


#########################################

#Source Antibody Value Measurement Function
source("scripts/helper/fc_vaccine_perm.R")

#ELISA VALUES 
##ONE
one_elisa_wt <- fc_perm_summary(one_dose_data, one_dose_permutation, v1_elisa_wt, min_n = 5, dose_label = "1 Dose")

one_valid_perms_e      <- one_elisa_wt$valid_perms
one_dose_medians_e     <- one_elisa_wt$medians
one_n_levels_e         <- one_elisa_wt$n_levels
one_mid_x_e            <- one_elisa_wt$mid_x


##TWO 
two_elisa_wt <- fc_perm_summary(two_dose_data, two_dose_permutation, v2_elisa_wt, min_n = 5, dose_label = "2 Dose")

two_valid_perms_e      <- two_elisa_wt$valid_perms
two_dose_medians_e     <- two_elisa_wt$medians
two_n_levels_e         <- two_elisa_wt$n_levels
two_mid_x_e            <- two_elisa_wt$mid_x

#THREE
three_elisa_wt <- fc_perm_summary(three_dose_data, three_dose_permutation, v3_elisa_wt, min_n = 5, dose_label = "3 Dose")

three_valid_perms_e      <- three_elisa_wt$valid_perms
three_dose_medians_e     <- three_elisa_wt$medians
three_n_levels_e         <- three_elisa_wt$n_levels
three_mid_x_e            <- three_elisa_wt$mid_x



#sVNT all Vaccine permutations 
##ONE
one_svnt_wt <- fc_perm_summary(one_dose_data, one_dose_permutation, v1_svnt_wt, min_n = 5, dose_label = "1 Dose")

one_valid_perms      <- one_svnt_wt$valid_perms
one_dose_medians     <- one_svnt_wt$medians
one_n_levels         <- one_svnt_wt$n_levels
one_mid_x            <- one_svnt_wt$mid_x

##TWO 
two_svnt_wt <- fc_perm_summary(two_dose_data, two_dose_permutation, v2_svnt_wt, min_n = 5, dose_label = "2 Dose")

two_valid_perms      <- two_svnt_wt$valid_perms
two_dose_medians     <- two_svnt_wt$medians
two_n_levels         <- two_svnt_wt$n_levels
two_mid_x            <- two_svnt_wt$mid_x

##THREE
three_svnt_wt <- fc_perm_summary(three_dose_data, three_dose_permutation, v3_svnt_wt, min_n = 5, dose_label = "3 Dose")

three_valid_perms      <- three_svnt_wt$valid_perms
three_dose_medians     <- three_svnt_wt$medians
three_n_levels         <- three_svnt_wt$n_levels
three_mid_x            <- three_svnt_wt$mid_x


##FOUR
four_svnt_wt <- fc_perm_summary(four_dose_data, four_dose_permutation, v4_svnt_wt, min_n = 5, dose_label = "4 Doses")

four_valid_perms      <- four_svnt_wt$valid_perms
four_dose_medians     <- four_svnt_wt$medians
four_n_levels         <- four_svnt_wt$n_levels
four_mid_x            <- four_svnt_wt$mid_x
