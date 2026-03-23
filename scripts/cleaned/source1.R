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

# Create age/sex group bins

data <- data %>%
  mutate(
    age_bin = case_when(
      age_enrol >= 21 & age_enrol <= 35 ~ "Younger Adults (21-35)",
      age_enrol > 35 & age_enrol <= 55 ~ "Adults (35-55)",
      age_enrol > 55 ~ "Older Adults (55+)",
      TRUE ~ NA_character_
    ),
    age_bin = as.factor(age_bin),
    male = as.factor(male)
  )

#Load Label Strings for Permutation 1 and 2 
one_labels <- c("1" = " B", "4" = "S")
two_labels <- c("1-1" =" B-B", "4-4" ="S-S")
three_labels_svnt <- c("4-4-1" ="S-S-B", "4-4-4" ="S-S-S", "1-1-1" = "B-B-B", "1-1-4" = "B-B-S")
three_labels_elisa <- c("4-4-1" ="S-S-B", "4-4-4" ="S-S-S", "1-1-1" = "B-B-B")
four_labels_svnt <- c("1-1-1-1" =" B-B-B-B", "4-4-1-1" =" S-S-B-B", "4-4-4-4" ="S-S-S-S","4-4-4-1" ="S-S-S-B")

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
  filter(!grepl("NA", one_dose_permutation)) %>%
  filter(is.na(posdate_p1) | dose1_date <= posdate_p1)

two_dose_data <- data %>%
  filter(!grepl("NA", two_dose_permutation)) %>%
  filter(is.na(posdate_p1) | dose2_date <= posdate_p1)

three_dose_data <- data %>%
  filter(!grepl("NA", three_dose_permutation)) %>%
filter(is.na(posdate_p1) | dose3_date <= posdate_p1)

four_dose_data <- data %>%
  filter(!grepl("NA", four_dose_permutation)) %>%
  filter(is.na(posdate_p1) | dose4_date <= posdate_p1)

five_dose_data <- data %>%
  filter(!grepl("NA", five_dose_permutation)) %>%
  filter(is.na(posdate_p1) | dose5_date <= posdate_p1)

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


# ELISA VALUES

##ONE (v1_date is the vaccine date for 1 dose)
one_elisa_wt <- fc_perm_summary(one_dose_data, one_dose_permutation, v1_elisa_wt, 
                                min_n = 5, dose_label = "1 Dose", 
                                vaccine_date_col = "v1_date", infection_window_days = 180)

##TWO (v2_date is the vaccine date for 2 doses)
two_elisa_wt <- fc_perm_summary(two_dose_data, two_dose_permutation, v2_elisa_wt, 
                                min_n = 5, dose_label = "2 Dose",
                                vaccine_date_col = "v2_date", infection_window_days = 180)

#THREE (v3_date is the vaccine date for 3 doses)
three_elisa_wt <- fc_perm_summary(three_dose_data, three_dose_permutation, v3_elisa_wt, 
                                  min_n = 5, dose_label = "3 Dose",
                                  vaccine_date_col = "v3_date", infection_window_days = 180)

# sVNT values

##ONE (v1_date is the vaccine date for 1 dose)
one_svnt_wt <- fc_perm_summary(one_dose_data, one_dose_permutation, v1_svnt_wt, 
                               min_n = 5, dose_label = "1 Dose",
                               vaccine_date_col = "v1_date", infection_window_days = 180)

##TWO (v2_date is the vaccine date for 2 doses)
two_svnt_wt <- fc_perm_summary(two_dose_data, two_dose_permutation, v2_svnt_wt, 
                               min_n = 5, dose_label = "2 Dose",
                               vaccine_date_col = "v2_date", infection_window_days = 180)

##THREE (v3_date is the vaccine date for 3 doses)
three_svnt_wt <- fc_perm_summary(three_dose_data, three_dose_permutation, v3_svnt_wt, 
                                 min_n = 5, dose_label = "3 Dose",
                                 vaccine_date_col = "v3_date", infection_window_days = 180)

##FOUR (v4_date is the vaccine date for 4 doses)
four_svnt_wt <- fc_perm_summary(four_dose_data, four_dose_permutation, v4_svnt_wt, 
                                min_n = 5, dose_label = "4 Dose",
                                vaccine_date_col = "v4_date", infection_window_days = 180)




