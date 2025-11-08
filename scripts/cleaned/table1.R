# Load libraries
library(librarian)
shelf(xtable)
shelf(dplyr)

source("scripts/cleaned/source1.R")

#Take appropriate counts 
## Community 

## Cohort Post-Infection Analysis 2022 
count_cohort_elisa <- nrow(cohort_data_elisa) 
count_cohort_svnt <- nrow(cohort_data_svnt)

cohort_counts <- data.frame(
  Cohort_Data = c("Cohort ELISA", "Cohort sVNT"),
  Count = c(count_cohort_elisa, count_cohort_svnt)
)

print(cohort_counts)

##Community Post-Infection Analysis 2021
count_community_elisa <- nrow(community_data_elisa) 
count_commuity_svnt <- nrow(community_data_svnt)

community_counts <- data.frame(
  Community_Data = c("Community ELISA", "Community sVNT"),
  Count = c(count_community_elisa, count_commuity_svnt)
)

print(community_counts)

##Baseline Value Measurements 

baseline_count_elisa <- nrow(baseline_elisa) 
baseline_count_svnt <- nrow(baseline_svnt)

baseline_counts <- data.frame(
  Baseline_Data = c("Baseline ELISA", "Baseline sVNT"),
  Count = c(baseline_count_elisa, baseline_count_svnt)
)

print(baseline_counts)

## ELISA Doses Count 
one_dose_elisa_count <- one_elisa_wt[["valid_perm_count"]] 
two_dose_elisa_count <- two_elisa_wt[["valid_perm_count"]] 
three_dose_elisa_count <- three_elisa_wt[["valid_perm_count"]]


elisa_counts <- list(
  "One Dose ELISA" = one_dose_elisa_count,
  "Two Doses ELISA" = two_dose_elisa_count,
  "Three Doses ELISA" = three_dose_elisa_count
)

print(elisa_counts)

## sVNT Doses Counts 
one_dose_svnt_count <- one_svnt_wt[["valid_perm_count"]] 
two_dose_svnt_count <- two_svnt_wt[["valid_perm_count"]] 
three_dose_svnt_count <- three_svnt_wt[["valid_perm_count"]] 
four_dose_svnt_count <- four_svnt_wt[["valid_perm_count"]] 

svnt_counts <- list(
  "One Dose sVNT" = one_dose_svnt_count,
  "Two Doses sVNT" = two_dose_svnt_count,
  "Three Doses sVNT" = three_dose_svnt_count,
  "Four Doses sVNT" = four_dose_svnt_count
)

print(svnt_counts)

