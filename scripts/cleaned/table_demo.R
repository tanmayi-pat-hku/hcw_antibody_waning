library(dplyr)
library(rlang) 

source("scripts/helper/fc_demographic.R")
source("scripts/cleaned/source1.R")


# For 1 Dose
one_dose_demographics <- calculate_demographic_breakdown(one_dose_data, "one_dose_permutation")
print("Demographic Breakdown for 1 Dose:")
print(one_dose_demographics)

# For 2 Doses
two_dose_demographics <- calculate_demographic_breakdown(two_dose_data, "two_dose_permutation")
print("Demographic Breakdown for 2 Doses:")
print(two_dose_demographics)

# For 3 Doses
three_dose_demographics <- calculate_demographic_breakdown(three_dose_data, "three_dose_permutation")
print("Demographic Breakdown for 3 Doses:")
print(three_dose_demographics)

# Calculate total demographics
total_demographics <- calculate_total_demographics(data)
print("Total Counts in the Dataset:")
print(total_demographics)