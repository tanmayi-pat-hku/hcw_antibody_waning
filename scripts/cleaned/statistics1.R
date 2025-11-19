#Boost Analysis Tests
#source code 
source("scripts/cleaned/source1.R")
source("scripts/helper/fc_two_group_test.R")
source("scripts/helper/fc_multi_group_test.R")
#####################################

# Boost Analysis Results 

## WILCOXON TEST

##ELISA TESTS WILCOXON 
two_group_test(one_dose_data,   one_dose_permutation, v1_elisa_wt, "1", "4")
two_group_test(two_dose_data,   two_dose_permutation, v2_elisa_wt, "1-1", "4-4")

##sVNT TESTS WILCOXON 
two_group_test(one_dose_data,   one_dose_permutation, v1_svnt_wt, "1", "4")
two_group_test(two_dose_data,   two_dose_permutation, v2_svnt_wt, "1-1", "4-4")

## KRUSKAL TESTS 

##ELISA TESTS KW - 3 Dose
multi_group_test(three_dose_data, three_dose_permutation, v3_elisa_wt, groups = c("1-1-1", "4-4-4", "4-4-1"))


##sVNT TESTS KW - 3 Dose
multi_group_test(three_dose_data, three_dose_permutation, v3_svnt_wt, groups = c("1-1-1", "4-4-4", "4-4-1", "1-1-4"))

##sVNT TESTS KW - 4 Dose
multi_group_test(four_dose_data, four_dose_permutation, v4_svnt_wt, groups = c("1-1-1-1", "4-4-4-4", "4-4-4-1", "4-4-1-1"))


