#Boost Analysis Tests
#source code 
source("scripts/cleaned/source1.R")
source("scripts/helper/fc_group_tests.R")
#####################################

boost_results_table <- bind_rows(
  two_group_test(one_dose_data, one_dose_permutation, v1_elisa_wt, "1", "4", return_table = TRUE),
  two_group_test(two_dose_data, two_dose_permutation, v2_elisa_wt, "1-1", "4-4", return_table = TRUE),
  two_group_test(one_dose_data, one_dose_permutation, v1_svnt_wt, "1", "4", return_table = TRUE),
  two_group_test(two_dose_data, two_dose_permutation, v2_svnt_wt, "1-1", "4-4", return_table = TRUE),
  multi_group_test(three_dose_data, three_dose_permutation, v3_elisa_wt, 
                   c("1-1-1", "4-4-4", "4-4-1"), return_table = TRUE),
  multi_group_test(three_dose_data, three_dose_permutation, v3_svnt_wt,
                   c("1-1-1", "4-4-4", "4-4-1", "1-1-4"), return_table = TRUE),
  multi_group_test(four_dose_data, four_dose_permutation, v4_svnt_wt,
                   c("1-1-1-1", "4-4-4-4", "4-4-4-1", "4-4-1-1"), return_table = TRUE)
)

#Print table
print(boost_results_table)
write.csv(boost_results_table, "boost_stats_table.csv", row.names = FALSE)


