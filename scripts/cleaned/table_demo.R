source("scripts/helper/fc_demographic.R")
source("scripts/cleaned/source1.R")

library(librarian)
shelf(tidyverse)

#Calculate for each dose group
one_dose_demo <- calculate_demographic_breakdown(one_dose_data, "one_dose_permutation")
two_dose_demo <- calculate_demographic_breakdown(two_dose_data, "two_dose_permutation")
three_dose_demo <- calculate_demographic_breakdown(three_dose_data, "three_dose_permutation")

#Calculate totals
total_n <- nrow(data)
total_demo <- data.frame(
  one_dose_permutation = "Total",
  n = total_n,
  male_pct = round(sum(data$male == 1, na.rm = TRUE) / total_n * 100, 1),
  female_pct = round(sum(data$male == 0, na.rm = TRUE) / total_n * 100, 1),
  under50_pct = round(sum(data$age_enrol < 50, na.rm = TRUE) / total_n * 100, 1),
  over50_pct = round(sum(data$age_enrol >= 50, na.rm = TRUE) / total_n * 100, 1),
  chronic_pct = round(sum(data$chronic == 1, na.rm = TRUE) / total_n * 100, 1)  # ADDED - using 'chronic'
)

one_dose_pct <- one_dose_demo %>%
  mutate(pct_of_total = round(n / total_n * 100, 1))

two_dose_pct <- two_dose_demo %>%
  mutate(pct_of_total = round(n / total_n * 100, 1))

three_dose_pct <- three_dose_demo %>%
  mutate(pct_of_total = round(n / total_n * 100, 1))

#Organize
one_dose_demo <- one_dose_demo %>%
  mutate(
    Dose = "One-dose",
    Group = recode(one_dose_permutation, !!!one_labels)
  )

two_dose_demo <- two_dose_demo %>%
  mutate(
    Dose = "Two-dose",
    Group = recode(two_dose_permutation, !!!two_labels)
  )

three_dose_demo <- three_dose_demo %>%
  mutate(
    Dose = "Three-dose",
    Group = recode(three_dose_permutation, !!!three_labels_svnt)
  )

# Combine all
demographic_table <- bind_rows(
  one_dose_demo %>% select(Dose, Group, n, male_pct, female_pct, under50_pct, over50_pct, chronic_pct) %>% mutate(pct_of_total = one_dose_pct$pct_of_total),
  two_dose_demo %>% select(Dose, Group, n, male_pct, female_pct, under50_pct, over50_pct, chronic_pct) %>% mutate(pct_of_total = two_dose_pct$pct_of_total),
  three_dose_demo %>% select(Dose, Group, n, male_pct, female_pct, under50_pct, over50_pct, chronic_pct) %>% mutate(pct_of_total = three_dose_pct$pct_of_total),
  total_demo %>% mutate(Dose = "Total", Group = "", pct_of_total = 100) %>% select(Dose, Group, n, male_pct, female_pct, under50_pct, over50_pct, chronic_pct, pct_of_total)
)

# Print
print(demographic_table)

# Save to CSV
write.csv(demographic_table, "table1_demographic.csv", row.names = FALSE)