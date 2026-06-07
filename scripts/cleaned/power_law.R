#Set Working Directory for Stand alone (For Wey Wen Dropbox)
#setwd("~/Desktop/Shared COVID HCW antibody waning/2025_10_hcw_abwaning")

source("scripts/cleaned/source2.R")
#source("output/scripts/cleaned/source2.R") #For Wey Wen Dropbox
source("scripts/helper/fc_power_law_bootstrap.R")
#source("output/scripts/helper/fc_power_law_bootstrap.R") #For Wey Wen Dropbox

# Formula: 
#   log_weight ~ log_days * permutation + male + age_bin + 
#               log_days:male + log_days:age_bin + (1 | id)

# Coefficient = power law exponent (waning rate)
#   Negative value = antibodies decrease over time
#   The exponent tells you HOW FAST antibodies decline (more negative = faster waning)

# Fixed Effects Structure:
#   1. log_days:                    Base waning rate (reference group: B-B, female, age 21-40)
#   2. log_days:permutation:        Allows waning rate to differ by vaccine series
#   3. male + age_bin:              Baseline differences by sex and age group
#   4. log_days:male:               Tests if waning rate differs by sex
#   5. log_days:age_bin:            Tests if waning rate differs by age group

# Random Effects:
#   (1 | id): Random intercept for each participant
#     - Accounts for repeated measurements within individuals
#     - Different starting antibody levels for each person
#     - Captures individual variation beyond fixed effects


# Post-vaccinatation: Run adjusted models with interactions
# Fit the power‑law model to the post‑vaccination waning datasets.
after_dose2_adj <- fit_powerlaw_with_boot_adj(two_dose_waning_svnt, n_boot = 500, time_offset = 0.01)
after_dose3_adj <- fit_powerlaw_with_boot_adj(three_dose_waning_svnt, n_boot = 500, time_offset = 0.01)

# CREATE MARGINAL PREDICTIONS FOR PLOTS across age bins and sex 
# Marginal predictions are population‑averaged curves that account for the
# observed distribution of sex and age in the data.  The time_offset MUST
# match the one used in the model fitting.
dose2_marginal <- create_marginal_predictions(after_dose2_adj, time_offset = 0.01)
dose3_marginal <- create_marginal_predictions(after_dose3_adj, time_offset = 0.01)

#Post-Infection: adjusted models with interactions
# No permutation term here – only one group (post‑infection).
# Uses a fixed 0.01 offset internally.
post_infection_adj <- fit_powerlaw_infection(post_infection_svnt, n_boot = 500) 

# CREATE MARGINAL PREDICTIONS FOR PLOTS across age bins and sex 
post_infection_marginal <- create_marginal_predictions_infection(post_infection_adj)