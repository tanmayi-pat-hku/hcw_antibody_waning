source("scripts/cleaned/source2.R")
source("scripts/helper/fc_power_law_bootstrap.R")

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
after_dose2_adj <- fit_powerlaw_with_boot_adj(three_dose_waning_svnt, n_boot = 500)
after_dose3_adj <- fit_powerlaw_with_boot_adj(four_dose_waning_svnt, n_boot = 500)


# CREATE MARGINAL PREDICTIONS FOR PLOTS across age bins and sex 
dose2_marginal <- create_marginal_predictions(after_dose2_adj)
dose3_marginal <- create_marginal_predictions(after_dose3_adj)

#Post-Infection: adjusted models with interactions
post_infection_adj <- fit_powerlaw_infection(post_infection_svnt, n_boot = 500) 

# CREATE MARGINAL PREDICTIONS FOR PLOTS across age bins and sex 
post_infection_marginal <- create_marginal_predictions_infection(post_infection_adj)