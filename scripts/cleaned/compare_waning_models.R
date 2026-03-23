source("scripts/cleaned/power_law_waning.R")

# Simple AIC/BIC comparison between linear, power law, and exponential models
# Including age and sex adjustments

# Prepare data with log transformations
data_dose2 <- three_dose_waning_svnt %>%
  mutate(
    log_days = log(days_since_dose1 + 0.01),
    log_weight = log(weight + 0.01),
    permutation = as.factor(permutation),
    male = as.factor(male),
    age_bin = as.factor(age_bin)
  )

data_dose3 <- four_dose_waning_svnt %>%
  mutate(
    log_days = log(days_since_dose1 + 0.01),
    log_weight = log(weight + 0.01),
    permutation = as.factor(permutation),
    male = as.factor(male),
    age_bin = as.factor(age_bin)
  )

# Fit models with age and sex

# Linear: weight ~ days
linear_d2 <- lmer(weight ~ days_since_dose1 * permutation + male + age_bin + (1 | id), 
                  data = data_dose2,
                  control = lmerControl(optimizer = "bobyqa"))

linear_d3 <- lmer(weight ~ days_since_dose1 * permutation + male + age_bin + (1 | id), 
                  data = data_dose3,
                  control = lmerControl(optimizer = "bobyqa"))

# Power law: log(weight) ~ log(days)
power_d2 <- lmer(log_weight ~ log_days * permutation + male + age_bin + (1 | id), 
                 data = data_dose2,
                 control = lmerControl(optimizer = "bobyqa"))

power_d3 <- lmer(log_weight ~ log_days * permutation + male + age_bin + (1 | id), 
                 data = data_dose3,
                 control = lmerControl(optimizer = "bobyqa"))

# Exponential: log(weight) ~ days
exp_d2 <- lmer(log_weight ~ days_since_dose1 * permutation + male + age_bin + (1 | id), 
               data = data_dose2,
               control = lmerControl(optimizer = "bobyqa"))

exp_d3 <- lmer(log_weight ~ days_since_dose1 * permutation + male + age_bin + (1 | id), 
               data = data_dose3,
               control = lmerControl(optimizer = "bobyqa"))

# Compare
cat("\n========== DOSE 2 ==========\n")
cat("Linear (weight ~ days):           AIC =", round(AIC(linear_d2), 1), " BIC =", round(BIC(linear_d2), 1), "\n")
cat("Power law (log weight ~ log days): AIC =", round(AIC(power_d2), 1), " BIC =", round(BIC(power_d2), 1), "\n")
cat("Exponential (log weight ~ days):  AIC =", round(AIC(exp_d2), 1), " BIC =", round(BIC(exp_d2), 1), "\n")

cat("\n========== DOSE 3 ==========\n")
cat("Linear (weight ~ days):           AIC =", round(AIC(linear_d3), 1), " BIC =", round(BIC(linear_d3), 1), "\n")
cat("Power law (log weight ~ log days): AIC =", round(AIC(power_d3), 1), " BIC =", round(BIC(power_d3), 1), "\n")
cat("Exponential (log weight ~ days):  AIC =", round(AIC(exp_d3), 1), " BIC =", round(BIC(exp_d3), 1), "\n")