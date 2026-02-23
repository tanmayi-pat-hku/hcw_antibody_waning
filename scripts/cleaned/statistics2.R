# Source Code 
source("scripts/cleaned/source2.R")
library(librarian)
shelf(robustlmm)



# MIXED EFFECTS MODEL STRUCTURE:
# lmer(log_value ~ days * perm + (1 | id), data = df_d2_continuous, REML = FALSE)

# FIXED EFFECTS COMPONENT:
# - days: Continuous time variable representing days since first dose
#   * Tests the overall waning slope across all participants
# - perm: Categorical variable for vaccine permutation (1-1, 4-4, 1-4, 4-1)
#   * Tests baseline differences between vaccine series
# - days * perm: Interaction term between time and permutation
#   * Tests whether waning RATES differ between vaccine series
#   * This is the key research question: do different vaccines wane at different rates?

# RANDOM EFFECTS COMPONENT:
# - (1 | id): Random intercepts for each participant
#   * Accounts for repeated measures design (multiple time points per person)
#   * Each person has their own baseline antibody level
#   * Controls for correlation within individuals over time
#   * Prevents pseudoreplication by acknowledging non-independence of measurements

# MODEL ASSUMPTIONS:
# 1. LINEARITY: Log-transformed antibody levels decline linearly over time
# 2. NORMALITY: Residuals are normally distributed (improved by log transformation)
# 3. HOMOSCEDASTICITY: Equal variance across time points and groups
# 4. INDEPENDENCE: Observations are independent AFTER accounting for individual random effects
# 5. MISSING DATA: Missing at random (MAR) assumption

# STATISTICAL RATIONALE:
# - Mixed effects model chosen over repeated measures ANOVA because:
#   * Handles unbalanced data (different number of time points per person)
#   * Accommodates irregular measurement intervals
#   * Provides estimates of individual variability
#   * More powerful for longitudinal data with missing observations

# INTERPRETATION:
# - Significant 'days' term: Overall antibody waning occurs
# - Significant 'perm' term: Different vaccine series have different baseline levels
# - Significant 'days * perm' interaction: Waning RATES differ between vaccine series
# - Random effects variance: How much individuals vary in their baseline levels

# LIMITATIONS ACKNOWLEDGED:
# - Assumes linear waning on log scale (may not capture non-linear patterns)
# - Does not account for potential correlation structure in time (AR1)
# - Random intercepts only (could consider random slopes if individuals have different waning rates)


df_d3_ss_b <- four_dose_waning_svnt %>%
  mutate(log_value = log10(pmax(weight, 1)),
         days = days_since_dose1,
         perm = permutation,
         last_dose_brand = as.factor(dose3_brand)) %>%
  filter(!is.na(log_value), !is.na(days), !is.na(id), !is.na(perm))

# Fit mixed effects model including all permutations
model_d3_ss_b <- rlmer(log_value ~ days * perm + (1 | id), data = df_d3_ss_b, REML = FALSE)

# Extract slopes with emtrends
trend_d3_ss_b <- emtrends(model_d3_ss_b, ~ perm, var = "days", infer = TRUE)

table_d3_ss_b <- as.data.frame(trend_d3_ss_b) %>%
  mutate(
    Interval = "After Dose 3",
    `Slope (Δlog10/day)` = round(days.trend, 5),
    lower.CI.percent = round((10^lower.CL - 1) * 100, 3),
    upper.CI.percent = round((10^upper.CL - 1) * 100, 3),
    `Model 95% CI` = paste0("(", lower.CI.percent, ", ", upper.CI.percent, ")"),
    
    `Percent Change per Day` = round((10^days.trend - 1) * 100, 3),
    `Daily Change` = ifelse(days.trend < 0, 
                            paste0("Decreases by ", abs(round((10^days.trend - 1) * 100, 3)), "%"),
                            paste0("Increases by ", round((10^days.trend - 1) * 100, 3), "%")),
    `Half-life (days)` = round(log10(2) / -days.trend),
    `Significant` = ifelse(lower.CI.percent < 0 & upper.CI.percent < 0, "Yes", 
                           ifelse(lower.CI.percent > 0 & upper.CI.percent > 0, "Yes", "No"))
  ) %>%
  select(Interval, perm, `Slope (Δlog10/day)`, `Model 95% CI`, 
         `Percent Change per Day`, `Daily Change`, `Half-life (days)`, `Significant`)

print(table_d3_ss_b)

####################################

# Data Preparation for After Dose 2
df_d2_continuous <- three_dose_waning_svnt %>%
  mutate(log_value = log10(pmax(weight, 1)),
         days = days_since_dose1,
         perm = permutation,
         last_dose_brand = as.factor(dose2_brand)) %>%
  filter(!is.na(log_value), !is.na(days), !is.na(id), !is.na(perm))

model_d2_perm <- lmer(log_value ~ days * perm + (1 | id), data = df_d2_continuous, REML = FALSE)
model_d2_brand <- lmer(log_value ~ days * last_dose_brand + (1 | id), data = df_d2_continuous, REML = FALSE)

trend_d2_perm <- emtrends(model_d2_perm, ~ perm, var = "days", infer = TRUE)
trend_d2_brand <- emtrends(model_d2_brand, ~ last_dose_brand, var = "days", infer = TRUE)

table_d2 <- bind_rows(
  as.data.frame(trend_d2_perm) %>% mutate(Comparison = "By Permutation", Group = perm),
  as.data.frame(trend_d2_brand) %>% mutate(Comparison = "By Brand", Group = as.character(last_dose_brand))
) %>%
  mutate(
    Interval = "After Dose 2",
    `Slope (Δlog10/day)` = round(days.trend, 5),
    
    # Convert model 95% CI to percent
    lower.CI.percent = round((10^lower.CL - 1) * 100, 3),
    upper.CI.percent = round((10^upper.CL - 1) * 100, 3),
    `Model 95% CI` = paste0("(", lower.CI.percent, ", ", upper.CI.percent, ")"),
    
    `Percent Change per Day` = round((10^days.trend - 1) * 100, 3),
    `Daily Change` = ifelse(days.trend < 0, 
                            paste0("Decreases by ", abs(round((10^days.trend - 1) * 100, 3)), "%"),
                            paste0("Increases by ", round((10^days.trend - 1) * 100, 3), "%")),
    `Half-life (days)` = round(log10(2) / -days.trend),
    `Significant` = ifelse(lower.CI.percent < 0 & upper.CI.percent < 0, "Yes", 
                           ifelse(lower.CI.percent > 0 & upper.CI.percent > 0, "Yes", "No"))
  ) %>%
  select(Interval, Comparison, Group, `Slope (Δlog10/day)`, `Model 95% CI`, 
         `Percent Change per Day`, `Daily Change`, `Half-life (days)`, `Significant`)

print(table_d2)
