# Source Code 
source("scripts/cleaned/source2.R")
source("scripts/helper/fc_bootstrap_ci.R")

library(librarian)
shelf(lme4)
shelf(lmerTest)
shelf(emmeans)
shelf(boot)

# MIXED EFFECTS MODELS FOR CONTINUOUS WANING SLOPES

# After Dose 2 
df_d2_continuous <- three_dose_waning_svnt %>%
  mutate(log_value = log10(pmax(weight, 1)),
         days = days_since_dose1,
         perm = permutation,
         last_dose_brand = as.factor(dose2_brand)) %>%
  filter(!is.na(log_value), !is.na(days), !is.na(id), !is.na(perm))

# Tests if slopes differ by vaccine series
model_d2_perm <- lmer(log_value ~ days * perm + (1 | id), data = df_d2_continuous, REML = FALSE)

##EXTRA
model_d2_brand <- lmer(log_value ~ days * last_dose_brand + (1 | id), data = df_d2_continuous, REML = FALSE)

# Extract slope comparisons
trend_d2_perm <- emtrends(model_d2_perm, ~ perm, var = "days", infer = TRUE)
trend_d2_brand <- emtrends(model_d2_brand, ~ last_dose_brand, var = "days", infer = TRUE)  

# AFTER DOSE 3 
df_d3_continuous <- four_dose_waning_svnt %>%
  mutate(log_value = log10(pmax(weight, 1)),
         days = days_since_dose1,
         perm = permutation,
         last_dose_brand = as.factor(dose3_brand)) %>%
  filter(!is.na(log_value), !is.na(days), !is.na(id), !is.na(perm))

df_d3_homologous <- df_d3_continuous %>% filter(perm %in% c("4-4-4", "1-1-1"))

# Models for dose 3
model_d3_perm <- lmer(log_value ~ days * perm + (1 | id), data = df_d3_homologous, REML = FALSE)

##EXTRA
model_d3_brand <- lmer(log_value ~ days * last_dose_brand + (1 | id), data = df_d3_homologous, REML = FALSE)

# Extract slopes
trend_d3_perm <- emtrends(model_d3_perm, ~ perm, var = "days", infer = TRUE)
trend_d3_brand <- emtrends(model_d3_brand, ~ last_dose_brand, var = "days", infer = TRUE)


# RESULTS TABLES WITH BOOTSTRAP CIs

# Table for After Dose 2
table_d2 <- bind_rows(
  as.data.frame(trend_d2_perm) %>% mutate(Comparison = "By Permutation", Group = perm),
  as.data.frame(trend_d2_brand) %>% mutate(Comparison = "By Brand", Group = as.character(last_dose_brand))
) %>%
  mutate(
    Interval = "After Dose 2",
    `Slope (Δlog10/day)` = round(days.trend, 5),
    `Model 95% CI` = paste0("(", round(lower.CL, 4), ", ", round(upper.CL, 4), ")"),
    `Bootstrap 95% CI` = ifelse(
      Comparison == "By Permutation" & Group %in% c("1-1", "4-4"),
      paste0("(", round(boot_d2_perm$ci_lower, 4), ", ", round(boot_d2_perm$ci_upper, 4), ")"),
      `Model 95% CI`  # Use model CI for other groups
    ),
    `Percent Change per Day` = round((10^days.trend - 1) * 100, 3),
    `Daily Change` = ifelse(days.trend < 0, 
                            paste0("Decreases by ", abs(round((10^days.trend - 1) * 100, 3)), "%"),
                            paste0("Increases by ", round((10^days.trend - 1) * 100, 3), "%")),
    `Half-life (days)` = round(log10(2) / -days.trend),
    `Significant` = ifelse(lower.CL < 0 & upper.CL < 0, "Yes", 
                           ifelse(lower.CL > 0 & upper.CL > 0, "Yes", "No"))
  ) %>%
  select(Interval, Comparison, Group, `Slope (Δlog10/day)`, `Model 95% CI`, `Bootstrap 95% CI`,
         `Percent Change per Day`, `Daily Change`, `Half-life (days)`, `Significant`)

# Table for After Dose 3  
table_d3 <- bind_rows(
  as.data.frame(trend_d3_perm) %>% mutate(Comparison = "By Permutation", Group = perm),
  as.data.frame(trend_d3_brand) %>% mutate(Comparison = "By Brand", Group = as.character(last_dose_brand))
) %>%
  mutate(
    Interval = "After Dose 3",
    `Slope (Δlog10/day)` = round(days.trend, 5),
    `Model 95% CI` = paste0("(", round(lower.CL, 4), ", ", round(upper.CL, 4), ")"),
    `Bootstrap 95% CI` = ifelse(
      Comparison == "By Permutation" & Group %in% c("1-1-1", "4-4-4"),
      paste0("(", round(boot_d3_perm$ci_lower, 4), ", ", round(boot_d3_perm$ci_upper, 4), ")"),
      `Model 95% CI`  
    ),
    `Percent Change per Day` = round((10^days.trend - 1) * 100, 3),
    `Daily Change` = ifelse(days.trend < 0, 
                            paste0("Decreases by ", abs(round((10^days.trend - 1) * 100, 3)), "%"),
                            paste0("Increases by ", round((10^days.trend - 1) * 100, 3), "%")),
    `Half-life (days)` = round(log10(2) / -days.trend),
    `Significant` = ifelse(lower.CL < 0 & upper.CL < 0, "Yes", 
                           ifelse(lower.CL > 0 & upper.CL > 0, "Yes", "No"))
  ) %>%
  select(Interval, Comparison, Group, `Slope (Δlog10/day)`, `Model 95% CI`, `Bootstrap 95% CI`,
         `Percent Change per Day`, `Daily Change`, `Half-life (days)`, `Significant`)

# Final combined table
final_slope_table <- bind_rows(table_d2, table_d3)

# FIXED TABLE USING MODEL CIs FOR SIGNIFICANCE
final_slope_table <- final_slope_table %>%
  mutate(
    # Extract numeric values from MODEL CI
    ci_lower = as.numeric(gsub(".*\\(([^,]+).*", "\\1", `Model 95% CI`)),
    ci_upper = as.numeric(gsub(".*, ([^)]+).*", "\\1", `Model 95% CI`)),
    # Correct significance based on MODEL CI
    `Significant` = ifelse(ci_lower < 0 & ci_upper < 0, "Yes", 
                           ifelse(ci_lower > 0 & ci_upper > 0, "Yes", "No"))
  ) 



##Type III ANOVA Between Slopes

# Filter
df_d2_homologous <- df_d2_continuous %>% filter(perm %in% c("1-1", "4-4"))

# Fit model with only those two groups
model_d2_homo <- lmer(log_value ~ days * perm + (1 | id), data = df_d2_homologous, REML = FALSE)

anova_results <- anova(model_d2_homo)
interaction_p <- anova_results["days:perm", "Pr(>F)"] #2.333232e-06


# After Dose 3 - Test 1-1-1 vs 4-4-4
df_d3_homologous <- df_d3_continuous %>% filter(perm %in% c("1-1-1", "4-4-4"))

# Fit model with only those two groups
model_d3_homo <- lmer(log_value ~ days * perm + (1 | id), data = df_d3_homologous, REML = FALSE)

# Test
anova_results_d3 <- anova(model_d3_homo)
interaction_p_d3 <- anova_results_d3["days:perm", "Pr(>F)"] #6.563022e-14 




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


df_d3_ss_b <- df_d3_continuous

model_d3_ss_b <- lmer(log_value ~ days * perm + (1 | id), data = df_d3_ss_b, REML = FALSE)

trend_d3_ss_b <- emtrends(model_d3_ss_b, ~ perm, var = "days", infer = TRUE)

# Use the full dataset for the analysis
df_d3_ss_b <- df_d3_continuous

# Fit mixed effects model including all permutations
model_d3_ss_b <- lmer(log_value ~ days * perm + (1 | id), data = df_d3_ss_b, REML = FALSE)

# Extract slopes with emtrends
trend_d3_ss_b <- emtrends(model_d3_ss_b, ~ perm, var = "days", infer = TRUE)

# Create the results table combining everything
table_d3_ss_b <- as.data.frame(trend_d3_ss_b) %>%
  mutate(
    Interval = "After Dose 3",
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
  select(Interval, perm, `Slope (Δlog10/day)`, `Model 95% CI`, 
         `Percent Change per Day`, `Daily Change`, `Half-life (days)`, `Significant`)

# Display the final table
print(table_d3_ss_b)





# Load necessary packages only once
library(librarian)
shelf(lme4)
shelf(lmerTest)
shelf(emmeans)

# Data Preparation for After Dose 2
df_d2_continuous <- three_dose_waning_svnt %>%
  mutate(log_value = log10(pmax(weight, 1)),
         days = days_since_dose1,
         perm = permutation,
         last_dose_brand = as.factor(dose2_brand)) %>%
  filter(!is.na(log_value), !is.na(days), !is.na(id), !is.na(perm))

# Fit mixed effects model for Dose 2
model_d2_perm <- lmer(log_value ~ days * perm + (1 | id), data = df_d2_continuous, REML = FALSE)
model_d2_brand <- lmer(log_value ~ days * last_dose_brand + (1 | id), data = df_d2_continuous, REML = FALSE)

# Extract slope comparisons for Dose 2
trend_d2_perm <- emtrends(model_d2_perm, ~ perm, var = "days", infer = TRUE)
trend_d2_brand <- emtrends(model_d2_brand, ~ last_dose_brand, var = "days", infer = TRUE)

# Create the results table for Dose 2
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

# Display the final table for Dose 2
print(table_d2)