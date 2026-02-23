




###################################################
# Spline Fitting (cubic spline models)
# - Early phase 
# - Middle phase
# - Late phase 
###################################################
source("scripts/cleaned/source2.R")

library(librarian)
shelf(splines)
shelf(lme4)
shelf(emmeans)
shelf(ggeffects)
shelf(ggplot2)
shelf(dplyr)
shelf(kableExtra)

focal_permutations <- c("1-1-1", "4-4-4", "4-4-1")

# After Third Dose

# Prepare data for Dose 3 analysis
df_d3_ss_b <- four_dose_waning_svnt %>%
  filter(permutation %in% focal_permutations) %>%
  mutate(log_value = log10(pmax(weight, 1)),
         days = days_since_dose1,
         perm = factor(permutation, levels = focal_permutations),
         last_dose_brand = as.factor(dose3_brand)) %>%
  filter(!is.na(log_value), !is.na(days), !is.na(id), !is.na(perm))

print(table(df_d3_ss_b$perm))

# Fit Spline (3 degrees of freedom; 4 knots)

model_d3_spline <- lmer(log_value ~ ns(days, df = 3) * perm + (1 | id), 
                        data = df_d3_ss_b, REML = FALSE)

# Fit linear model for comparison (to test if spline is significantly better)
model_d3_linear <- lmer(log_value ~ days * perm + (1 | id), 
                        data = df_d3_ss_b, REML = FALSE)


# CALCULATE WANING RATES FROM SPLINE MODEL
intervals <- list(
  `Early (0-60 days)` = c(0, 60),      
  `Middle (61-180 days)` = c(61, 180), 
  `Late (181-365 days)` = c(181, 365),  
  `Overall (0-365 days)` = c(0, 365)  
)

# Function to calculate average slope from spline over a time interval
calculate_avg_slope <- function(model, perm_level, start_day, end_day) {
  days_seq <- seq(start_day, end_day, length.out = 500)
  
  pred_vals <- predict(model, 
                       newdata = data.frame(days = days_seq, 
                                            perm = perm_level,
                                            id = NA),
                       re.form = NA)  
  
  # Calculate total change and average slope
  total_change <- tail(pred_vals, 1) - head(pred_vals, 1)
  avg_slope <- total_change / (end_day - start_day)
  
  return(avg_slope)
}


# CREATE SUMMARY TABLE WITH EXPONENTIATED VALUES

spline_results_d3 <- data.frame()

for(perm_level in focal_permutations) {
  for(interval_name in names(intervals)) {
    interval <- intervals[[interval_name]]
    
    # Only calculate if we have sufficient data in this interval
    data_count <- sum(df_d3_ss_b$days >= interval[1] & 
                        df_d3_ss_b$days <= interval[2] & 
                        df_d3_ss_b$perm == perm_level)
    
    if(data_count > 5) {  # Require at least 5 observations
      avg_slope <- calculate_avg_slope(model_d3_spline, perm_level, 
                                       interval[1], interval[2])
      
      spline_results_d3 <- rbind(spline_results_d3, data.frame(
        Interval = "After Dose 3",
        Phase = interval_name,
        Permutation = perm_level,
        avg_slope_log10 = avg_slope,
        n_observations = data_count
      ))
    }
  }
}


# EXPONENTIATE AND FORMAT RESULTS FOR INTERPRETATION

# Create phase-grouped summary
phase_summary <- spline_results_d3 %>%
  mutate(
    Vaccine_Series = case_when(
      Permutation == "1-1-1" ~ "B-B-B",
      Permutation == "4-4-4" ~ "S-S-S",
      Permutation == "4-4-1" ~ "S-S-B",
      TRUE ~ Permutation
    ),
    `% Change/Day` = (10^avg_slope_log10 - 1) * 100,
    `Half-life (days)` = ifelse(avg_slope_log10 < 0, 
                                log10(2) / -avg_slope_log10, 
                                NA),
    Phase_Simple = gsub(" \\(.*\\)", "", Phase)  # Remove parentheses part
  ) %>%
  select(Phase_Simple, Vaccine_Series, `% Change/Day`, `Half-life (days)`, N = n_observations) %>%
  pivot_wider(
    names_from = Vaccine_Series,
    values_from = c(`% Change/Day`, `Half-life (days)`, N),
    names_glue = "{Vaccine_Series} {.value}"
  ) %>%
  # Reorder phases
  mutate(Phase_Simple = factor(Phase_Simple, 
                               levels = c("Early", "Middle", "Late", "Overall"))) %>%
  arrange(Phase_Simple)

# Create formatted kable
phase_kable <- phase_summary %>%
  mutate(
    across(where(is.numeric), ~sprintf("%.3f", .))
  ) %>%
  kable(
    format = "html",
    align = c("l", rep(c("r", "r", "r"), 3)),
    caption = "Table 1: Phase-Specific Antibody Waning Rates After Third Dose"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = TRUE,
    font_size = 11
  ) %>%
  add_header_above(
    c(" " = 1,
      "Change/Day" = 3,
      "Half-life" = 3,
      "Series" = 3)
  ) %>%
  column_spec(1, bold = TRUE, width = "10%") %>%
  # Color code: red for negative % change (waning), green for positive
  column_spec(c(2,5,8), 
              color = ifelse(as.numeric(phase_summary$`B-B-B % Change/Day`) < 0, "red", "green"))

##### AFTER THIRD DOSE TABLE
print(phase_kable)


###################################################
# After Second Dose Spline Analysis
###################################################

# Filter to homologous series for dose 2 analysis (B-B and S-S only)
df_d2_continuous <- three_dose_waning_svnt %>%
  filter(permutation %in% c("1-1", "4-4")) %>%
  mutate(log_value = log10(pmax(weight, 1)),
         days = days_since_dose1,
         perm = factor(permutation, levels = c("1-1", "4-4"))) %>%
  filter(!is.na(log_value), !is.na(days), !is.na(id), !is.na(perm))

# Fit Spline model for Dose 2
model_d2_spline <- lmer(log_value ~ ns(days, df = 3) * perm + (1 | id), 
                        data = df_d2_continuous, REML = FALSE)

# CREATE DOSE 2 SUMMARY TABLE
spline_results_d2 <- data.frame()

for(perm_level in c("1-1", "4-4")) {
  for(interval_name in names(intervals)) {
    interval <- intervals[[interval_name]]
    
    # Only calculate if we have sufficient data
    data_count <- sum(df_d2_continuous$days >= interval[1] & 
                        df_d2_continuous$days <= interval[2] & 
                        df_d2_continuous$perm == perm_level)
    
    if(data_count > 5) {
      avg_slope <- calculate_avg_slope(model_d2_spline, perm_level, 
                                       interval[1], interval[2])
      
      spline_results_d2 <- rbind(spline_results_d2, data.frame(
        Interval = "After Dose 2",
        Phase = interval_name,
        Permutation = perm_level,
        avg_slope_log10 = avg_slope,
        n_observations = data_count
      ))
    }
  }
}

# Create phase-grouped summary for Dose 2
phase_summary_d2 <- spline_results_d2 %>%
  mutate(
    Vaccine_Series = case_when(
      Permutation == "1-1" ~ "B-B",
      Permutation == "4-4" ~ "S-S",
      TRUE ~ Permutation
    ),
    `% Change/Day` = (10^avg_slope_log10 - 1) * 100,
    `Half-life (days)` = ifelse(avg_slope_log10 < 0, 
                                log10(2) / -avg_slope_log10, 
                                NA),
    Phase_Simple = gsub(" \\(.*\\)", "", Phase)
  ) %>%
  select(Phase_Simple, Vaccine_Series, `% Change/Day`, `Half-life (days)`, N = n_observations) %>%
  pivot_wider(
    names_from = Vaccine_Series,
    values_from = c(`% Change/Day`, `Half-life (days)`, N),
    names_glue = "{Vaccine_Series} {.value}"
  ) %>%
  mutate(Phase_Simple = factor(Phase_Simple, 
                               levels = c("Early", "Middle", "Late", "Overall"))) %>%
  arrange(Phase_Simple)

# Create formatted kable for Dose 2
phase_kable_d2 <- phase_summary_d2 %>%
  mutate(
    across(where(is.numeric), ~sprintf("%.3f", .))
  ) %>%
  kable(
    format = "html",
    align = c("l", rep(c("r", "r", "r"), 2)),
    caption = "Table 2: Phase-Specific Antibody Waning Rates After Second Dose"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = TRUE,
    font_size = 11
  ) %>%
  add_header_above(
    c(" " = 1,
      "Change/Day" = 2,
      "Half-life" = 2,
      "Series" = 2)
  ) %>%
  column_spec(1, bold = TRUE, width = "10%") %>%
  column_spec(2, color = ifelse(as.numeric(phase_summary_d2[[2]]) < 0, "red", "darkgreen")) %>%
  column_spec(5, color = ifelse(as.numeric(phase_summary_d2[[5]]) < 0, "red", "darkgreen"))
###########

##### AFTER SECOND DOSE TABLE
print(phase_kable_d2)

##### AFTER THIRD DOSE TABLE
print(phase_kable)
