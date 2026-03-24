# Function to calculate demographic breakdown
calculate_demographic_breakdown <- function(data, perm_col) {
  data %>%
    group_by(!!sym(perm_col)) %>%
    summarise(
      n = n(),
      male_pct = round(sum(male == 1, na.rm = TRUE) / n() * 100, 1),
      female_pct = round(sum(male == 0, na.rm = TRUE) / n() * 100, 1),
      under50_pct = round(sum(age_enrol < 50, na.rm = TRUE) / n() * 100, 1),
      over50_pct = round(sum(age_enrol >= 50, na.rm = TRUE) / n() * 100, 1),
      chronic_pct = round(sum(chronic == 1, na.rm = TRUE) / n() * 100, 1),  # ADDED - using 'chronic'
      .groups = 'drop'
    )
}