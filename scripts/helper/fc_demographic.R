# Function to calculate gender and age breakdown by permutation
calculate_demographic_breakdown <- function(data, permutation_column) {
  data %>%
    filter(grepl("^(1|4|1-1|4-4|1-1-1|4-4-4|4-4-1|1-1-4)$", !!sym(permutation_column))) %>%
    group_by(!!sym(permutation_column)) %>%
    summarise(
      total_count = n(),  # Total count for the permutation
      count_male = sum(male == 1, na.rm = TRUE),  # Count males
      count_female = sum(male == 0, na.rm = TRUE),  # Count females
      count_over_50 = sum(age_enrol >= 50, na.rm = TRUE),  # Count over 50
      count_under_50 = sum(age_enrol < 50, na.rm = TRUE),  # Count under 50
      percentage_male = (count_male / total_count) * 100,  # Calculate male percentage
      percentage_female = (count_female / total_count) * 100,  # Calculate female percentage
      percentage_over_50 = (count_over_50 / total_count) * 100,  # Calculate % over 50
      percentage_under_50 = (count_under_50 / total_count) * 100,  # Calculate % under 50
      .groups = 'drop'
    )
}

calculate_total_demographics <- function(data) {
  total_count <- nrow(data)  # Total number of individuals
  
  data %>%
    summarise(
      total_male = sum(male == 1, na.rm = TRUE),  
      total_female = sum(male == 0, na.rm = TRUE),  
      total_under_50 = sum(age_enrol < 50, na.rm = TRUE), 
      total_over_50 = sum(age_enrol >= 50, na.rm = TRUE),
      percent_male = (total_male / total_count) * 100,        # Calculate male percentage
      percent_female = (total_female / total_count) * 100,    # Calculate female percentage
      percent_under_50 = (total_under_50 / total_count) * 100, # Calculate percentage under 50
      percent_over_50 = (total_over_50 / total_count) * 100      # Calculate percentage over 50
    )
}