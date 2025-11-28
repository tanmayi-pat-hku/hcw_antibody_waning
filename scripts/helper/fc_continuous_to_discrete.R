create_time_intervals_discrete <- function(data) {
  max_days <- max(data$days_since_dose1, na.rm = TRUE)
  breaks <- seq(0, ceiling(max_days/90)*90, by = 90)
  
  # Create the intervals first
  data <- data %>%
    mutate(
      time_interval = cut(days_since_dose1, 
                          breaks = breaks,
                          include.lowest = TRUE),
      permutation_type = case_when(
        permutation %in% c("1-1-1", "4-4-4", "1-1-1-1", "4-4-4-4") ~ "Homologous",
        permutation %in% c("4-4-1", "1-1-4", "4-4-1-1", "4-4-4-1") ~ "Heterologous",
        TRUE ~ "Other"
      )
    )
  
  # Get the actual levels from the cut() result
  interval_levels <- levels(data$time_interval)
  
  # Create labels that match the actual number of intervals
  n_intervals <- length(interval_levels)
  custom_labels <- paste0(breaks[1:n_intervals], "-", breaks[2:(n_intervals+1)] - 1)
  
  # Apply the labels
  data %>%
    mutate(
      time_interval = factor(time_interval, labels = custom_labels)
    )
}