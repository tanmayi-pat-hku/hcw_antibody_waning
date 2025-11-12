create_time_intervals <- function(data) {
  max_days <- max(data$days_since_dose1, na.rm = TRUE)
  breaks <- seq(0, ceiling(max_days/90)*90, by = 90)
  
  data %>%
    mutate(
      time_interval = cut(days_since_dose1, 
                          breaks = breaks,
                          include.lowest = TRUE),
      permutation_type = case_when(
        permutation %in% c("1-1-1", "4-4-4", "1-1-1-1", "4-4-4-4") ~ "Homologous",
        permutation %in% c("4-4-1", "1-1-4", "4-4-1-1", "4-4-4-1") ~ "Heterologous",
        TRUE ~ "Other"
      )
    ) %>%
    mutate(
      time_interval = factor(time_interval,
                             labels = paste0(breaks[-length(breaks)], "-", breaks[-1] - 1))
    )
}
