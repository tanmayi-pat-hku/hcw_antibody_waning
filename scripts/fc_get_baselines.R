get_baselines <- function(data, value_type, group_vars = NULL) {
  date_pattern <- "^r\\d+_date$"
  value_pattern <- paste0("^r\\d+_", value_type, "$")
  index_pattern <- paste0("r(\\d+)_", value_type)
  
  long_data <- data %>%
    pivot_longer(
      cols = matches(date_pattern),
      names_to = "r_index",
      values_to = "r_date"
    ) %>%
    pivot_longer(
      cols = matches(value_pattern),
      names_to = "value_index",
      values_to = "value",
      names_pattern = index_pattern
    ) %>%
    filter(r_index == paste0("r", value_index, "_date")) %>%
    filter(!is.na(r_date) & r_date < v1_date & !is.na(value))
  
  if (!is.null(group_vars)) {
    long_data <- long_data %>% group_by(across(all_of(group_vars)))
  } else {
    long_data <- long_data %>% group_by(across(-c(r_index, r_date, value_index, value)))
  }
  
  long_data %>%
    slice(which.min(r_date)) %>%
    rename(
      earliest_r_date = r_date,
      baseline_value = value,
      baseline_r = r_index
    ) %>%
    select(-value_index)
}
