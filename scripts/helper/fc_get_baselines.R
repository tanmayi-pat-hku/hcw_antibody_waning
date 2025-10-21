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


#data: A dataframe containing multiple columns, including date columns and value columns associated with different indices.
#value_type: A string that specifies the type of value to extract. This is used to match the corresponding columns in the dataframe.
#group_vars: An optional parameter that allows grouping of the data based on specified variables. If not provided, the function groups by all columns except for certain key columns.

#Condition: r_date < v1_date


#Define Patterns: Sets regex patterns to identify date columns, value columns based on the specified value_type, and to capture indices.

#Reshape Data: Converts the data frame into a long format twice: first for dates and then for values, creating new columns for each.

#Filter Data: Applies conditions to retain rows where the date corresponds to the value index, ensuring dates are valid, earlier than v1_date, and values are non-NA.

#Group Data: Groups the data by specified variables (if provided) or by all other relevant columns to prepare for extraction.

#Extract and Rename: Selects the earliest date, retrieves its corresponding value, renames the columns for clarity, and returns a clean data frame with baseline information.