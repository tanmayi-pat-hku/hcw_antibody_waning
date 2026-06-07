#' Extract baseline antibody measurements for each individual
#'
#' This function finds the earliest available antibody measurement before the
#' first vaccine dose (v1_date) for each individual (or group). It reshapes the
#' data from wide to long format, identifies the relevant date–value pairs,
#' and returns the baseline value and its date.
#'
#' @param data        A data.frame containing wide-format columns for each
#'                    measurement round
#' @param value_type  A string specifying the antibody type to extract
#'                    (e.g., "elisa_wt", "svnt_wt"). Used to find columns like
#'                    r1_elisa_wt, r2_elisa_wt, …
#' @param group_vars  Optional character vector of column names that define
#'                    groups (e.g., c("id", "dose1_brand")). If NULL, the
#'                    function groups by all columns except the measurement
#'                    columns (id, dates, values).
#'
#' @return A data.frame with columns: all grouping variables, earliest_r_date,
#'         baseline_value, baseline_r (the round index of that measurement).
get_baselines <- function(data, value_type, group_vars = NULL) {
  
  # Define regex patterns to identify columns
  date_pattern  <- "^r\\d+_date$"           # e.g., r1_date, r2_date, …
  value_pattern <- paste0("^r\\d+_", value_type, "$")  # e.g., r1_elisa_wt, …
  index_pattern <- paste0("r(\\d+)_", value_type)      # captures the number in r<num>_<type>
  
  # Reshape from wide to long 
  # We first pivot the date columns, then the value columns, and finally match them
  # by their round number (r1_date goes with r1_elisa_wt, etc.).
  long_data <- data %>%
    # Pivot date columns: creates columns r_index (e.g., "r1_date") and r_date.
    pivot_longer(
      cols      = matches(date_pattern),
      names_to  = "r_index",
      values_to = "r_date"
    ) %>%
    # Pivot value columns: captures the round number via names_pattern and
    #     creates value_index (just the number, e.g., "1", "2") and value.
    pivot_longer(
      cols         = matches(value_pattern),
      names_to     = "value_index",
      values_to    = "value",
      names_pattern = index_pattern
    ) %>%
    # 2c. Keep only rows where the round numbers match (r1_date with r1_elisa_wt, etc.).
    #     The condition compares "r1_date" with paste0("r", "1", "_date").
    filter(r_index == paste0("r", value_index, "_date"))
  
  # Apply inclusion criteria 
  # Only keep measurements that:
  #   - have a valid date,
  #   - occurred before the first vaccine dose (v1_date),
  #   - have a non‑missing antibody value.
  long_data <- long_data %>%
    filter(!is.na(r_date) & r_date < v1_date & !is.na(value))
  
  # Group data 
  # If explicit grouping variables are supplied, use them; otherwise group by
  # everything except the measurement‑specific columns. This ensures we later
  # pick the earliest measurement per individual (or per specified group).
  if (!is.null(group_vars)) {
    long_data <- long_data %>% group_by(across(all_of(group_vars)))
  } else {
    # "across(-c(r_index, r_date, value_index, value))" groups by all remaining columns.
    long_data <- long_data %>% group_by(across(-c(r_index, r_date, value_index, value)))
  }
  
  # Select the earliest measurement per group 
  # For each group, keep the row with the smallest r_date.
  long_data %>%
    slice(which.min(r_date)) %>%
    # Rename columns to clearer baseline names.
    rename(
      earliest_r_date = r_date,
      baseline_value  = value,
      baseline_r      = r_index
    ) %>%
    # Remove the now redundant value_index column.
    select(-value_index)
}