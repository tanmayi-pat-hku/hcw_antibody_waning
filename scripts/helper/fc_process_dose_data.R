library(dplyr)
library(tidyr)

process_dose_data <- function(data, baseline_data, value_type, dose_count, permutation_col) {
  # Define column names based on the value type
  base_col <- paste0("base_", value_type)
  v1_col <- paste0("v1_", value_type, "_wt")
  v2_col <- paste0("v2_", value_type, "_wt")
  
  # Add baseline values to the data
  data[[base_col]] <- baseline_data$baseline_value[match(data$id, baseline_data$id)]
  
  # Create the homologous dataset
  homologous_data <- data %>%
    filter(!is.na(!!sym(base_col)) & 
             !is.na(!!sym(v1_col)) & 
             !is.na(!!sym(v2_col)) & 
             (if (dose_count == 3) !is.na(!!sym(paste0("v3_", value_type, "_wt"))) else TRUE)) %>%
    filter(
      if (dose_count == 2) {
        !!sym(permutation_col) %in% c("1-1", "4-4")
      } else {
        !!sym(permutation_col) %in% c("1-1-1", "4-4-4")
      }
    )
  
  # Reshape to long format, conditionally including v3_col
  if (dose_count == 3) {
    v3_col <- paste0("v3_", value_type, "_wt")
    long_data <- homologous_data %>%
      select(id, !!sym(base_col), !!sym(v1_col), !!sym(v2_col), !!sym(v3_col), !!sym(permutation_col)) %>%
      pivot_longer(cols = c(!!sym(base_col), !!sym(v1_col), !!sym(v2_col), !!sym(v3_col)),
                   names_to = "dose", values_to = "value")
  } else {
    long_data <- homologous_data %>%
      select(id, !!sym(base_col), !!sym(v1_col), !!sym(v2_col), !!sym(permutation_col)) %>%
      pivot_longer(cols = c(!!sym(base_col), !!sym(v1_col), !!sym(v2_col)),
                   names_to = "dose", values_to = "value")
  }
  
  long_data <- long_data %>%
    mutate(color = ifelse(!!sym(permutation_col) == paste0("1-1", if (dose_count == 2) "" else "-1"), "#1F77B4", "#FF7F0E")) %>%
    mutate(dose = factor(dose, levels = c(base_col, v1_col, v2_col, if (dose_count == 3) v3_col)))
  
  # Calculate median values
  median_values <- long_data %>%
    group_by(dose, !!sym(permutation_col)) %>%
    summarize(median_value = median(value, na.rm = TRUE), .groups = "drop")
  
  return(list(long_data = long_data, median_values = median_values))
}