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


#Define Column Names: Constructs column names for baseline and value weights based on the specified value_type, including base_col, v1_col, and v2_col, and optionally v3_col if dose_count is 3.

#Add Baseline Values: Merges baseline values from baseline_data into data using the individual IDs to match corresponding baseline values.

#Filter Data: Creates a homologous_data data frame by filtering out rows with NA values in essential columns and applying conditions based on the dose_count and permutation_col.

#Reshape to Long Format: Converts the filtered data into a long format where each measurement corresponds to its dose type, while conditionally including the third dose if applicable.

#Calculate Median Values: Computes the median values for each dose and permutation category, returning both the reshaped long data and the calculated median values in a list.


#A data frame in long format that includes:
  #Individual IDs (id).
#Baseline values (base_col).
#Value weights for the first, second, and (if applicable) third doses.
#A new column indicating the dose type (dose).
#A color coding based on the permutation_col values.

