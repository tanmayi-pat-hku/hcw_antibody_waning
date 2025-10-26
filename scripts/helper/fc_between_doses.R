get_days_between_doses <- function(data, first_dose_col, second_dose_col, permutation_col) {
  # Ensure the specified columns are present
  if (!all(c(first_dose_col, second_dose_col, permutation_col) %in% names(data))) {
    stop("One or more specified columns do not exist in the data.")
  }
  
  # Create a result data frame to hold the output
  result <- data.frame()
  
  # Check if the date columns exist
  if (first_dose_col %in% names(data) && second_dose_col %in% names(data)) {
    # Filter rows where both doses exist
    temp_result <- data %>%
      filter(!is.na(.data[[first_dose_col]]) & !is.na(.data[[second_dose_col]])) %>%
      mutate(days_between_doses = as.numeric(difftime(.data[[second_dose_col]], .data[[first_dose_col]], units = "days"))) %>%
      mutate(permutation = .data[[permutation_col]])  # Add permutation column directly
    
    # Append the temporary result to the final result
    result <- rbind(result, temp_result)
  }
  
  return(result)
}