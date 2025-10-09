library(dplyr)

get_measurements_in_interval <- function(data, first_dose_col, second_dose_col, weight_type = "elisa", permutation_col) {
  # Ensure the specified columns are present
  if (!all(c(first_dose_col, second_dose_col, permutation_col) %in% names(data))) {
    stop("One or more specified columns do not exist in the data.")
  }
  
  # Eliminate rows with missing values in specified columns
  data <- data %>%
    filter(!is.na(.data[[first_dose_col]]) & !is.na(.data[[second_dose_col]]) & !is.na(.data[[permutation_col]]))
  
  # Determine the weight column based on user input
  if (weight_type == "elisa") {
    weight_suffix = "_elisa_wt"
  } else if (weight_type == "svnt") {
    weight_suffix = "_svnt_wt"
  } else {
    stop("Invalid weight type specified. Use 'elisa' or 'svnt'.")
  }
  
  # Create a result data frame to hold the output
  result <- data.frame()
  
  # Loop through r1_date to r11_date
  for (i in 1:11) {
    r_date_col <- paste0("r", i, "_date")
    weight_col <- paste0("r", i, weight_suffix)
    
    # Check if the date column exists
    if (r_date_col %in% names(data) && weight_col %in% names(data)) {
      # Filter rows where the date is within the dose interval
      temp_result <- data %>%
        filter(!is.na(.data[[r_date_col]]) & 
                 .data[[r_date_col]] >= .data[[first_dose_col]] & 
                 .data[[r_date_col]] <= .data[[second_dose_col]]) %>%
        mutate(days_since_dose1 = as.numeric(difftime(.data[[r_date_col]], .data[[first_dose_col]], units = "days"))) %>%
        select(id, measure_date = .data[[r_date_col]], weight = .data[[weight_col]], days_since_dose1, permutation = .data[[permutation_col]])
      
      # Append the temporary result to the final result
      result <- rbind(result, temp_result)
    }
  }
  
  # Filter out rows with NA weights and exclude positive cases
  result <- result %>%
    filter(!is.na(weight) & 
             !id %in% data$id[!is.na(data$posdate_p1) & data$posdate_p1 >= data[[first_dose_col]] & data$posdate_p1 <= data[[second_dose_col]] |
                                !is.na(data$posdate_p2) & data$posdate_p2 >= data[[first_dose_col]] & data$posdate_p2 <= data[[second_dose_col]] |
                                !is.na(data$posdate_p3) & data$posdate_p3 >= data[[first_dose_col]] & data$posdate_p3 <= data[[second_dose_col]] |
                                !is.na(data$posdate_p4) & data$posdate_p4 >= data[[first_dose_col]] & data$posdate_p4 <= data[[second_dose_col]]])
  
  return(result)
}