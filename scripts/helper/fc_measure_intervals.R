#' Extract antibody measurements between two consecutive doses
#'
#' This function extracts all antibody measurements (r1_date … r11_date) that
#' fall within a specified inter‑dose interval (e.g., between dose2 and dose3).
#' It excludes individuals who had a positive PCR test during that interval,
#' applies a minimum number of days since the earlier dose, and removes
#' invalid (negative or zero) weights.
#'
#' @param data            Data frame with wide‑format measurement columns.
#' @param first_dose_col  Name of the column containing the date of the earlier
#'                        vaccine dose (e.g., "dose2_date").
#' @param second_dose_col Name of the column containing the date of the later
#'                        vaccine dose (e.g., "dose3_date").
#' @param weight_type     "elisa" or "svnt": which antibody readout to extract.
#' @param permutation_col Name of the column containing the vaccine permutation
#'                        string (e.g., "two_dose_permutation").
#' @param min_days        Minimum number of days after the first dose required
#'                        for a measurement to be included (default = 30).
#'                        Increase to exclude the early plateau phase.
#'
#' @return A data.frame with one row per measurement, containing subject id,
#'         measurement date, antibody weight, days since first dose,
#'         permutation string, demographic variables, and dose brands.
get_measurements_in_interval <- function(data, first_dose_col, second_dose_col,
                                         weight_type = "elisa", permutation_col,
                                         min_days = 30) {
  
  # Check that required columns exist
  if (!all(c(first_dose_col, second_dose_col, permutation_col) %in% names(data))) {
    stop("One or more specified columns do not exist in the data.")
  }
  
  # Remove rows with missing values in key columns
  # Need complete data for the interval start, end, and the vaccine series.
  data <- data %>%
    filter(!is.na(.data[[first_dose_col]]) &
             !is.na(.data[[second_dose_col]]) &
             !is.na(.data[[permutation_col]]))
  
  # Determine the suffix for weight columns
  # For ELISA, columns are named r1_elisa_wt, r2_elisa_wt, …
  # For sVNT, columns are named r1_svnt_wt,  r2_svnt_wt, …
  if (weight_type == "elisa") {
    weight_suffix <- "_elisa_wt"
  } else if (weight_type == "svnt") {
    weight_suffix <- "_svnt_wt"
  } else {
    stop("Invalid weight type specified. Use 'elisa' or 'svnt'.")
  }
  
  # Initialise an empty data frame
  result <- data.frame()
  
  # Loop through all possible measurement rounds (r1 … r11)
  for (i in 1:11) {
    # Construct column names for the current round
    r_date_col  <- paste0("r", i, "_date")
    weight_col  <- paste0("r", i, weight_suffix)
    
    # Process only if both columns exist in the data
    if (r_date_col %in% names(data) && weight_col %in% names(data)) {
      # Keep only rows where the measurement date falls within the interval
      #     [first_dose_date, second_dose_date] and the measurement is not missing.
      temp_result <- data %>%
        filter(!is.na(.data[[r_date_col]]) &
                 .data[[r_date_col]] >= .data[[first_dose_col]] &
                 .data[[r_date_col]] <= .data[[second_dose_col]]) %>%
        mutate(
          # Calculate days since the earlier dose
          days_since_dose1 = as.numeric(difftime(.data[[r_date_col]],
                                                 .data[[first_dose_col]],
                                                 units = "days"))
        ) %>%
        # Select and rename columns for the output
        select(id,
               measure_date    = .data[[r_date_col]],
               weight          = .data[[weight_col]],
               days_since_dose1,
               permutation     = .data[[permutation_col]],
               dose1_brand, dose2_brand, dose3_brand,
               dose4_brand, dose5_brand, dose6_brand,
               type, age_enrol, age_bin, male)
      
      # Append the current round’s data to the overall result
      result <- rbind(result, temp_result)
    }
  }
  
  # Exclude individuals who had a positive PCR test during the interval 
  # Any person with a posdate_p1 … posdate_p4 that falls between the two dose
  # dates is removed, because a natural infection during that window would
  # confound the vaccine‑induced antibody trajectory.
  result <- result %>%
    filter(
      !is.na(weight),
      !id %in% data$id[
        # posdate_p1
        (!is.na(data$posdate_p1) & data$posdate_p1 >= data[[first_dose_col]] &
           data$posdate_p1 <= data[[second_dose_col]]) |
          # posdate_p2
          (!is.na(data$posdate_p2) & data$posdate_p2 >= data[[first_dose_col]] &
             data$posdate_p2 <= data[[second_dose_col]]) |
          # posdate_p3
          (!is.na(data$posdate_p3) & data$posdate_p3 >= data[[first_dose_col]] &
             data$posdate_p3 <= data[[second_dose_col]]) |
          # posdate_p4
          (!is.na(data$posdate_p4) & data$posdate_p4 >= data[[first_dose_col]] &
             data$posdate_p4 <= data[[second_dose_col]])
      ]
    )
  
  # Apply the minimum‑days filter 
  # Keep only measurements taken at least `min_days` after the earlier dose.
  # This can be used to exclude the early plateau / peak phase.
  result <- result %>% filter(days_since_dose1 >= min_days)
  
  # Remove invalid (non‑positive) weights
  result <- result %>% filter(weight >= 1)
  
  # Return the cleaned dataset
  return(result)
}