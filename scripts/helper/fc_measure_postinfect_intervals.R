#' Extract antibody measurements after a positive PCR test (post‑infection waning)
#'
#' This function extracts all antibody measurements (r1_date … r11_date) that
#' occurred after a specified infection date (e.g., the first positive PCR)
#' and before any subsequent vaccine dose (if given). It mirrors the logic
#' of `get_measurements_in_interval` but for a post‑infection window.
#'
#' @param data               Data frame with wide‑format measurement columns.
#' @param infection_date_col Name of the column containing the infection date
#'                           (default = "posdate_p1").
#' @param weight_type        "elisa" or "svnt": which antibody readout to extract.
#' @param min_days           Minimum number of days after the infection date
#'                           required for a measurement to be included
#'                           (default = 0). Increase to exclude the immediate
#'                           post‑infection period (analogous to excluding the
#'                           vaccination plateau).
#'
#' @return A data.frame with one row per measurement, containing subject id,
#'         measurement date, antibody weight, days since infection,
#'         the infection date, the next vaccine dose date (if any),
#'         and demographic variables.
get_infection_waning <- function(data,
                                 infection_date_col = "posdate_p1",
                                 weight_type = "svnt",
                                 min_days = 0) {
  
  # Check that the infection date column exists
  if (!(infection_date_col %in% names(data))) {
    stop("The specified infection date column does not exist in the data.")
  }
  
  # Determine the suffix for antibody weight columns 
  # For ELISA, columns are named r1_elisa_wt, r2_elisa_wt, …
  # For sVNT, columns are named r1_svnt_wt,  r2_svnt_wt, …
  if (weight_type == "elisa") {
    weight_suffix <- "_elisa_wt"
  } else if (weight_type == "svnt") {
    weight_suffix <- "_svnt_wt"
  } else {
    stop("Invalid weight type specified. Use 'elisa' or 'svnt'.")
  }
  
  # Keep only individuals with a recorded infection date 
  data <- data %>%
    filter(!is.na(.data[[infection_date_col]]))
  
  # For each person, find the earliest vaccine dose given after the infection 
  # This ensures we only use antibody measurements from the pure post‑infection,
  # pre‑booster period. If a person received a vaccine dose after infection,
  # measurements after that dose are excluded (they would reflect vaccine‑boosted
  # antibodies, not solely infection‑induced waning).
  data <- data %>%
    mutate(
      next_vaccine_date = pmin(
        ifelse(!is.na(dose1_date) & dose1_date >= .data[[infection_date_col]], dose1_date, Inf),
        ifelse(!is.na(dose2_date) & dose2_date >= .data[[infection_date_col]], dose2_date, Inf),
        ifelse(!is.na(dose3_date) & dose3_date >= .data[[infection_date_col]], dose3_date, Inf),
        ifelse(!is.na(dose4_date) & dose4_date >= .data[[infection_date_col]], dose4_date, Inf),
        ifelse(!is.na(dose5_date) & dose5_date >= .data[[infection_date_col]], dose5_date, Inf),
        ifelse(!is.na(dose6_date) & dose6_date >= .data[[infection_date_col]], dose6_date, Inf),
        ifelse(!is.na(dose7_date) & dose7_date >= .data[[infection_date_col]], dose7_date, Inf),
        ifelse(!is.na(dose8_date) & dose8_date >= .data[[infection_date_col]], dose8_date, Inf),
        na.rm = TRUE
      ),
      # Replace Inf with NA (no subsequent vaccine dose)
      next_vaccine_date = ifelse(is.infinite(next_vaccine_date), NA, next_vaccine_date)
    )
  
  # Initialise an empty data frame to collect results
  result <- data.frame()
  
  # Loop through all possible measurement rounds (r1 … r11)
  for (i in 1:11) {
    r_date_col  <- paste0("r", i, "_date")
    weight_col  <- paste0("r", i, weight_suffix)
    
    if (r_date_col %in% names(data) && weight_col %in% names(data)) {
      
      # Keep rows where:
      #   - the measurement date is not missing,
      #   - the measurement date is >= the infection date,
      #   - AND either there is no next vaccine dose or the measurement
      #     is before (or on) that next vaccine date.
      temp_result <- data %>%
        filter(!is.na(.data[[r_date_col]])) %>%
        filter(.data[[r_date_col]] >= .data[[infection_date_col]]) %>%
        filter(is.na(next_vaccine_date) | .data[[r_date_col]] <= next_vaccine_date) %>%
        mutate(
          days_since_infection = as.numeric(difftime(.data[[r_date_col]],
                                                     .data[[infection_date_col]],
                                                     units = "days"))
        ) %>%
        select(
          id,
          measure_date        = .data[[r_date_col]],
          weight              = .data[[weight_col]],
          days_since_infection,
          infection_date      = .data[[infection_date_col]],
          next_vaccine_date,
          dose1_date, dose2_date, dose3_date, dose4_date, dose5_date,
          dose6_date, dose7_date, dose8_date,
          age_enrol, age_bin, male
        )
      
      result <- rbind(result, temp_result)
    }
  }
  
  # Apply filters to the extracted data 
  # Keep only measurements with:
  #   - non‑missing weight
  #   - weight >= 1 (remove invalid / negative values)
  #   - days since infection >= min_days (to exclude the immediate post‑infection period)
  result <- result %>%
    filter(!is.na(weight)) %>%
    filter(weight >= 1) %>%
    filter(days_since_infection >= min_days)
  
  # Return the cleaned dataset
  return(result)
}