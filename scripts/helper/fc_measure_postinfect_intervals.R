#Post-Infection Waning
get_infection_waning <- function(data, infection_date_col = "posdate_p1", 
                                 weight_type = "svnt") {
  
  # Determine weight suffix
  if (weight_type == "elisa") {
    weight_suffix = "_elisa_wt"
  } else if (weight_type == "svnt") {
    weight_suffix = "_svnt_wt"
  } else {
    stop("Invalid weight type specified. Use 'elisa' or 'svnt'.")
  }
  
  # Filter to individuals with infection date
  data <- data %>%
    filter(!is.na(.data[[infection_date_col]]))
  
  # Create a column with the next vaccine dose date after infection
  data <- data %>%
    mutate(
      # Find the earliest vaccine dose date that is >= infection date
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
      # If no vaccine after infection, set to Inf (keep all measurements)
      next_vaccine_date = ifelse(is.infinite(next_vaccine_date), NA, next_vaccine_date)
    )
  
  # Create result data frame
  result <- data.frame()
  
  # Loop through r1_date to r11_date
  for (i in 1:11) {
    r_date_col <- paste0("r", i, "_date")
    weight_col <- paste0("r", i, weight_suffix)
    
    if (r_date_col %in% names(data) && weight_col %in% names(data)) {
      
      temp_result <- data %>%
        filter(!is.na(.data[[r_date_col]]) & 
                 .data[[r_date_col]] >= .data[[infection_date_col]]) %>%
        # Only keep measurements before the next vaccine dose (if exists)
        filter(is.na(next_vaccine_date) | .data[[r_date_col]] <= next_vaccine_date) %>%
        mutate(
          days_since_infection = as.numeric(difftime(.data[[r_date_col]], .data[[infection_date_col]], units = "days"))
        ) %>%
        select(id, measure_date = .data[[r_date_col]], 
               weight = .data[[weight_col]], 
               days_since_infection,
               infection_date = .data[[infection_date_col]],
               next_vaccine_date,
               dose1_date, dose2_date, dose3_date, dose4_date, dose5_date,
               dose6_date, dose7_date, dose8_date,
               age_enrol, age_bin, male)
      
      result <- rbind(result, temp_result)
    }
  }
  
  # Filter out rows with NA weights
  result <- result %>%
    filter(!is.na(weight))
  
  return(result)
}