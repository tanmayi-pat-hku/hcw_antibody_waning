library(dplyr)
library(tidyr)

snapshot_vaccine_year <- function(data, cutoff_date) {
  # Convert cutoff date to Date type
  cutoff_date <- as.Date(cutoff_date)
  
  # Create a snapshot of the data
  data <- data %>%
    mutate(across(contains("date"), \(x) as.Date(x, format = "%Y-%m-%d"))) %>%
    mutate(
      # Count the number of doses received before the cutoff date
      num_vaccines = rowSums(
        sapply(1:5, function(i) {
          as.Date(data[[paste0("dose", i, "_date")]]) < cutoff_date
        }),
        na.rm = TRUE
      ),
      
      # Generate permutations of vaccine brands
      vaccines_permutation = case_when(
        num_vaccines == 0 ~ "No Vaccines",
        num_vaccines == 1 ~ as.character(dose1_brand),
        num_vaccines == 2 ~ paste(dose1_brand, dose2_brand, sep = "-"),
        num_vaccines == 3 ~ paste(dose1_brand, dose2_brand, dose3_brand, sep = "-"),
        num_vaccines == 4 ~ paste(dose1_brand, dose2_brand, dose3_brand, dose4_brand, sep = "-"),
        num_vaccines == 5 ~ paste(dose1_brand, dose2_brand, dose3_brand, dose4_brand, dose5_brand, sep = "-"),
        TRUE ~ "More than Five"
      )
    )
  
  return(data)
}

calculate_titer <- function(data, suffix) {
  data <- data %>%
    filter(num_vaccines > 0 & num_vaccines < 8)
  baseline_data <- data %>%
    mutate(
      baseline_titer = case_when(
        num_vaccines == 1 ~ get(paste0("v1", suffix)),
        num_vaccines == 2 ~ get(paste0("v2", suffix)),
        num_vaccines == 3 ~ get(paste0("v3", suffix)),
        num_vaccines == 4 ~ get(paste0("v4", suffix)),
        num_vaccines == 5 ~ get(paste0("v5", suffix)),
        TRUE ~ NA_real_
      ),
      titer_step_1 = case_when(
        num_vaccines == 1 ~ get(paste0("v2", suffix)),
        num_vaccines == 2 ~ get(paste0("v3", suffix)),
        num_vaccines == 3 ~ get(paste0("v4", suffix)),
        num_vaccines == 4 ~ get(paste0("v5", suffix)),
        num_vaccines == 5 ~ get(paste0("v6", suffix)),
        TRUE ~ NA_real_
      ),
      titer_step_2 = case_when(
        num_vaccines == 1 ~ get(paste0("v3", suffix)),
        num_vaccines == 2 ~ get(paste0("v4", suffix)),
        num_vaccines == 3 ~ get(paste0("v5", suffix)),
        num_vaccines == 4 ~ get(paste0("v6", suffix)),
        num_vaccines == 5 ~ get(paste0("v7", suffix)),
        TRUE ~ NA_real_
      )
    ) %>%
    return(baseline_data)
}

calculate_titer_3 <- function(data, suffix) {
  data <- data %>%
    filter(num_vaccines > 0 & num_vaccines < 8)
  baseline_data <- data %>%
    mutate(
      baseline_titer = case_when(
        num_vaccines == 1 ~ get(paste0("v1", suffix)),
        num_vaccines == 2 ~ get(paste0("v2", suffix)),
        num_vaccines == 3 ~ get(paste0("v3", suffix)),
        num_vaccines == 4 ~ get(paste0("v4", suffix)),
        num_vaccines == 5 ~ get(paste0("v5", suffix)),
        TRUE ~ NA_real_
      ),
      titer_step_1 = case_when(
        num_vaccines == 1 ~ get(paste0("v2", suffix)),
        num_vaccines == 2 ~ get(paste0("v3", suffix)),
        num_vaccines == 3 ~ get(paste0("v4", suffix)),
        num_vaccines == 4 ~ get(paste0("v5", suffix)),
        num_vaccines == 5 ~ get(paste0("v6", suffix)),
        TRUE ~ NA_real_
      ),
      titer_step_2 = case_when(
        num_vaccines == 1 ~ get(paste0("v3", suffix)),
        num_vaccines == 2 ~ get(paste0("v4", suffix)),
        num_vaccines == 3 ~ get(paste0("v5", suffix)),
        num_vaccines == 4 ~ get(paste0("v6", suffix)),
        num_vaccines == 5 ~ get(paste0("v7", suffix)),
        TRUE ~ NA_real_
      ),
      titer_step_3 = case_when(
        num_vaccines == 1 ~ get(paste0("v4", suffix)),
        num_vaccines == 2 ~ get(paste0("v5", suffix)),
        num_vaccines == 3 ~ get(paste0("v6", suffix)),
        num_vaccines == 4 ~ get(paste0("v7", suffix)),
        num_vaccines == 5 ~ get(paste0("v8", suffix)),
        TRUE ~ NA_real_
      )
    ) %>%
    return(baseline_data)
}


create_long_titer_data <- function(data, 
                                   baseline_col = "baseline_titer", 
                                   step1_col = "titer_step_1", 
                                   step2_col = "titer_step_2",
                                   baseline_label = "Baseline",
                                   step1_label = "Step 1", 
                                   step2_label = "Step 2") {
  
  data %>%
    pivot_longer(
      cols = c(all_of(baseline_col), all_of(step1_col), all_of(step2_col)),
      names_to = "time_point",
      values_to = "titer_value"
    ) %>%
    mutate(time_point = case_when(
      time_point == baseline_col ~ baseline_label,
      time_point == step1_col ~ step1_label,
      time_point == step2_col ~ step2_label,
      TRUE ~ time_point
    ))
}

create_long_titer_data_3 <- function(data, 
                                   baseline_col = "baseline_titer", 
                                   step1_col = "titer_step_1", 
                                   step2_col = "titer_step_2",
                                   step3_col = "titer_step_3",
                                   baseline_label = "Baseline",
                                   step1_label = "Step 1", 
                                   step2_label = "Step 2",
                                   step3_label = "Step 3") {
  
  data %>%
    pivot_longer(
      cols = c(all_of(baseline_col), all_of(step1_col), all_of(step2_col), all_of(step3_col)),
      names_to = "time_point",
      values_to = "titer_value"
    ) %>%
    mutate(time_point = case_when(
      time_point == baseline_col ~ baseline_label,
      time_point == step1_col ~ step1_label,
      time_point == step2_col ~ step2_label,
      time_point == step3_col ~ step3_label,
      TRUE ~ time_point
    ))
}
