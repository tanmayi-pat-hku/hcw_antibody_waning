extract_valid_dates_with_weights <- function(data) {
  if (!"earliest_posdate" %in% colnames(data)) {
    data <- calculate_earliest_date(data)
  }
  
  date_cols <- paste0("r", 1:11, "_date")
  data[date_cols] <- lapply(data[date_cols], as.Date)
  
  elisa_weight_cols <- paste0("r", 1:11, "_elisa_wt")
  svnt_weight_cols <- paste0("r", 1:11, "_svnt_wt")
  
  long_data <- data %>%
    pivot_longer(
      cols = all_of(date_cols),
      names_to = "r_index",
      values_to = "valid_date"
    ) %>%
    rowwise() %>%
    mutate(
      rX_elisa_wt = case_when(
        r_index == "r1_date" ~ r1_elisa_wt,
        r_index == "r2_date" ~ r2_elisa_wt,
        r_index == "r3_date" ~ r3_elisa_wt,
        r_index == "r4_date" ~ r4_elisa_wt,
        r_index == "r5_date" ~ r5_elisa_wt,
        r_index == "r6_date" ~ r6_elisa_wt,
        r_index == "r7_date" ~ r7_elisa_wt,
        r_index == "r8_date" ~ r8_elisa_wt,
        r_index == "r9_date" ~ r9_elisa_wt,
        r_index == "r10_date" ~ r10_elisa_wt,
        r_index == "r11_date" ~ r11_elisa_wt,
        TRUE ~ NA_real_
      ),
      rX_svnt_wt = case_when(
        r_index == "r1_date" ~ r1_svnt_wt,
        r_index == "r2_date" ~ r2_svnt_wt,
        r_index == "r3_date" ~ r3_svnt_wt,
        r_index == "r4_date" ~ r4_svnt_wt,
        r_index == "r5_date" ~ r5_svnt_wt,
        r_index == "r6_date" ~ r6_svnt_wt,
        r_index == "r7_date" ~ r7_svnt_wt,
        r_index == "r8_date" ~ r8_svnt_wt,
        r_index == "r9_date" ~ r9_svnt_wt,
        r_index == "r10_date" ~ r10_svnt_wt,
        r_index == "r11_date" ~ r11_svnt_wt,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(
      !is.na(valid_date) & valid_date > (earliest_posdate + 30) & valid_date <= (earliest_posdate + 60)
    )
  
  return(long_data %>% select(valid_date, rX_elisa_wt, rX_svnt_wt))
}



#Converts specified date columns to Date format.
#Reshapes the data to a long format to facilitate analysis.
#Adds corresponding weight columns based on the reshaped data.
#Filters for valid dates that fall within a specified range relative to earliest_posdate.
#Returns a clean data frame with relevant information for further analysis.

#it goes through all the rX_date columns and identifies dates that are between 30 and 60 days after the earliest_posdate.
#For each valid date found within that range, it retrieves the corresponding rX_elisa_wt and rX_svnt_wt values.
