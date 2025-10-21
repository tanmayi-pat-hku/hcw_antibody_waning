calculate_earliest_date <- function(data) {
  data <- data %>%
    mutate(earliest_posdate = pmin(posdate_p1, posdate_p2, posdate_p3, posdate_p4, na.rm = TRUE)) %>%
    filter(!is.na(earliest_posdate)) 
  return(data)
}

#mutate(): This function is used to create or modify columns in a data frame.
#earliest_posdate: A new column is created named earliest_posdate.
#pmin(): This function calculates the parallel minimum of the specified columns (posdate_p1, posdate_p2, posdate_p3, posdate_p4). It checks each row across these columns and returns the earliest (minimum) date.
#The argument na.rm = TRUE ensures that if any of the date columns contain NA (missing values), they are ignored in the comparison. If all values in a row are NA, the result for that row will also be NA.
#filters out NA values 

#creates a new column with this earliest date and filters out any rows where this earliest date could not be determined due to missing values