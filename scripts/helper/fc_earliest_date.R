calculate_earliest_date <- function(data) {
  data <- data %>%
    mutate(earliest_posdate = pmin(posdate_p1, posdate_p2, posdate_p3, posdate_p4, na.rm = TRUE)) %>%
    filter(!is.na(earliest_posdate)) 
  return(data)
}