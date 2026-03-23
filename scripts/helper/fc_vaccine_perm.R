fc_perm_summary <- function(df, perm_col, response_col, min_n = 5, dose_label = "X Doses", 
                            vaccine_date_col = NULL, infection_window_days = 90) {
  
  perm_colq     <- enquo(perm_col)
  response_colq <- enquo(response_col)
  perm_name     <- as_name(perm_colq)
  
  # Filtering: Remove rows where permutation or response is NA
  plot_data <- df %>%
    filter(!is.na(!!perm_colq) & !is.na(!!response_colq)) %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq)),
           dose_count = dose_label)
  
  # If vaccine_date_col is provided, filter out recent infections
  if (!is.null(vaccine_date_col)) {
    vaccine_date_sym <- sym(vaccine_date_col)
    
    plot_data <- plot_data %>%
      rowwise() %>%
      mutate(
        # Check if there's any positive infection within `infection_window_days` days before the vaccine date
        recent_infection = any(
          # Check posdate_p1
          (!is.na(posdate_p1) & !is.na(!!vaccine_date_sym) & 
             posdate_p1 >= (!!vaccine_date_sym - infection_window_days) & 
             posdate_p1 <= !!vaccine_date_sym),
          # Check posdate_p2
          (!is.na(posdate_p2) & !is.na(!!vaccine_date_sym) & 
             posdate_p2 >= (!!vaccine_date_sym - infection_window_days) & 
             posdate_p2 <= !!vaccine_date_sym),
          # Check posdate_p3
          (!is.na(posdate_p3) & !is.na(!!vaccine_date_sym) & 
             posdate_p3 >= (!!vaccine_date_sym - infection_window_days) & 
             posdate_p3 <= !!vaccine_date_sym),
          # Check posdate_p4
          (!is.na(posdate_p4) & !is.na(!!vaccine_date_sym) & 
             posdate_p4 >= (!!vaccine_date_sym - infection_window_days) & 
             posdate_p4 <= !!vaccine_date_sym)
        )
      ) %>%
      ungroup() %>%
      # Filter out rows with recent infections
      filter(!recent_infection) %>%
      select(-recent_infection)
  }
  
  # Grouping and filtering by minimum sample size
  valid_perms <- plot_data %>%
    group_by(!!perm_colq) %>%
    filter(n() >= min_n) %>%
    ungroup() %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq),
                                 levels = unique(as.character(!!perm_colq))))
  
  # Count valid entries per permutation
  valid_perm_count <- valid_perms %>%
    count(!!perm_colq, name = "count")
  
  # Calculate medians
  medians <- valid_perms %>%
    group_by(!!perm_colq) %>%
    summarise(median = median(!!response_colq, na.rm = TRUE), .groups = "drop") %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq),
                                 levels = levels(valid_perms[[perm_name]])),
           dose_count = dose_label)
  
  perms    <- levels(valid_perms[[perm_name]])
  n_levels <- length(perms)
  mid_x    <- n_levels / 2 + 0.5
  
  list(
    plot_data = plot_data,
    valid_perms = valid_perms,
    valid_perm_count = valid_perm_count,
    medians = medians,
    perms = perms,
    n_levels = n_levels,
    mid_x = mid_x
  )
}

#Parameters:
#df: The input dataframe containing the data to summarize.
#perm_col: The column representing permutations of vaccine brands (e.g., one_dose_permutation, two_dose_permutation).
#response_col: The column representing the response variable 
#min_n: Minimum number of entries required for a permutation to be considered valid (default is 5).
#dose_label: A label for the dose count (default is "4 Doses").

#Output
#plot_data: The filtered and structured data frame ready for analysis.
#valid_perms: The data frame with only valid permutations.
#valid_perm_count: A count of valid entries for each permutation.
#medians: The calculated median values for the response variable by permutation.
#perms: The levels of valid permutations.
#n_levels: The total number of unique permutation levels.
#mid_x: A calculated midpoint for plotting purposes.

