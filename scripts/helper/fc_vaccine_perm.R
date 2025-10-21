fc_perm_summary <- function(df, perm_col, response_col, min_n = 5, dose_label = "4 Doses") {
  perm_colq     <- enquo(perm_col)
  response_colq <- enquo(response_col)
  perm_name     <- as_name(perm_colq)
  #Filtering: Removes rows where either the permutation or the response variable is NA
  plot_data <- df %>%
    filter(!is.na(!!perm_colq) & !is.na(!!response_colq)) %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq)),
           dose_count = dose_label)
  #Grouping: Groups the data by the permutation column.
  #Filtering: Keeps only those groups where the count of entries is greater than or equal to min_n.
  #Re-factoring: Ensures that the levels of the permutation factor are unique.
  
  valid_perms <- plot_data %>%
    group_by(!!perm_colq) %>%
    filter(n() >= min_n) %>%
    ungroup() %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq),
                                 levels = unique(as.character(!!perm_colq))))
  #This creates a new data frame that counts the number of valid entries for each permutation.
  valid_perm_count <- valid_perms %>%
    count(!!perm_colq, name = "count")
  
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


