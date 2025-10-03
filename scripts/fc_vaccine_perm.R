fc_perm_summary <- function(df, perm_col, response_col, min_n = 5, dose_label = "4 Doses") {
  perm_colq     <- enquo(perm_col)
  response_colq <- enquo(response_col)
  perm_name     <- as_name(perm_colq)
  
  plot_data <- df %>%
    filter(!is.na(!!perm_colq) & !is.na(!!response_colq)) %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq)),
           dose_count = dose_label)
  
  valid_perms <- plot_data %>%
    group_by(!!perm_colq) %>%
    filter(n() >= min_n) %>%
    ungroup() %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq),
                                 levels = unique(as.character(!!perm_colq))))
  
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