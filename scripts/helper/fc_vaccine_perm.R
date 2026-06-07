#' Summarise antibody response after a given dose (boost analysis)
#'
#' This function filters the dataset for a specific dose level (e.g., after
#' dose2), excludes participants with recent infections before that dose,
#' keeps only vaccine‑brand permutations that have a minimum number of
#' observations, and calculates median antibody levels for plotting.
#'
#' @param df                   Data frame containing all observations.
#' @param perm_col             Column (bare name) that defines the vaccine
#'                             brand permutation (e.g., two_dose_permutation).
#' @param response_col         Column (bare name) with the antibody measurement
#'                             (e.g., v2_elisa_wt).
#' @param min_n                Minimum number of observations per permutation
#'                             required for inclusion (default = 5).
#' @param dose_label           Character label describing the dose number
#'                             (e.g., "2 Dose").
#' @param vaccine_date_col     Column name (as string) of the date on which the
#'                             relevant vaccine dose was given (e.g., "v2_date").
#'                             If provided, participants with a PCR‑confirmed
#'                             infection in the `infection_window_days` before
#'                             that date are excluded.
#' @param infection_window_days Number of days before vaccination to check for
#'                              prior infection (default = 90).
#'
#' @return A list with elements:
#'   - plot_data         : the cleaned (but unfiltered) data
#'   - valid_perms       : data for permutations with ≥ min_n observations
#'   - valid_perm_count  : count of observations per valid permutation
#'   - medians           : median response value per permutation
#'   - perms             : vector of valid permutation levels
#'   - n_levels          : number of valid permutations
#'   - mid_x             : midpoint for plotting horizontal positions
fc_perm_summary <- function(df, perm_col, response_col, min_n = 5,
                            dose_label = "X Doses",
                            vaccine_date_col = NULL,
                            infection_window_days = 90) {
  
  # Capture the bare column names (tidy evaluation) 
  # `enquo()` captures the expression typed (e.g., two_dose_permutation)
  perm_colq     <- enquo(perm_col)
  response_colq <- enquo(response_col)
  perm_name     <- as_name(perm_colq)
  
  # Remove rows with missing permutation or response
  plot_data <- df %>%
    filter(!is.na(!!perm_colq) & !is.na(!!response_colq)) %>%
    mutate(
      # Convert permutation to factor (needed for plotting) and add dose label
      !!perm_colq := factor(as.character(!!perm_colq)),
      dose_count = dose_label
    )
  
  # Exclude participants with a recent infection before the dose
  # If a vaccine_date_col is specified, we remove any person who had a positive
  # PCR test (posdate_p1 .. posdate_p4) within `infection_window_days` days
  # before that dose. This ensures that the measured antibody response is
  # primarily vaccine‑induced, not boosted by a recent natural infection.
  if (!is.null(vaccine_date_col)) {
    vaccine_date_sym <- sym(vaccine_date_col)   # convert string to symbol
    
    plot_data <- plot_data %>%
      rowwise() %>%
      mutate(
        recent_infection = any(
          (!is.na(posdate_p1) & !is.na(!!vaccine_date_sym) &
             posdate_p1 >= (!!vaccine_date_sym - infection_window_days) &
             posdate_p1 <= !!vaccine_date_sym),
          (!is.na(posdate_p2) & !is.na(!!vaccine_date_sym) &
             posdate_p2 >= (!!vaccine_date_sym - infection_window_days) &
             posdate_p2 <= !!vaccine_date_sym),
          (!is.na(posdate_p3) & !is.na(!!vaccine_date_sym) &
             posdate_p3 >= (!!vaccine_date_sym - infection_window_days) &
             posdate_p3 <= !!vaccine_date_sym),
          (!is.na(posdate_p4) & !is.na(!!vaccine_date_sym) &
             posdate_p4 >= (!!vaccine_date_sym - infection_window_days) &
             posdate_p4 <= !!vaccine_date_sym)
        )
      ) %>%
      ungroup() %>%
      filter(!recent_infection) %>%
      select(-recent_infection)
  }
  
  # Keep only permutations with enough observations
  # `min_n` (default 5) ensures that a vaccine series has enough data to
  # produce a reliable median. Permutations with fewer than `min_n` rows
  # are dropped from subsequent summaries.
  valid_perms <- plot_data %>%
    group_by(!!perm_colq) %>%
    filter(n() >= min_n) %>%
    ungroup() %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq),
                                 levels = unique(as.character(!!perm_colq))))
  
  # Count observations per valid permutation
  valid_perm_count <- valid_perms %>%
    count(!!perm_colq, name = "count")
  
  # Calculate median response for each permutation 
  medians <- valid_perms %>%
    group_by(!!perm_colq) %>%
    summarise(median = median(!!response_colq, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(!!perm_colq := factor(as.character(!!perm_colq),
                                 levels = levels(valid_perms[[perm_name]])),
           dose_count = dose_label)
  
  # Store permutation levels
  perms    <- levels(valid_perms[[perm_name]])
  n_levels <- length(perms)
  mid_x    <- n_levels / 2 + 0.5   # midpoint for x‑axis positioning of labels
  
  # Return everything in a list
  list(
    plot_data        = plot_data,
    valid_perms      = valid_perms,
    valid_perm_count = valid_perm_count,
    medians          = medians,
    perms            = perms,
    n_levels         = n_levels,
    mid_x            = mid_x
  )
}