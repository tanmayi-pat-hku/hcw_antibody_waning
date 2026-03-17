
fit_powerlaw_with_boot <- function(data, n_boot = 500, seed = 123) {
  
  # Prepare data
  model_data <- data %>%
    mutate(
      log_days = log(days_since_dose1 + 0.01),
      log_weight = log(weight + 0.01),
      permutation = as.factor(permutation)
    )
  
  # Fit model
  model <- lmer(log_weight ~ log_days * permutation + (1 | id), 
                data = model_data,
                control = lmerControl(optimizer = "bobyqa"))
  
  # Create prediction grid
  pred_grid <- expand.grid(
    days_since_dose1 = seq(0, 420, length.out = 100),
    permutation = levels(model_data$permutation)  # Use levels() not unique()
  ) %>%
    mutate(log_days = log(days_since_dose1 + 0.01))
  
  # Function to predict on newdata for bootstrapping
  pred_fun <- function(.) {
    predict(., newdata = pred_grid, re.form = NA)
  }
  
  # Parametric bootstrap
  set.seed(seed)
  boot_obj <- bootMer(model, pred_fun, nsim = n_boot, parallel = "multicore", ncpus = 4)
  
  # Calculate CIs
  cis <- apply(boot_obj$t, 2, function(x) {
    quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  })
  
  # Original predictions
  original_pred <- predict(model, newdata = pred_grid, re.form = NA)
  
  # Add to prediction grid
  pred_grid$pred_weight <- exp(original_pred) - 0.01
  pred_grid$lower <- exp(cis[1, ]) - 0.01
  pred_grid$upper <- exp(cis[2, ]) - 0.01
  
  # Ensure within bounds
  pred_grid$pred_weight <- pmax(pmin(pred_grid$pred_weight, 100), 0)
  pred_grid$lower <- pmax(pmin(pred_grid$lower, 100), 0)
  pred_grid$upper <- pmax(pmin(pred_grid$upper, 100), 0)
  
  # FIXED: Extract waning rates properly
  fix <- fixef(model)
  perms <- levels(model_data$permutation)
  
  waning_rates <- data.frame(
    permutation = perms,
    waning_rate = NA,
    lower_ci = NA,
    upper_ci = NA
  )
  
  # Reference group (first permutation)
  waning_rates$waning_rate[1] <- fix["log_days"]
  
  # For other permutations, find the interaction term
  for(i in 2:length(perms)) {
    # Look for any coefficient that contains this permutation
    # The actual name might be "log_days:permutation4-4" or "log_days:permutation4-4-1"
    pattern <- paste0("log_days:permutation", perms[i])
    matching_term <- grep(pattern, names(fix), value = TRUE)
    
    if(length(matching_term) > 0) {
      waning_rates$waning_rate[i] <- fix["log_days"] + fix[matching_term[1]]
    } else {
      # If no interaction found, try without "permutation" in the name
      pattern2 <- paste0("log_days:", perms[i])
      matching_term2 <- grep(pattern2, names(fix), value = TRUE)
      
      if(length(matching_term2) > 0) {
        waning_rates$waning_rate[i] <- fix["log_days"] + fix[matching_term2[1]]
      } else {
        warning(paste("No interaction term found for", perms[i]))
      }
    }
  }
  
  # FIXED: Bootstrap waning rates with same logic
  rate_fun <- function(.) {
    f <- fixef(.)
    rates <- c(f["log_days"])
    
    for(i in 2:length(perms)) {
      # Try both possible patterns
      pattern1 <- paste0("log_days:permutation", perms[i])
      pattern2 <- paste0("log_days:", perms[i])
      
      if(pattern1 %in% names(f)) {
        rates <- c(rates, f["log_days"] + f[pattern1])
      } else if(pattern2 %in% names(f)) {
        rates <- c(rates, f["log_days"] + f[pattern2])
      } else {
        rates <- c(rates, NA)
      }
    }
    return(rates)
  }
  
  rate_boot <- bootMer(model, rate_fun, nsim = n_boot, parallel = "multicore", ncpus = 4)
  
  for(i in 1:length(perms)) {
    waning_rates$lower_ci[i] <- quantile(rate_boot$t[,i], 0.025, na.rm = TRUE)
    waning_rates$upper_ci[i] <- quantile(rate_boot$t[,i], 0.975, na.rm = TRUE)
  }
  
  # Return everything
  list(
    model = model,
    predictions = pred_grid,
    waning_rates = waning_rates,
    n_participants = length(unique(model_data$id)),
    n_obs = nrow(model_data)
  )
}