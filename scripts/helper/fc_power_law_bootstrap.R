#Post-vaccine waning
fit_powerlaw_with_boot_adj <- function(data, n_boot = 500, seed = 123) {
  
  # Prepare data
  model_data <- data %>%
    mutate(
      log_days = log(days_since_dose1 + 0.01),
      log_weight = log(weight + 0.01),
      permutation = as.factor(permutation),
      male = as.factor(male),
      age_bin = as.factor(age_bin)
    )
  
  # Fit model with interactions to test if waning differs by age/sex
  model <- lmer(log_weight ~ log_days * permutation + male + age_bin + 
                  log_days:male + log_days:age_bin + (1 | id), 
                data = model_data,
                control = lmerControl(optimizer = "bobyqa"))
  
  # Create prediction grid (all combinations)
  pred_grid <- expand.grid(
    days_since_dose1 = seq(0, 420, length.out = 100),
    permutation = levels(model_data$permutation),
    male = levels(model_data$male),
    age_bin = levels(model_data$age_bin)
  ) %>%
    mutate(log_days = log(days_since_dose1 + 0.01))
  
  # Bootstrap predictions
  set.seed(seed)
  pred_fun <- function(.) predict(., newdata = pred_grid, re.form = NA)
  boot_obj <- bootMer(model, pred_fun, nsim = n_boot, parallel = "multicore", ncpus = 4)
  
  # Calculate prediction CIs and add to grid
  cis <- apply(boot_obj$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  original_pred <- predict(model, newdata = pred_grid, re.form = NA)
  
  pred_grid <- pred_grid %>%
    mutate(
      pred_weight = pmax(pmin(exp(original_pred) - 0.01, 100), 0),
      lower = pmax(pmin(exp(cis[1, ]) - 0.01, 100), 0),
      upper = pmax(pmin(exp(cis[2, ]) - 0.01, 100), 0)
    )
  
  # Extract fixed effects
  fix <- fixef(model)
  perms <- levels(model_data$permutation)
  male_lvls <- levels(model_data$male)
  age_lvls <- levels(model_data$age_bin)
  
  # Create waning rates for all demographic combinations
  waning_rates <- expand.grid(permutation = perms, male = male_lvls, age_bin = age_lvls,
                              stringsAsFactors = FALSE) %>%
    mutate(waning_rate = fix["log_days"])
  
  # Add permutation interactions
  for(perm in perms[-1]) {
    pattern <- paste0("log_days:permutation", perm)
    term <- grep(pattern, names(fix), value = TRUE)[1]
    if(!is.na(term)) waning_rates$waning_rate[waning_rates$permutation == perm] <- 
      waning_rates$waning_rate[waning_rates$permutation == perm] + fix[term]
  }
  
  # Add sex interaction
  if("log_days:male1" %in% names(fix)) {
    waning_rates$waning_rate[waning_rates$male == "1"] <- 
      waning_rates$waning_rate[waning_rates$male == "1"] + fix["log_days:male1"]
  }
  
  # Add age interactions
  for(age in age_lvls[-1]) {
    pattern <- paste0("log_days:age_bin", age)
    term <- grep(pattern, names(fix), value = TRUE)[1]
    if(!is.na(term)) {
      waning_rates$waning_rate[waning_rates$age_bin == age] <- 
        waning_rates$waning_rate[waning_rates$age_bin == age] + fix[term]
    }
  }
  
  # Bootstrap waning rates
  rate_fun <- function(.) {
    f <- fixef(.)
    rates <- numeric(nrow(waning_rates))
    for(i in 1:nrow(waning_rates)) {
      rate <- f["log_days"]
      perm <- waning_rates$permutation[i]
      sex <- waning_rates$male[i]
      age <- waning_rates$age_bin[i]
      
      if(perm != perms[1]) {
        pattern <- paste0("log_days:permutation", perm)
        if(pattern %in% names(f)) rate <- rate + f[pattern]
      }
      if(sex == "1" && "log_days:male1" %in% names(f)) rate <- rate + f["log_days:male1"]
      if(age != age_lvls[1]) {
        pattern <- paste0("log_days:age_bin", age)
        if(pattern %in% names(f)) rate <- rate + f[pattern]
      }
      rates[i] <- rate
    }
    return(rates)
  }
  
  rate_boot <- bootMer(model, rate_fun, nsim = n_boot, parallel = "multicore", ncpus = 4)
  
  waning_rates <- waning_rates %>%
    mutate(
      lower_ci = apply(rate_boot$t, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
      upper_ci = apply(rate_boot$t, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
    )
  
  # Bootstrap interaction effects
  interaction_terms <- names(fix)[grepl("log_days:male|log_days:age_bin", names(fix))]
  
  if(length(interaction_terms) > 0) {
    interaction_fun <- function(.) fixef(.)[interaction_terms]
    interaction_boot <- bootMer(model, interaction_fun, nsim = n_boot, parallel = "multicore", ncpus = 4)
    
    interaction_effects <- data.frame(
      term = interaction_terms,
      estimate = fix[interaction_terms],
      lower_ci = apply(interaction_boot$t, 2, function(x) quantile(x, 0.025, na.rm = TRUE)),
      upper_ci = apply(interaction_boot$t, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
    ) %>%
      mutate(significant = !(lower_ci < 0 & upper_ci > 0))
  } else {
    interaction_effects <- data.frame()
  }
  
  # Main effects (baseline differences)
  main_effects <- data.frame(
    term = names(fix)[grepl("^male|^age_bin", names(fix))],
    estimate = fix[grepl("^male|^age_bin", names(fix))],
    stringsAsFactors = FALSE
  )
  
  # Return results
  list(
    model = model,
    predictions = pred_grid,
    waning_rates = waning_rates,
    main_effects = main_effects,
    interaction_effects = interaction_effects,
    n_participants = length(unique(model_data$id)),
    n_obs = nrow(model_data)
  )
}

#Fit Powerlaw for Post-infection
fit_powerlaw_infection <- function(data, n_boot = 500, seed = 123) {
  
  # Prepare data
  model_data <- data %>%
    mutate(
      log_days = log(days_since_dose1 + 0.01),
      log_weight = log(weight + 0.01),
      male = as.factor(male),
      age_bin = as.factor(age_bin)
    ) %>%
    # Remove rows with infinite values
    filter(is.finite(log_days), is.finite(log_weight))
  
  # Fit model WITHOUT permutation (only one group)
  model <- lmer(log_weight ~ log_days + male + age_bin + 
                  log_days:male + log_days:age_bin + (1 | id), 
                data = model_data,
                control = lmerControl(optimizer = "bobyqa"))
  
  # Create prediction grid
  pred_grid <- expand.grid(
    days_since_dose1 = seq(0, 420, length.out = 100),
    male = levels(model_data$male),
    age_bin = levels(model_data$age_bin)
  ) %>%
    mutate(log_days = log(days_since_dose1 + 0.01))
  
  # Bootstrap predictions
  set.seed(seed)
  pred_fun <- function(.) predict(., newdata = pred_grid, re.form = NA)
  boot_obj <- bootMer(model, pred_fun, nsim = n_boot, parallel = "multicore", ncpus = 4)
  
  # Calculate CIs
  cis <- apply(boot_obj$t, 2, function(x) quantile(x, c(0.025, 0.975), na.rm = TRUE))
  original_pred <- predict(model, newdata = pred_grid, re.form = NA)
  
  pred_grid <- pred_grid %>%
    mutate(
      pred_weight = pmax(pmin(exp(original_pred) - 0.01, 100), 0),
      lower = pmax(pmin(exp(cis[1, ]) - 0.01, 100), 0),
      upper = pmax(pmin(exp(cis[2, ]) - 0.01, 100), 0)
    )
  
  # Extract waning rate
  fix <- fixef(model)
  
  waning_rate <- data.frame(
    waning_rate = fix["log_days"],
    lower_ci = NA,
    upper_ci = NA
  )
  
  # Bootstrap waning rate
  rate_fun <- function(.) {
    f <- fixef(.)
    return(f["log_days"])
  }
  
  rate_boot <- bootMer(model, rate_fun, nsim = n_boot, parallel = "multicore", ncpus = 4)
  
  waning_rate <- waning_rate %>%
    mutate(
      lower_ci = quantile(rate_boot$t, 0.025, na.rm = TRUE),
      upper_ci = quantile(rate_boot$t, 0.975, na.rm = TRUE)
    )
  
  # Return results
  list(
    model = model,
    predictions = pred_grid,
    waning_rate = waning_rate,
    n_participants = length(unique(model_data$id)),
    n_obs = nrow(model_data)
  )
}

# Post vaccination: function to create marginal predictions (averaged over sex and age)
create_marginal_predictions <- function(model_results) {
  model_results$predictions %>%
    group_by(days_since_dose1, permutation) %>%
    summarise(
      pred_weight = mean(pred_weight, na.rm = TRUE),
      lower = mean(lower, na.rm = TRUE),
      upper = mean(upper, na.rm = TRUE),
      .groups = 'drop'
    )
}

#Post infection:function to create marginal predictions (averaged over sex and age)
create_marginal_predictions_infection <- function(model_results) {
  model_results$predictions %>%
    group_by(days_since_dose1) %>%  
    summarise(
      pred_weight = mean(pred_weight, na.rm = TRUE),
      lower = mean(lower, na.rm = TRUE),
      upper = mean(upper, na.rm = TRUE),
      .groups = 'drop'
    )
}