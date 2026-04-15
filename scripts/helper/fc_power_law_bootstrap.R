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

create_marginal_predictions <- function(model_results, n_boot = 500, n_days = 100) {
  
  model <- model_results$model
  dat <- model@frame
  
  # Demographic weights (using original column names)
  w <- dat %>%
    distinct(id, male, age_bin) %>%
    count(male, age_bin) %>%
    mutate(weight = n / sum(n))
  
  # Prediction grid with original column names
  grid <- expand.grid(
    days_since_dose1 = seq(0, 420, length.out = n_days),
    permutation = levels(dat$permutation),
    male = levels(dat$male),
    age_bin = levels(dat$age_bin),
    stringsAsFactors = FALSE
  ) %>%
    mutate(log_days = log(days_since_dose1 + 0.01))
  
  # Bootstrap statistic: weighted mean prediction on log scale
  stat_fun <- function(.) {
    pred <- predict(., newdata = grid, re.form = NA)
    grid %>%
      mutate(pred = pred) %>%
      left_join(w, by = c("male", "age_bin")) %>%
      group_by(days_since_dose1, permutation) %>%
      summarise(value = weighted.mean(pred, weight), .groups = 'drop') %>%
      pull(value)
  }
  
  set.seed(123)
  boot_obj <- bootMer(model, stat_fun, nsim = n_boot, 
                      type = "parametric",
                      parallel = "multicore", ncpus = 4)
  
  # Point estimates (original model)
  point_pred <- predict(model, newdata = grid, re.form = NA)
  point_ests <- grid %>%
    mutate(pred = point_pred) %>%
    left_join(w, by = c("male", "age_bin")) %>%
    group_by(days_since_dose1, permutation) %>%
    summarise(pred_weight = weighted.mean(pred, weight), .groups = 'drop')
  
  # CIs from bootstrap
  result <- point_ests %>%
    mutate(
      lower = apply(boot_obj$t, 2, quantile, 0.025),
      upper = apply(boot_obj$t, 2, quantile, 0.975),
      across(c(pred_weight, lower, upper), ~ pmax(pmin(exp(.) - 0.01, 100), 0))
    )
  
  return(result)
}

get_marginal_waning <- function(model_results, n_boot = 500) {
  
  model <- model_results$model
  dat <- model@frame
  
  # Demographic weights
  w <- dat %>%
    distinct(id, male, age_bin) %>%
    count(male, age_bin) %>%
    mutate(weight = n / sum(n)) %>%
    select(male, age_bin, weight)
  
  perms <- levels(dat$permutation)
  if (length(perms) < 2) {
    warning("Only one permutation – no comparisons possible.")
    ref_perm <- perms[1]
  } else {
    ref_perm <- perms[1]
  }
  ref_age <- levels(dat$age_bin)[1]
  
  # Marginal rate function (same as before)
  marginal_rate <- function(f, perm) {
    base <- f["log_days"]
    perm_effect <- 0
    if (perm != ref_perm) {
      term <- paste0("log_days:permutation", perm)
      if (term %in% names(f)) perm_effect <- f[term]
    }
    slopes <- numeric(nrow(w))
    for (i in 1:nrow(w)) {
      s <- base + perm_effect
      if (w$male[i] == "1" && "log_days:male1" %in% names(f)) 
        s <- s + f["log_days:male1"]
      if (w$age_bin[i] != ref_age) {
        term <- paste0("log_days:age_bin", w$age_bin[i])
        if (term %in% names(f)) s <- s + f[term]
      }
      slopes[i] <- s
    }
    weighted.mean(slopes, w$weight)
  }
  
  # Bootstrap statistic
  stat_fun <- function(.) {
    f <- fixef(.)
    sapply(perms, function(p) marginal_rate(f, p))
  }
  
  set.seed(123)
  boot_obj <- bootMer(model, stat_fun, nsim = n_boot, 
                      type = "parametric",
                      parallel = "multicore", ncpus = 4)
  
  # Point estimates
  f0 <- fixef(model)
  point_rates <- sapply(perms, function(p) marginal_rate(f0, p))
  
  # Build waning table with comparisons to reference
  result <- data.frame(
    permutation = perms,
    waning_rate = point_rates,
    lower_ci = apply(boot_obj$t, 2, quantile, 0.025),
    upper_ci = apply(boot_obj$t, 2, quantile, 0.975)
  )
  
  # Add comparisons if more than one regimen
  if (length(perms) > 1) {
    ref_idx <- 1
    result$diff_vs_ref <- NA
    result$diff_lower <- NA
    result$diff_upper <- NA
    result$significant <- "No"
    for (i in 2:length(perms)) {
      diff_boot <- boot_obj$t[, i] - boot_obj$t[, ref_idx]
      diff_ci <- quantile(diff_boot, c(0.025, 0.975))
      result$diff_vs_ref[i] <- point_rates[i] - point_rates[ref_idx]
      result$diff_lower[i] <- diff_ci[1]
      result$diff_upper[i] <- diff_ci[2]
      result$significant[i] <- ifelse(!(diff_ci[1] < 0 & diff_ci[2] > 0), "Yes", "No")
    }
    # For reference regimen itself
    result$significant[1] <- "Ref"
  }
  
  return(result)
}

#Post infection:function to create marginal predictions (averaged over sex and age)
create_marginal_predictions_infection <- function(model_results, n_boot = 500, n_days = 100) {
  
  model <- model_results$model
  dat <- model@frame
  
  # Demographic weights
  w <- dat %>%
    distinct(id, male, age_bin) %>%
    count(male, age_bin) %>%
    mutate(weight = n / sum(n))
  
  # Prediction grid 
  grid <- expand.grid(
    days_since_dose1 = seq(0, 420, length.out = n_days),
    male = levels(dat$male),
    age_bin = levels(dat$age_bin),
    stringsAsFactors = FALSE
  ) %>%
    mutate(log_days = log(days_since_dose1 + 0.01))
  
  # Bootstrap statistic: weighted mean prediction on log scale (one value per day)
  stat_fun <- function(.) {
    pred <- predict(., newdata = grid, re.form = NA)
    grid %>%
      mutate(pred = pred) %>%
      left_join(w, by = c("male", "age_bin")) %>%
      group_by(days_since_dose1) %>%
      summarise(value = weighted.mean(pred, weight), .groups = 'drop') %>%
      pull(value)
  }
  
  set.seed(123)
  boot_obj <- bootMer(model, stat_fun, nsim = n_boot, 
                      type = "parametric",
                      parallel = "multicore", ncpus = 4)
  
  # Point estimates (original model)
  point_pred <- predict(model, newdata = grid, re.form = NA)
  point_ests <- grid %>%
    mutate(pred = point_pred) %>%
    left_join(w, by = c("male", "age_bin")) %>%
    group_by(days_since_dose1) %>%
    summarise(pred_weight = weighted.mean(pred, weight), .groups = 'drop')
  
  # CIs from bootstrap
  result <- point_ests %>%
    mutate(
      lower = apply(boot_obj$t, 2, quantile, 0.025),
      upper = apply(boot_obj$t, 2, quantile, 0.975),
      across(c(pred_weight, lower, upper), ~ pmax(pmin(exp(.) - 0.01, 100), 0))
    )
  
  return(result)
}