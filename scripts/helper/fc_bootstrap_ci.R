bootstrap_slope <- function(data, formula, group_var, group_value, n_boot = 500) {
  set.seed(123)
  slopes <- numeric(n_boot)
  
  for(i in 1:n_boot) {
    #Bootstrap sample by ID 
    boot_ids <- sample(unique(data$id), replace = TRUE)
    boot_data <- data %>% filter(id %in% boot_ids)
    
    tryCatch({
      model <- lmer(formula, data = boot_data, REML = FALSE)
      
      #EXTRACT GROUP-SPECIFIC SLOPE using emtrends
      if(group_var == "perm") {
        trends <- emtrends(model, ~ perm, var = "days")
        slope_df <- as.data.frame(trends)
        #Find the slope for the specific permutation
        slopes[i] <- slope_df$days.trend[slope_df$perm == group_value]
      } else {
        trends <- emtrends(model, ~ last_dose_brand, var = "days")
        slope_df <- as.data.frame(trends)
        #Find the slope for the specific brand
        slopes[i] <- slope_df$days.trend[slope_df$last_dose_brand == group_value]
      }
    }, error = function(e) slopes[i] <- NA)
  }
  
  slopes <- slopes[!is.na(slopes)]
  if(length(slopes) == 0) return(list(ci_lower = NA, ci_upper = NA))
  
  ci_lower <- quantile(slopes, 0.025, na.rm = TRUE)
  ci_upper <- quantile(slopes, 0.975, na.rm = TRUE)
  
  return(list(ci_lower = ci_lower, ci_upper = ci_upper))
}