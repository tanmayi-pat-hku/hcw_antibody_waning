multi_group_test <- function(raw_data, perm_col, value_col, groups, min_n = 5) {
  require(dplyr)
  require(FSA)      
  require(rstatix) 
  
  perm_sym  <- enquo(perm_col)
  value_sym <- enquo(value_col)
  perm_name <- as_name(perm_sym)
  
  #Clean and filter data
  data_clean <- raw_data %>%
    filter(!is.na(!!perm_sym), !is.na(!!value_sym)) %>%
    mutate(!!perm_sym := as.character(!!perm_sym)) %>%
    filter(!!perm_sym %in% groups)
  
  #Check all groups present
  missing <- setdiff(groups, unique(data_clean[[perm_name]]))
  if (length(missing) > 0) {
    stop(paste("Groups missing after NA removal:", paste(missing, collapse = ", ")))
  }
  
  #Extract data per group
  group_data <- data_clean %>%
    group_by(!!perm_sym) %>%
    summarise(
      values = list(!!value_sym),
      n = n(),
      .groups = "drop"
    )
  
  n_vec <- setNames(group_data$n, group_data[[perm_name]])
  cat("\n=== Multi-group comparison: ", paste(groups, collapse = " vs "), " ===\n")
  cat("Sample sizes:", paste0(names(n_vec), " (n=", n_vec, ")"), "\n")
  
  if (any(n_vec < min_n)) {
    small <- names(n_vec)[n_vec < min_n]
    warning(paste("Small sample(s):", paste(small, collapse = ", "), "(n <", min_n, ")"))
  }
  
  #Normality test for each group
  normality <- group_data %>%
    mutate(
      shapiro = map(values, ~shapiro.test(.x)),
      W = map_dbl(shapiro, "statistic"),
      p = map_dbl(shapiro, "p.value"),
      normal = p > 0.05
    )
  
  all_normal <- all(normality$normal)
  
  cat("Shapiro-Wilk normality:\n")
  for (i in seq_along(groups)) {
    g <- groups[i]
    pval <- normality$p[normality[[perm_name]] == g]
    status <- ifelse(pval > 0.05, "(normal)", "(non-normal)")
    cat(sprintf("  %s: p = %.2g %s\n", g, pval, status))
  }
  
  #Choose global test
  if (all_normal) {
    cat("\n→ All groups normally distributed → Using one-way ANOVA + Tukey HSD\n")
    formula <- as.formula(paste(as_name(value_sym), "~", as_name(perm_sym)))
    anova_res <- aov(formula, data = data_clean)
    global_p <- summary(anova_res)[[1]][["Pr(>F)"]][1]
    
    posthoc <- TukeyHSD(anova_res)
    pairwise <- as.data.frame(posthoc[[1]]) %>%
      tibble::rownames_to_column("comparison") %>%
      mutate(
        comparison = gsub("-"," vs ", comparison),
        p_adj = `p adj`,
        sig = case_when(p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**", p_adj < 0.05 ~ "*", TRUE ~ "")
      )
    
    cat(sprintf("ANOVA: F = %.3f, p = %.3g %s\n\n", 
                summary(anova_res)[[1]][["F value"]][1], global_p,
                ifelse(global_p < 0.001, "***", ifelse(global_p < 0.01, "**", ifelse(global_p < 0.05, "*", "")))))
    
  } else {
    cat("\n→ At least one group non-normal → Using Kruskal-Wallis + Dunn's test (Bonferroni)\n")
    kw_res <- kruskal.test(as.formula(paste(as_name(value_sym), "~", as_name(perm_sym))), data = data_clean)
    global_p <- kw_res$p.value
    
    # Dunn's test with Bonferroni
    dunn_res <- FSA::dunnTest(as.formula(paste(as_name(value_sym), "~", as_name(perm_sym))),
                              data = data_clean, method = "bonferroni")
    
    pairwise <- dunn_res$res %>%
      mutate(
        comparison = paste(Comparison),
        comparison = gsub(" - ", " vs ", comparison),
        Z = round(Z, 3),
        p_adj = round(`P.adj`, 6),
        p_fmt = format(p_adj, scientific = TRUE, digits = 3),
        sig = case_when(p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**", p_adj < 0.05 ~ "*", TRUE ~ "")
      )
    
    cat(sprintf("Kruskal-Wallis: χ² = %.3f, p = %.3g %s\n\n", kw_res$statistic, global_p,
                ifelse(global_p < 0.001, "***", ifelse(global_p < 0.01, "**", ifelse(global_p < 0.05, "*", "")))))
  }
  
  #Print pairwise results
  cat("Pairwise comparisons:\n")
  print(pairwise[, c("comparison", if(!all_normal) "Z" else "diff", "p_adj" = "p_fmt", "sig")], row.names = FALSE)
  
  invisible(list(
    global_test = if(all_normal) anova_res else kw_res,
    posthoc = pairwise,
    normality = normality,
    all_normal = all_normal,
    data_per_group = setNames(group_data$values, group_data[[perm_name]]),
    n_per_group = n_vec
  ))
}