two_group_test <- function(raw_data, perm_col, value_col, group1, group2, min_n = 5, return_table = FALSE) {
  perm_sym  <- enquo(perm_col)
  value_sym <- enquo(value_col)
  perm_name <- as_name(perm_sym)
  
  data_clean <- raw_data %>%
    filter(!is.na(!!perm_sym), !is.na(!!value_sym)) %>%
    mutate(!!perm_sym := as.character(!!perm_sym))
  
  data_comp <- data_clean %>% filter(!!perm_sym %in% c(group1, group2))
  
  if (!group1 %in% data_comp[[perm_name]]) stop(paste("Group", group1, "missing after NA removal"))
  if (!group2 %in% data_comp[[perm_name]]) stop(paste("Group", group2, "missing after NA removal"))
  
  x <- data_comp %>% filter(!!perm_sym == group1) %>% pull(!!value_sym)
  y <- data_comp %>% filter(!!perm_sym == group2) %>% pull(!!value_sym)
  
  n1 <- length(x); n2 <- length(y)
  
  if (n1 < 2 || n2 < 2) 
    stop(sprintf("Too few observations: %s (n=%d), %s (n=%d)", group1, n1, group2, n2))
  if (n1 < min_n || n2 < min_n) 
    warning(sprintf("Small sample: %s (n=%d), %s (n=%d)", group1, n1, group2, n2))
  
  # Test Normality
  norm1 <- shapiro.test(x)
  norm2 <- shapiro.test(y)
  both_normal <- norm1$p.value > 0.05 && norm2$p.value > 0.05
  
  # Choose test based on normality
  if (both_normal) {
    test_res <- t.test(x, y, var.equal = TRUE)
    test_name <- "Student's t-test"
    test_abbr <- "t-test"
    stat_value <- round(test_res$statistic, 3)
  } else {
    test_res <- wilcox.test(x, y, conf.int = TRUE, exact = FALSE)
    test_name <- "Wilcoxon rank-sum test"
    test_abbr <- "Wilcoxon"
    stat_value <- format(test_res$statistic[[1]], big.mark = ",")
  }
  
  p_raw <- test_res$p.value
  p_fmt <- format(p_raw, scientific = TRUE, digits = 4)
  
  stars <- dplyr::case_when(
    p_raw < 0.001 ~ "***",
    p_raw < 0.01  ~ "**",
    p_raw < 0.05  ~ "*",
    TRUE          ~ ""
  )
  
  # Console output
  cat("\n=== Comparison:", group1, "vs", group2, "===\n")
  cat(sprintf("n = %d (%s) vs n = %d (%s)\n", n1, group1, n2, group2))
  cat(sprintf("Shapiro-Wilk: %s p = %.2g %s\n", 
              group1, norm1$p.value, ifelse(norm1$p.value > 0.05, "(normal)", "(non-normal)")))
  cat(sprintf("              %s p = %.2g %s\n", 
              group2, norm2$p.value, ifelse(norm2$p.value > 0.05, "(normal)", "(non-normal)")))
  cat("Test:", test_name, "\n")
  cat(sprintf("%s = %s | p = %s %s\n\n", test_abbr, stat_value, p_fmt, stars))
  
  # Return table if requested
  if(return_table) {
    return(data.frame(
      Assay = ifelse(grepl("elisa", deparse(substitute(value_col))), "ELISA", "sVNT"),
      Comparison = paste(group1, "vs", group2),
      n1 = n1,
      n2 = n2,
      Test = test_abbr,
      Test_Statistic = as.character(stat_value),
      Significance = ifelse(p_raw < 0.001, "P < 0.001", 
                            ifelse(p_raw < 0.05, paste("P =", round(p_raw, 3)), "ns")),
      stringsAsFactors = FALSE
    ))
  } else {
    invisible(list(
      test = test_res,
      test_name = test_name,
      test_abbr = test_abbr,
      stat_value = stat_value,
      p_value = p_raw,
      p_formatted = p_fmt,
      stars = stars,
      n1 = n1, n2 = n2,
      data1 = x,
      data2 = y
    ))
  }
}



multi_group_test <- function(raw_data, perm_col, value_col, groups, min_n = 5, return_table = FALSE) {
  require(dplyr)
  require(FSA)      
  require(rstatix) 
  
  perm_sym  <- enquo(perm_col)
  value_sym <- enquo(value_col)
  perm_name <- as_name(perm_sym)
  
  # Clean and filter data
  data_clean <- raw_data %>%
    filter(!is.na(!!perm_sym), !is.na(!!value_sym)) %>%
    mutate(!!perm_sym := as.character(!!perm_sym)) %>%
    filter(!!perm_sym %in% groups)
  
  # Check all groups present
  missing <- setdiff(groups, unique(data_clean[[perm_name]]))
  if (length(missing) > 0) {
    stop(paste("Groups missing after NA removal:", paste(missing, collapse = ", ")))
  }
  
  # Extract data per group and get sample sizes
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
  
  # Normality test for each group
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
  
  # Choose global test based on normality
  if (all_normal) {
    cat("\n→ All groups normally distributed → Using one-way ANOVA + Tukey HSD\n")
    formula <- as.formula(paste(as_name(value_sym), "~", as_name(perm_sym)))
    anova_res <- aov(formula, data = data_clean)
    global_p <- summary(anova_res)[[1]][["Pr(>F)"]][1]
    global_test_abbr <- "F"
    global_stat <- round(summary(anova_res)[[1]][["F value"]][1], 1)
    pairwise_test_abbr <- "diff"
    
    posthoc <- TukeyHSD(anova_res)
    pairwise <- as.data.frame(posthoc[[1]]) %>%
      tibble::rownames_to_column("comparison") %>%
      mutate(
        comparison = gsub("-"," vs ", comparison),
        p_adj = `p adj`,
        stat_value = round(diff, 2),
        sig = case_when(p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**", p_adj < 0.05 ~ "*", TRUE ~ "")
      )
    
    cat(sprintf("ANOVA: F = %.3f, p = %.3g %s\n\n", 
                summary(anova_res)[[1]][["F value"]][1], global_p,
                ifelse(global_p < 0.001, "***", ifelse(global_p < 0.01, "**", ifelse(global_p < 0.05, "*", "")))))
    
  } else {
    cat("\n→ At least one group non-normal → Using Kruskal-Wallis + Dunn's test (Bonferroni)\n")
    kw_res <- kruskal.test(as.formula(paste(as_name(value_sym), "~", as_name(perm_sym))), data = data_clean)
    global_p <- kw_res$p.value
    global_test_abbr <- "χ²"
    global_stat <- round(kw_res$statistic, 1)
    pairwise_test_abbr <- "Z"
    
    # Dunn's test with Bonferroni
    dunn_res <- FSA::dunnTest(as.formula(paste(as_name(value_sym), "~", as_name(perm_sym))),
                              data = data_clean, method = "bonferroni")
    
    pairwise <- dunn_res$res %>%
      mutate(
        comparison = paste(Comparison),
        comparison = gsub(" - ", " vs ", comparison),
        stat_value = round(Z, 3),
        p_adj = round(`P.adj`, 6),
        p_fmt = format(p_adj, scientific = TRUE, digits = 3),
        sig = case_when(p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**", p_adj < 0.05 ~ "*", TRUE ~ "")
      )
    
    cat(sprintf("Kruskal-Wallis: χ² = %.3f, p = %.3g %s\n\n", kw_res$statistic, global_p,
                ifelse(global_p < 0.001, "***", ifelse(global_p < 0.01, "**", ifelse(global_p < 0.05, "*", "")))))
  }
  
  # Print pairwise results
  cat("Pairwise comparisons:\n")
  print(pairwise[, c("comparison", if(!all_normal) "stat_value" else "stat_value", "p_adj", "sig")], row.names = FALSE)
  
  # Return table if requested
  if(return_table) {
    # Create table for global test
    global_row <- data.frame(
      Assay = ifelse(grepl("elisa", deparse(substitute(value_col))), "ELISA", "sVNT"),
      Comparison = paste(global_test_abbr, "=", global_stat),
      n1 = NA, n2 = NA,
      Test = global_test_abbr,
      Test_Statistic = as.character(global_stat),
      Significance = ifelse(global_p < 0.001, "P < 0.001",
                            ifelse(global_p < 0.05, paste("P =", round(global_p, 3)), "ns")),
      stringsAsFactors = FALSE
    )
    
    # Create pairwise rows with sample sizes
    pairwise_rows <- pairwise %>%
      rowwise() %>%
      mutate(
        # Extract group names from comparison
        group1 = strsplit(comparison, " vs ")[[1]][1],
        group2 = strsplit(comparison, " vs ")[[1]][2],
        # Get sample sizes for each group
        n1 = n_vec[group1],
        n2 = n_vec[group2],
        Assay = ifelse(grepl("elisa", deparse(substitute(value_col))), "ELISA", "sVNT"),
        Test = pairwise_test_abbr,
        Test_Statistic = as.character(stat_value),
        Significance = ifelse(p_adj < 0.001, "P < 0.001",
                              ifelse(p_adj < 0.05, paste("P =", round(p_adj, 3)), "ns"))
      ) %>%
      ungroup() %>%
      select(Assay, Comparison = comparison, n1, n2, Test, Test_Statistic, Significance)
    
    return(bind_rows(global_row, pairwise_rows))
  } else {
    invisible(list(
      global_test = if(all_normal) anova_res else kw_res,
      global_test_abbr = if(all_normal) "F" else "χ²",
      global_stat = if(all_normal) round(summary(anova_res)[[1]][["F value"]][1], 1) else round(kw_res$statistic, 1),
      global_p = global_p,
      posthoc = pairwise,
      pairwise_test_abbr = if(all_normal) "diff" else "Z",
      normality = normality,
      all_normal = all_normal,
      data_per_group = setNames(group_data$values, group_data[[perm_name]]),
      n_per_group = n_vec
    ))
  }
}