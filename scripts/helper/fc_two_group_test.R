two_group_test <- function(raw_data, perm_col, value_col, group1, group2, min_n = 5) {
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
  
  #Test Normality
  norm1 <- shapiro.test(x)
  norm2 <- shapiro.test(y)
  both_normal <- norm1$p.value > 0.05 && norm2$p.value > 0.05
  
  if (both_normal) {
    test_res <- t.test(x, y, var.equal = TRUE)
    test_name <- "Student's t-test"
    stat_label <- paste0("t = ", round(test_res$statistic, 3))
  } else {
    test_res <- wilcox.test(x, y, conf.int = TRUE, exact = FALSE)
    test_name <- "Wilcoxon rank-sum test"
    W <- test_res$statistic[[1]]
    stat_label <- paste0("W = ", format(W, big.mark = ","))  # ← FIXED: removed digits=0
  }
  
  #Exact P-value
  p_raw <- test_res$p.value
  p_fmt <- format(p_raw, scientific = TRUE, digits = 4)
  
  stars <- dplyr::case_when(
    p_raw < 0.001 ~ "***",
    p_raw < 0.01  ~ "**",
    p_raw < 0.05  ~ "*",
    TRUE          ~ ""
  )
  
  cat("\n=== Comparison:", group1, "vs", group2, "===\n")
  cat(sprintf("n = %d (%s) vs n = %d (%s)\n", n1, group1, n2, group2))
  cat(sprintf("Shapiro-Wilk: %s p = %.2g %s\n", 
              group1, norm1$p.value, ifelse(norm1$p.value > 0.05, "(normal)", "(non-normal)")))
  cat(sprintf("              %s p = %.2g %s\n", 
              group2, norm2$p.value, ifelse(norm2$p.value > 0.05, "(normal)", "(non-normal)")))
  cat("Test:", test_name, "\n")
  cat(stat_label, "| p =", p_fmt, stars, "\n\n")
  
  invisible(list(
    test = test_res,
    test_name = test_name,
    p_value = p_raw,
    p_formatted = p_fmt,
    stars = stars,
    n1 = n1, n2 = n2,
    data1 = x,
    data2 = y
  ))
}