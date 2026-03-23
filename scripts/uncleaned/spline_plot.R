source("scripts/cleaned/spline_fitting.R")

library(librarian)
shelf(ggnewscale)
shelf(dplyr)
shelf(patchwork)
shelf(ggeffects)


###################################################
# SPLINE VISUALIZATION - JUST THE CURVES
###################################################

# Generate plots

#Write Function
plot_spline_curves <- function(model, data, title) {
  pred <- ggpredict(model, terms = c("days [all]", 
                                     if("perm" %in% names(model@frame)) "perm" else "last_dose_brand"))
  
  ggplot(pred, aes(x = x, y = predicted, color = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
    labs(title = title,
         x = "Days Since Dose 1",
         y = "Log10 Antibody Level",
         color = "Group",
         fill = "Group") +
    theme_minimal() +
    theme(legend.position = "bottom")
}


#After Dose 3
plot_d3 <- plot_spline_curves(model_d3_spline, df_d3_ss_b, 
                              "Antibody Waning After Dose 3 (Spline Model)")


print(plot_d3)

#After Dose 2

plot_d2 <- plot_spline_curves(model_d2_spline, df_d2_continuous,
                                   "Antibody Waning After Dose 2 by Permutation (Spline)")

print(plot_d2)