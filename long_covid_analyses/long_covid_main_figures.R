# read packages
lapply(c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","future.apply","gridExtra","scales","patchwork","grid"), library, character.only = TRUE)

# Define a helper function
multiply_columns_by_100 <- function(dt, cols) {
  dt[, (cols) := lapply(.SD, function(x) x * 100), .SDcols = cols]
}

# Columns to multiply for each table
cols_mean_diff <- c("estimate", "estimate.boot", "se", "lower", "upper")
cols_diff_extra <- c("estimate.A", "estimate.B")

#Overall -----------
# Landmark 180, Times 180 ------------------
g_formula_model_LC_cum_0_90_landmark180_time_180 <- readRDS(file="g_formula_model_LC_cum_0_90_landmark180_time_180.rds")
mean_risk_lm180_t180 <- g_formula_model_LC_cum_0_90_landmark180_time_180$meanRisk
diff_risk_lm180_t180 <- g_formula_model_LC_cum_0_90_landmark180_time_180$diffRisk
ratio_risk_lm180_t180 <- g_formula_model_LC_cum_0_90_landmark180_time_180$ratioRisk

# Apply to each data table
multiply_columns_by_100(mean_risk_lm180_t180, cols_mean_diff)
multiply_columns_by_100(diff_risk_lm180_t180, c(cols_diff_extra, cols_mean_diff))

# Plot 1: Absolute Risk
mean_risk_lm180_t180 <- mean_risk_lm180_t180[treatment != "0"]
mean_risk_lm180_t180 <- mean_risk_lm180_t180[treatment != "3+"]

p1_lm180_t180 <- ggplot(mean_risk_lm180_t180, aes(x = treatment, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections") +
  theme_bw()
p1_lm180_t180
# Plot 2: Mean Risk Difference
diff_risk_lm180_t180[, contrast := paste0(B, " vs ", A)]
diff_risk_lm180_t180 <- diff_risk_lm180_t180[!(A == 0 | B == 0)]
diff_risk_lm180_t180 <- diff_risk_lm180_t180[!(A == "3+" | B == "3+")]

p2_lm180_t180 <- ggplot(diff_risk_lm180_t180, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)") +
  theme_bw()
p2_lm180_t180
# Plot 3: Risk Ratio
ratio_risk_lm180_t180 <- ratio_risk_lm180_t180[!(A == 0 | B == 0)]
ratio_risk_lm180_t180 <- ratio_risk_lm180_t180[!(A == "3+" | B == "3+")]

ratio_risk_lm180_t180[, contrast := paste0(B, " vs ", A)]
p3_lm180_t180 <- ggplot(ratio_risk_lm180_t180, aes(x = contrast, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  labs(y = "Risk Ratio (95% CI)", x = "Infection Count Comparison") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme_bw()
p3_lm180_t180


# Landmark 365, Times 180 ------------------
g_formula_model_LC_cum_0_90_landmark365_time_180 <- readRDS(file="g_formula_model_LC_cum_0_90_landmark365_time_180.rds")
mean_risk_lm365_t180 <- g_formula_model_LC_cum_0_90_landmark365_time_180$meanRisk
diff_risk_lm365_t180 <- g_formula_model_LC_cum_0_90_landmark365_time_180$diffRisk
ratio_risk_lm365_t180 <- g_formula_model_LC_cum_0_90_landmark365_time_180$ratioRisk
# Plot 1: Absolute Risk
mean_risk_lm365_t180 <- mean_risk_lm365_t180[treatment != "0"]
mean_risk_lm365_t180 <- mean_risk_lm365_t180[treatment != "3+"]
# Apply to each data table
multiply_columns_by_100(mean_risk_lm365_t180, cols_mean_diff)
multiply_columns_by_100(diff_risk_lm365_t180, c(cols_diff_extra, cols_mean_diff))

p1_lm365_t180 <- ggplot(mean_risk_lm365_t180, aes(x = treatment, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections") +
  theme_bw()
p1_lm365_t180
# Plot 2: Mean Risk Difference
diff_risk_lm365_t180[, contrast := paste0(B, " vs ", A)]
diff_risk_lm365_t180 <- diff_risk_lm365_t180[!(A == 0 | B == 0)]
diff_risk_lm365_t180 <- diff_risk_lm365_t180[!(A == "3+" | B == "3+")]

p2_lm365_t180 <- ggplot(diff_risk_lm365_t180, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)") +
  theme_bw()
p2_lm365_t180
# Plot 3: Risk Ratio
ratio_risk_lm365_t180 <- ratio_risk_lm365_t180[!(A == 0 | B == 0)]
ratio_risk_lm365_t180 <- ratio_risk_lm365_t180[!(A == "3+" | B == "3+")]

ratio_risk_lm365_t180[, contrast := paste0(B, " vs ", A)]
p3_lm365_t180 <- ggplot(ratio_risk_lm365_t180, aes(x = contrast, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  labs(y = "Risk Ratio (95% CI)", x = "Infection Count Comparison") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme_bw()
p3_lm365_t180

# Landmark 730, Times 180 ------------------
g_formula_model_LC_cum_0_90_landmark730_time_180 <- readRDS(file="g_formula_model_LC_cum_0_90_landmark730_time_180.rds")
mean_risk_lm730_t180 <- g_formula_model_LC_cum_0_90_landmark730_time_180$meanRisk
diff_risk_lm730_t180 <- g_formula_model_LC_cum_0_90_landmark730_time_180$diffRisk
ratio_risk_lm730_t180 <- g_formula_model_LC_cum_0_90_landmark730_time_180$ratioRisk
# Plot 1: Absolute Risk
mean_risk_lm730_t180 <- mean_risk_lm730_t180[treatment != "0"]
mean_risk_lm730_t180 <- mean_risk_lm730_t180[treatment != "3+"]
# Apply to each data table
multiply_columns_by_100(mean_risk_lm730_t180, cols_mean_diff)
multiply_columns_by_100(diff_risk_lm730_t180, c(cols_diff_extra, cols_mean_diff))

p1_lm730_t180 <- ggplot(mean_risk_lm730_t180, aes(x = treatment, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections") +
  theme_bw()
p1_lm730_t180
# Plot 2: Mean Risk Difference
diff_risk_lm730_t180[, contrast := paste0(B, " vs ", A)]
diff_risk_lm730_t180 <- diff_risk_lm730_t180[!(A == 0 | B == 0)]
diff_risk_lm730_t180 <- diff_risk_lm730_t180[!(A == "3+" | B == "3+")]

p2_lm730_t180 <- ggplot(diff_risk_lm730_t180, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)") +
  theme_bw()
p2_lm730_t180
# Plot 3: Risk Ratio
ratio_risk_lm730_t180 <- ratio_risk_lm730_t180[!(A == 0 | B == 0)]
ratio_risk_lm730_t180 <- ratio_risk_lm730_t180[!(A == "3+" | B == "3+")]

ratio_risk_lm730_t180[, contrast := paste0(B, " vs ", A)]
p3_lm730_t180 <- ggplot(ratio_risk_lm730_t180, aes(x = contrast, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  labs(y = "Risk Ratio (95% CI)", x = "Infection Count Comparison") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  theme_bw()
p3_lm730_t180

# Main figure, absolute and risk ratios ---------------
# Wrap all plots
p1_lm180_t180_annotated <- p1_lm180_t180 + 
  ggtitle("Landmark 180d") +
  theme(plot.title = element_text(hjust = 0.5), 
        theme(axis.title.y = element_blank()))

p2_lm180_t180_annotated <- p2_lm180_t180

p1_lm365_t180_annotated <- p1_lm365_t180 + 
  ggtitle("Landmark 1yr") + 
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank()
  )

p2_lm365_t180_annotated <- p2_lm365_t180 + 
  theme(axis.title.y = element_blank())

p1_lm730_t180_annotated <- p1_lm730_t180 + 
  ggtitle("Landmark 2yrs") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_blank())

p2_lm730_t180_annotated <- p2_lm730_t180 + 
  theme(axis.title.y = element_blank())


p3_lm180_t180_annotated <- p3_lm180_t180 + 
  theme(plot.title = element_text(hjust = 0.5))

p3_lm365_t180_annotated <- p3_lm365_t180 + 
  theme(plot.title = element_text(hjust = 0.5))  + 
  theme(axis.title.y = element_blank())

p3_lm730_t180_annotated <- p3_lm730_t180 + 
  theme(plot.title = element_text(hjust = 0.5))  + 
  theme(axis.title.y = element_blank())

# Main figure, risk differences for first infection vs. subsequent infection ---------------
#180
g_formula_model_LC_cum_0_90_landmark180_time_180 <- readRDS(file="g_formula_model_LC_cum_0_90_landmark180_time_180.rds")
diff_risk_lm180_t180_new <- g_formula_model_LC_cum_0_90_landmark180_time_180$diffRisk
multiply_columns_by_100(diff_risk_lm180_t180_new, cols_mean_diff)
diff_risk_lm180_t180_new[, contrast := paste0(B, " vs ", A)]
diff_risk_lm180_t180_new <- diff_risk_lm180_t180_new[!(A == "0" & B == "2")]
diff_risk_lm180_t180_new <- diff_risk_lm180_t180_new[!(A == "3+" | B == "3+")]

p2_lm180_t180_new <- ggplot(diff_risk_lm180_t180_new, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)") +
  theme_bw() + 
  ggtitle("Landmark 180d") +
  theme(plot.title = element_text(hjust = 0.5))
p2_lm180_t180_new

#365
g_formula_model_LC_cum_0_90_landmark365_time_180 <- readRDS(file="g_formula_model_LC_cum_0_90_landmark365_time_180.rds")
diff_risk_lm365_t180_new <- g_formula_model_LC_cum_0_90_landmark365_time_180$diffRisk
multiply_columns_by_100(diff_risk_lm365_t180_new, cols_mean_diff)
diff_risk_lm365_t180_new[, contrast := paste0(B, " vs ", A)]
diff_risk_lm365_t180_new <- diff_risk_lm365_t180_new[!(A == "0" & B == "2")]
diff_risk_lm365_t180_new <- diff_risk_lm365_t180_new[!(A == "3+" | B == "3+")]

p2_lm365_t180_new <- ggplot(diff_risk_lm365_t180_new, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)") +
  theme_bw() + 
  ggtitle("Landmark 1yr") +
  theme(plot.title = element_text(hjust = 0.5))
p2_lm365_t180_new
p2_lm365_t180_new_annotated <- p2_lm365_t180_new + 
  theme(axis.title.y = element_blank())

#730
g_formula_model_LC_cum_0_90_landmark730_time_180 <- readRDS(file="g_formula_model_LC_cum_0_90_landmark730_time_180.rds")
diff_risk_lm730_t180_new <- g_formula_model_LC_cum_0_90_landmark730_time_180$diffRisk
multiply_columns_by_100(diff_risk_lm730_t180_new, cols_mean_diff)
diff_risk_lm730_t180_new[, contrast := paste0(B, " vs ", A)]
diff_risk_lm730_t180_new <- diff_risk_lm730_t180_new[!(A == "0" & B == "2")]
diff_risk_lm730_t180_new <- diff_risk_lm730_t180_new[!(A == "3+" | B == "3+")]

p2_lm730_t180_new <- ggplot(diff_risk_lm730_t180_new, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)") +
  theme_bw() + 
  ggtitle("Landmark 2yrs") +
  theme(plot.title = element_text(hjust = 0.5))
p2_lm730_t180_new
p2_lm730_t180_new_annotated <- p2_lm730_t180_new + 
  theme(axis.title.y = element_blank())

#Main plot adjustment
p2_lm180_t180_new_annotated_new <- p2_lm180_t180_new + 
  theme(plot.title = element_blank())
p2_lm365_t180_new_annotated_new <- p2_lm365_t180_new_annotated +theme(plot.title = element_blank())
p2_lm730_t180_new_annotated_new <- p2_lm730_t180_new_annotated +theme(plot.title = element_blank())


#Final plots ------------
#Main Plot Version 1
row_labels <- list(
  textGrob("A", x = unit(0, "npc"), y = unit(1, "npc"),
           just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 14)),
  textGrob("B", x = unit(0, "npc"), y = unit(1, "npc"),
           just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 14)),
  textGrob("C", x = unit(0, "npc"), y = unit(1, "npc"),
           just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 14))
)
row1 <- arrangeGrob(p1_lm180_t180_annotated, p1_lm365_t180_annotated, p1_lm730_t180_annotated, ncol = 3, widths = c(1.1, 1, 1))
row2 <- arrangeGrob(p2_lm180_t180_new_annotated_new, p2_lm365_t180_new_annotated_new, p2_lm730_t180_new_annotated_new, ncol = 3, widths = c(1.1, 1, 1))
row3 <- arrangeGrob(p3_lm180_t180_annotated, p3_lm365_t180_annotated, p3_lm730_t180_annotated, ncol = 3, widths = c(1.1, 1, 1))
main_long_covid_plot_v1 <- arrangeGrob(
  arrangeGrob(row_labels[[1]], row1, ncol = 2, widths = c(0.1, 3.1)),
  arrangeGrob(row_labels[[2]], row2, ncol = 2, widths = c(0.1, 3.1)),
  arrangeGrob(row_labels[[3]], row3, ncol = 2, widths = c(0.1, 3.1)),
  ncol = 1,
  heights = c(1.1, 1, 1)
)
grid.newpage()
grid.draw(main_long_covid_plot_v1)

pdf("main_long_covid_plot_v1.pdf", width = 8, height = 8)  # Adjust size as needed
grid.draw(main_long_covid_plot_v1)
dev.off()

#Version 2
row_labels <- list(
  textGrob("A", x = unit(0, "npc"), y = unit(1, "npc"),
           just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 14)),
  textGrob("B", x = unit(0, "npc"), y = unit(1, "npc"),
           just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 14)),
  textGrob("C", x = unit(0, "npc"), y = unit(1, "npc"),
           just = c("left", "top"), gp = gpar(fontface = "bold", fontsize = 14))
)
row1 <- arrangeGrob(p1_lm180_t180_annotated, p1_lm365_t180_annotated, p1_lm730_t180_annotated, ncol = 3, widths = c(1.1, 1, 1))
row2 <- arrangeGrob(p3_lm180_t180_annotated, p3_lm365_t180_annotated, p3_lm730_t180_annotated, ncol = 3, widths = c(1.1, 1, 1))
main_long_covid_plot_v2 <- arrangeGrob(
  arrangeGrob(row_labels[[1]], row1, ncol = 2, widths = c(0.1, 3.1)),
  arrangeGrob(row_labels[[2]], row2, ncol = 2, widths = c(0.1, 3.1)),
  ncol = 1,
  heights = c(1.1, 1)
)
grid.newpage()
grid.draw(main_long_covid_plot_v2)

# Save as a large high-quality PDF (e.g., A4 or 10x12 inches)
pdf("main_long_covid_plot_v2.pdf", width = 8, height = 5)  # Adjust size as needed
grid.draw(main_long_covid_plot_v2)
dev.off()

risk_differences_long_covid_plot <- grid.arrange(
  # Column 1: Landmark 90 days
  p2_lm180_t180_new, p2_lm365_t180_new_annotated, p2_lm730_t180_new_annotated,
  ncol = 3, 
  widths = c(1.1, 1, 1)
)
risk_differences_long_covid_plot
grid.newpage()
grid.draw(risk_differences_long_covid_plot)

pdf("risk_differences_long_covid_plot.pdf", width = 8, height = 3)  # Adjust size as needed
grid.draw(risk_differences_long_covid_plot)
dev.off()

# Extracting values to create a new table ----------
print_risk_summary <- function(meanRisk, diffRisk, ratioRisk, landmark) {
  cat("\n==============================\n")
  cat("Landmark:", landmark, "days\n")
  cat("==============================\n")
  
  # Absolute risks
  cat("\nAbsolute Risks (% with 95% CI):\n")
  abs_risks <- meanRisk[treatment != "0" & treatment != "3+"]
  abs_risks[, estimate := round(estimate, 2)]
  abs_risks[, lower := round(lower, 2)]
  abs_risks[, upper := round(upper, 2)]
  print(abs_risks[, .(treatment, estimate, lower, upper)])
  
  # Risk differences: explicitly 1 vs 0 and 2 vs 1
  cat("\nRisk Differences (% with 95% CI):\n")
  diff_risks <- diffRisk[(A == "0" & B == "1") | (A == "1" & B == "2")]
  diff_risks[, contrast := paste0(B, " vs ", A)]
  diff_risks[, estimate := round(estimate, 2)]
  diff_risks[, lower := round(lower, 2)]
  diff_risks[, upper := round(upper, 2)]
  print(diff_risks[, .(contrast, estimate, lower, upper)])
  
  # Risk ratios: 2 vs 1 only
  cat("\nRisk Ratios (95% CI):\n")
  ratios <- ratioRisk[A == "1" & B == "2"]
  ratios[, contrast := paste0(B, " vs ", A)]
  ratios[, estimate := round(estimate, 2)]
  ratios[, lower := round(lower, 2)]
  ratios[, upper := round(upper, 2)]
  print(ratios[, .(contrast, estimate, lower, upper)])
}

# Test for all landmarks
print_risk_summary(mean_risk_lm180_t180, diff_risk_lm180_t180_new, ratio_risk_lm180_t180, 180)
print_risk_summary(mean_risk_lm365_t180, diff_risk_lm365_t180_new, ratio_risk_lm365_t180, 365)
print_risk_summary(mean_risk_lm730_t180, diff_risk_lm730_t180_new, ratio_risk_lm730_t180, 730)

# Check proportionality ---------
model_LC <- readRDS(file="model_LC_cum_0_90_2_or_less_infections.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- as.data.table(baseline_tmerged_cohort_3_period_90_delay_0_filtered)
baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less <- baseline_tmerged_cohort_3_period_90_delay_0_filtered[baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups != "3+", ]
baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less$infection_count_groups <- 
  droplevels(baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less$infection_count_groups)

cox_cause1 <- model_LC$models[["Cause 1"]]
ph_test <- cox.zph(cox_cause1)
print(ph_test)
plot(ph_test)

ph_test <- cox.zph(cox_cause1, transform = "identity")
ph_test$table[grep("infection_count_groups", rownames(ph_test$table)), ]

# For infection_count_groups
fit <- survfit(Surv(tstart, tstop, event) ~ infection_count_groups, data = baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less)
pdf("loglog_survival_infection_count_custom_colors.pdf", width = 7, height = 5)

# Define colors for the groups
cols <- c("purple", "black", "blue")

# Plot log(-log) survival curves with axis titles
plot(fit, fun = "cloglog", col = cols, lty = 1:3,
     xlab = "Follow-Up Time (days)", ylab = "log(-log(S(t)))")

# Add legend
legend("topleft", legend = levels(baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less$infection_count_groups),
       col = cols, lty = 1:3)

dev.off()

#Plotting hazard rates over time ----------
model_LC <- readRDS(file="model_LC_cum_0_90_2_or_less_infections.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- as.data.table(baseline_tmerged_cohort_3_period_90_delay_0_filtered)
baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less <- baseline_tmerged_cohort_3_period_90_delay_0_filtered[baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups != "3+", ]

# Sequence of calendar times in days
times_cal <- seq(0, max(dt$cal_stop_num), by=60)  # every 30 days
hazard_list <- list()
# Loop over infection_count_groups
for(g in levels(dt$infection_count_groups)) {
  dt_sub <- dt[infection_count_groups == g]
  
  pred_risk_cal <- predictRisk(
    object = model_LC,
    newdata = dt_sub,
    times = times_cal,
    cause = 1
  )
  
  mean_risk_cal <- colMeans(pred_risk_cal, na.rm = TRUE)
  hazard_cal <- c(NA, diff(mean_risk_cal)/diff(times_cal))
  
  hazard_list[[g]] <- data.frame(
    cal_time = ref_date + times_cal,
    hazard = hazard_cal,
    group = g
  )
}

# Combine all groups
plot_df <- do.call(rbind, hazard_list)

# Plot
hazard_by_infection_count <- ggplot(plot_df, aes(x = cal_time, y = hazard, color = group)) +
  geom_line(size = 1) +
  scale_color_viridis_d(option = "plasma", end = 0.9) + # "plasma" gives green → purple
  labs(
    title = "Estimated Hazard Over Calendar Time by Infection Count",
    x = "Calendar Date",
    y = "Hazard Rate",
    color = "Infection Count"
  ) +
  theme_minimal()
ggsave("hazard_by_infection_count.pdf", plot = hazard_by_infection_count, width = 6, height = 4, units = "in")

# Incidence and hazards over time -------------
Sys.setlocale("LC_TIME", "English")
#Read in survival models
model_LC_cum_0_90 <- readRDS(file="model_LC_cum_0_90.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")

dt <- as.data.table(baseline_tmerged_cohort_3_period_90_delay_0_filtered)
# Compute calendar date of event
dt[, calendar_date := as.Date(first_test_date) + tstop]
# Take only first event per person
dt_first <- dt[event == 1, .SD[which.min(tstop)], by = PERSON_ID]
# Get calendar month
dt_first[, calendar_month := floor_date(calendar_date, unit = "month")]
# Count new cases per month
monthly_new_cases <- dt_first[, .N, by = calendar_month][order(calendar_month)]
#Aggregate per month counts
ggplot(monthly_new_cases, aes(x = calendar_month, y = N)) +
  geom_col(fill = "steelblue") +
  geom_smooth(method = "loess", span = 0.3, color = "darkblue", se = FALSE) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",  # Abbreviated month and full year in English, e.g. "Jun 2025"
    expand = expansion(add = c(0, 0))
  ) +
  labs(
    title = "Number of New Cases per Month",
    x = "Calendar Month",
    y = "New Cases"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # rotate for readability
  )


#Cumulative -----
# Calculate cumulative sum of cases (absolute counts)
monthly_new_cases[, cumulative_cases := cumsum(N)]
# Plot cumulative cases over time
cumulative_incidence <- ggplot(monthly_new_cases, aes(x = calendar_month, y = cumulative_cases)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(color = "black") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(add = c(0, 0))
  ) +
  labs(
    title = "Cumulative Long Covid Incidence",
    x = "Calendar Month",
    y = "Cumulative Number of Cases"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
pdf("cumulative_incidence.pdf", width = 7, height = 3.5)  # Adjust size as needed
grid.draw(cumulative_incidence)
dev.off()

# Date of entrance into cohort -------------
Sys.setlocale("LC_TIME", "English")
#Read in survival models
model_LC_cum_0_90 <- readRDS(file="model_LC_cum_0_90.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")
dt <- as.data.table(baseline_tmerged_cohort_3_period_90_delay_0_filtered)
# Compute calendar date of event
dt[, calendar_date := as.Date(first_test_date) + tstop]
# One data point per person: first test date
dt_first <- dt[, .(first_test_date = min(as.Date(first_test_date), na.rm = TRUE)), by = PERSON_ID]
# Extract calendar month
dt_first[, calendar_month := floor_date(first_test_date, unit = "month")]
# Count number of first tests per month
monthly_first_tests <- dt_first[, .N, by = calendar_month][order(calendar_month)]
# Plot
study_entrance <- ggplot(monthly_first_tests, aes(x = calendar_month, y = N)) +
  geom_col(fill = "steelblue") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = expansion(add = c(0, 0))
  ) +
  labs(x = "Calendar Month",
    y = "Number of People Entering Study") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
study_entrance
ggsave("study_entrance_over_time.pdf", plot = study_entrance, width = 10, height = 6)

# Baseline characteristics -------------------
baseline_information <- baseline_tmerged_cohort_3_period_90_delay_0_filtered
# Step 1: Subset to row with highest tstop per PERSON_ID
baseline_unique <- baseline_information %>%
  group_by(PERSON_ID) %>%
  filter(tstop == max(tstop, na.rm = TRUE)) %>%
  ungroup()
# Step 2: Split into two groups
group_event_1 <- baseline_unique %>% filter(event == 1)
group_event_0_or_2 <- baseline_unique %>% filter(event %in% c(0, 2))
# Step 3: Total numbers
total_people <- nrow(baseline_unique)
total_event_1 <- nrow(group_event_1)
total_event_0_or_2 <- nrow(group_event_0_or_2)
cat("Total people:", total_people, "\n")
cat("Group event = 1:", total_event_1, "\n")
cat("Group event = 0 or 2:", total_event_0_or_2, "\n\n")

# Function to summarize counts and percentages for categorical variables
summarize_group <- function(df) {
  get_pct <- function(x) {
    tbl <- table(x)
    pct <- prop.table(tbl) * 100
    paste0(as.vector(tbl), " (", sprintf("%.1f", pct), "%)")
  }
  
  charlson_median <- median(df$charlson.index.5yrs, na.rm = TRUE)
  charlson_q <- quantile(df$charlson.index.5yrs, probs = c(0.25, 0.75), na.rm = TRUE)
  charlson_str <- paste0(charlson_median, " (", charlson_q[1], "–", charlson_q[2], ")")
  
  list(
    Sex = get_pct(df$sex),
    Income_Quantile = get_pct(df$income_quantile),
    Charlson_Index_5yrs = charlson_str,
    Age_Group = get_pct(df$age_group),
    Region = get_pct(df$REGIONSKODE)  # <-- Added REGIONSKODE
  )
}

# Summarize each group
summary_event_1 <- summarize_group(group_event_1)
summary_event_0_or_2 <- summarize_group(group_event_0_or_2)

# Print summaries
cat("=== Summary for Group event == 1 ===\n")
print(summary_event_1)

cat("\n=== Summary for Group event == 0 or 2 ===\n")
print(summary_event_0_or_2)

# Statistical Tests --------------------------------
cat("\n=== Statistical Tests ===\n")
# Chi-square tests for categorical variables
sex_test <- chisq.test(table(baseline_unique$sex, baseline_unique$event %in% 1))
income_test <- chisq.test(table(baseline_unique$income_quantile, baseline_unique$event %in% 1))
age_test <- chisq.test(table(baseline_unique$age_group, baseline_unique$event %in% 1))
region_test <- chisq.test(table(baseline_unique$REGIONSKODE, baseline_unique$event %in% 1))
# Wilcoxon test for continuous variable
baseline_unique$event_binary <- factor(ifelse(baseline_unique$event == 1, "event_1", "event_0_or_2"))
charlson_test <- wilcox.test(charlson.index.5yrs ~ event_binary, data = baseline_unique)

# Print p-values
cat("Sex - Chi-square p-value:", sex_test$p.value, "\n")
cat("Income Quantile - Chi-square p-value:", income_test$p.value, "\n")
cat("Charlson Index (5yrs) - Wilcoxon p-value:", charlson_test$p.value, "\n")
cat("Age Group - Chi-square p-value:", age_test$p.value, "\n")
cat("Region - Chi-square p-value:", region_test$p.value, "\n")

# Results for text -------------------
# Print values for G-computation results
diff_risk_lm180_t180
diff_risk_lm365_t180
diff_risk_lm730_t180
ratio_risk_lm180_t180
ratio_risk_lm365_t180
ratio_risk_lm730_t180
mean_risk_lm180_t180
mean_risk_lm365_t180
mean_risk_lm730_t180

#Risk differences for 0v1 and 1vs2
diff_risk_lm180_t180_new
diff_risk_lm365_t180_new
diff_risk_lm730_t180_new