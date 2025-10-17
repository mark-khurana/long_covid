# read packages
libs <- c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","future.apply","gridExtra","scales","patchwork","grid","cowplot"); lapply(libs, library, character.only = TRUE)

# Define a helper function
multiply_columns_by_100 <- function(dt, cols) {
  dt[, (cols) := lapply(.SD, function(x) x * 100), .SDcols = cols]
}

# Columns to multiply for each table
cols_mean_diff <- c("estimate", "estimate.boot", "se", "lower", "upper")
cols_diff_extra <- c("estimate.A", "estimate.B")

#Overall -----------
# Landmark 180, Times 180 ------------------
g_formula_survival_model_fatigue_0_90_landmark180_time_180 <- readRDS(file="g_formula_survival_model_fatigue_0_90_landmark180_time_180.rds")
g_formula_survival_model_headache_0_90_landmark180_time_180 <- readRDS(file="g_formula_survival_model_headache_0_90_landmark180_time_180.rds")

diff_risk_lm180_fatigue <- g_formula_survival_model_fatigue_0_90_landmark180_time_180$diffRisk
ratio_risk_lm180_fatigue <- g_formula_survival_model_fatigue_0_90_landmark180_time_180$ratioRisk
diff_risk_lm180_headache <- g_formula_survival_model_headache_0_90_landmark180_time_180$diffRisk
ratio_risk_lm180_headache <- g_formula_survival_model_headache_0_90_landmark180_time_180$ratioRisk

# Apply to each data table
multiply_columns_by_100(diff_risk_lm180_fatigue, c(cols_diff_extra, cols_mean_diff))
multiply_columns_by_100(diff_risk_lm180_headache, c(cols_diff_extra, cols_mean_diff))

# Risk Difference
diff_risk_lm180_fatigue[, contrast := paste0(B, " vs ", A)]
diff_risk_lm180_headache[, contrast := paste0(B, " vs ", A)]
diff_risk_lm180_fatigue <- diff_risk_lm180_fatigue[!(A == 0 & B == 2) & !(A == 2 & B == 0)]
diff_risk_lm180_headache <- diff_risk_lm180_headache[!(A == 0 & B == 2) & !(A == 2 & B == 0)]
diff_risk_lm180_fatigue[, outcome := "Fatigue"]
diff_risk_lm180_headache[, outcome := "Headache"]
diff_risk_combined <- rbind(diff_risk_lm180_fatigue, diff_risk_lm180_headache)

p2_lm180 <- ggplot(diff_risk_combined, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)") +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank()  # to avoid overlapping with panel.border
  )
p2_lm180

# Risk Ratio
ratio_risk_lm180_fatigue <- ratio_risk_lm180_fatigue[!(A == 0 & B == 2) & !(A == 2 & B == 0)]
ratio_risk_lm180_headache <- ratio_risk_lm180_headache[!(A == 0 & B == 2) & !(A == 2 & B == 0)]
ratio_risk_lm180_fatigue[, contrast := paste0(B, " vs ", A)]
ratio_risk_lm180_headache[, contrast := paste0(B, " vs ", A)]
ratio_risk_lm180_fatigue[, outcome := "Fatigue"]
ratio_risk_lm180_headache[, outcome := "Headache"]
ratio_risk_combined <- rbind(ratio_risk_lm180_fatigue, ratio_risk_lm180_headache)

p3_lm180 <- ggplot(ratio_risk_combined, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(y = "Risk Ratio (95% CI)", x = "Infection Count Comparison") +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank()
  )
p3_lm180

# Landmark 365, Times 180 ------------------
g_formula_survival_model_fatigue_0_90_landmark365_time_180 <- readRDS(file="g_formula_survival_model_fatigue_0_90_landmark365_time_180.rds")
g_formula_survival_model_headache_0_90_landmark365_time_180 <- readRDS(file="g_formula_survival_model_headache_0_90_landmark365_time_180.rds")

diff_risk_lm365_fatigue <- g_formula_survival_model_fatigue_0_90_landmark365_time_180$diffRisk
ratio_risk_lm365_fatigue <- g_formula_survival_model_fatigue_0_90_landmark365_time_180$ratioRisk
diff_risk_lm365_headache <- g_formula_survival_model_headache_0_90_landmark365_time_180$diffRisk
ratio_risk_lm365_headache <- g_formula_survival_model_headache_0_90_landmark365_time_180$ratioRisk

# Apply to each data table
multiply_columns_by_100(diff_risk_lm365_fatigue, c(cols_diff_extra, cols_mean_diff))
multiply_columns_by_100(diff_risk_lm365_headache, c(cols_diff_extra, cols_mean_diff))

# Risk Difference
diff_risk_lm365_fatigue[, contrast := paste0(B, " vs ", A)]
diff_risk_lm365_headache[, contrast := paste0(B, " vs ", A)]
diff_risk_lm365_fatigue <- diff_risk_lm365_fatigue[!(A == 0 & B == 2) & !(A == 2 & B == 0)]
diff_risk_lm365_headache <- diff_risk_lm365_headache[!(A == 0 & B == 2) & !(A == 2 & B == 0)]
diff_risk_lm365_fatigue[, outcome := "Fatigue"]
diff_risk_lm365_headache[, outcome := "Headache"]
diff_risk_combined <- rbind(diff_risk_lm365_fatigue, diff_risk_lm365_headache)

p2_lm365 <- ggplot(diff_risk_combined, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)") +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank()  # to avoid overlapping with panel.border
  )
p2_lm365

# Risk Ratio
ratio_risk_lm365_fatigue <- ratio_risk_lm365_fatigue[!(A == 0 & B == 2) & !(A == 2 & B == 0)]
ratio_risk_lm365_headache <- ratio_risk_lm365_headache[!(A == 0 & B == 2) & !(A == 2 & B == 0)]
ratio_risk_lm365_fatigue[, contrast := paste0(B, " vs ", A)]
ratio_risk_lm365_headache[, contrast := paste0(B, " vs ", A)]
ratio_risk_lm365_fatigue[, outcome := "Fatigue"]
ratio_risk_lm365_headache[, outcome := "Headache"]
ratio_risk_combined <- rbind(ratio_risk_lm365_fatigue, ratio_risk_lm365_headache)

p3_lm365 <- ggplot(ratio_risk_combined, aes(x = contrast, y = estimate)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(y = "Risk Ratio (95% CI)", x = "Infection Count Comparison") +
  facet_wrap(~ outcome, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank()
  )
p3_lm365


# Main figure -------------
# Function to add label grobs (A, B) top-left inside plot
add_label_to_plot <- function(plot, label) {
  ggdraw(plot) + 
    draw_label(label, x = 0.02, y = 0.97, hjust = 0, vjust = 1, size = 14)
}

# Label 365 plots
p2_lm365_lab <- add_label_to_plot(p2_lm365, "A")
p3_lm365_lab <- add_label_to_plot(p3_lm365, "B")
# Combine 365 risk difference and risk ratio side-by-side
one_row_365 <- plot_grid(
  p2_lm365_lab, p3_lm365_lab, 
  ncol = 2, 
  rel_widths = c(1.1, 1), 
  align = "hv"
)
# Combine title + plots vertically
final_365_plot <- plot_grid(
  one_row_365,
  ncol = 1,
  align = "v"
)
final_365_plot

# Supplementary figure ------
# Centered titles on top for each column
title_180 <- ggdraw() + draw_label("Landmark 180d", fontface = 'bold', hjust = 0.5, size = 12)
title_365 <- ggdraw() + draw_label("Landmark 1yr", fontface = 'bold', hjust = 0.5, size = 12)

# Function to add label grobs (A-D) top-left inside the plot area with margin
add_label_to_plot <- function(plot, label) {
  ggdraw(plot) + 
    draw_label(label, x = 0.02, y = 0.97, hjust = 0, vjust = 1, fontface = "bold", size = 14)
}

# Apply label A-D to the individual plots (assumes you have p2_lm180, p2_lm365, p3_lm180, p3_lm365 ready)
p2_lm180_lab <- add_label_to_plot(p2_lm180, "A")
p2_lm365_lab <- add_label_to_plot(p2_lm365, "B")
p3_lm180_lab <- add_label_to_plot(p3_lm180, "C")
p3_lm365_lab <- add_label_to_plot(p3_lm365, "D")

# Arrange the top titles
titles_row <- plot_grid(title_180, title_365, ncol = 2, rel_widths = c(1.1, 1))

# Arrange Risk Difference plots side-by-side
risk_diff_row <- plot_grid(p2_lm180_lab, p2_lm365_lab, ncol = 2, rel_widths = c(1.1, 1), align = "hv")

# Arrange Risk Ratio plots side-by-side
risk_ratio_row <- plot_grid(p3_lm180_lab, p3_lm365_lab, ncol = 2, rel_widths = c(1.1, 1), align = "hv")

# Combine all rows vertically
final_supplementary_plot <- plot_grid(
  titles_row,
  risk_diff_row,
  risk_ratio_row,
  ncol = 1,
  rel_heights = c(0.07, 1, 1),
  align = "v"
)

final_supplementary_plot

# Main
pdf("fatigue_headache_main_figure.pdf", width = 7, height = 5)
grid.draw(final_365_plot)
dev.off()

# Supplementary
pdf("fatigue_headache_supplementary_figure.pdf", width = 8, height = 8)
grid.draw(final_supplementary_plot)
dev.off()

# Baseline characteristics, fatigue  ----------------
baseline_tmerged_fatigue_cohort_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_fatigue_cohort_period_90_delay_0_filtered.rds")
baseline_information_fatigue <- baseline_tmerged_fatigue_cohort_period_90_delay_0_filtered

# Step 1: Subset to row with highest tstop per PERSON_ID
baseline_unique_fatigue <- baseline_information_fatigue %>%
  group_by(PERSON_ID) %>%
  filter(tstop == max(tstop, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Split into two groups
group_event_1 <- baseline_unique_fatigue %>% filter(event == 1)
group_event_0_or_2 <- baseline_unique_fatigue %>% filter(event %in% c(0, 2))

# Step 3: Total numbers
total_people <- nrow(baseline_unique_fatigue)
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

# Statistical Tests -
cat("\n=== Statistical Tests ===\n")

# Chi-square tests for categorical variables
sex_test <- chisq.test(table(baseline_unique_fatigue$sex, baseline_unique_fatigue$event %in% 1))
income_test <- chisq.test(table(baseline_unique_fatigue$income_quantile, baseline_unique_fatigue$event %in% 1))
age_test <- chisq.test(table(baseline_unique_fatigue$age_group, baseline_unique_fatigue$event %in% 1))
region_test <- chisq.test(table(baseline_unique_fatigue$REGIONSKODE, baseline_unique_fatigue$event %in% 1))

# Wilcoxon test for continuous variable
baseline_unique_fatigue$event_binary <- factor(ifelse(baseline_unique_fatigue$event == 1, "event_1", "event_0_or_2"))
charlson_test <- wilcox.test(charlson.index.5yrs ~ event_binary, data = baseline_unique_fatigue)

# Print p-values
cat("Sex - Chi-square p-value:", sex_test$p.value, "\n")
cat("Income Quantile - Chi-square p-value:", income_test$p.value, "\n")
cat("Charlson Index (5yrs) - Wilcoxon p-value:", charlson_test$p.value, "\n")
cat("Age Group - Chi-square p-value:", age_test$p.value, "\n")
cat("Region - Chi-square p-value:", region_test$p.value, "\n")

# Print values for G-computation results
diff_risk_lm365_fatigue
diff_risk_lm180_fatigue
ratio_risk_lm365_fatigue
ratio_risk_lm180_fatigue

# Baseline characteristics, headache  ----------------
baseline_tmerged_headache_cohort_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_headache_cohort_period_90_delay_0_filtered.rds")
baseline_information_headache <- baseline_tmerged_headache_cohort_period_90_delay_0_filtered

# Step 1: Subset to row with highest tstop per PERSON_ID
baseline_unique_headache <- baseline_information_headache %>%
  group_by(PERSON_ID) %>%
  filter(tstop == max(tstop, na.rm = TRUE)) %>%
  ungroup()

# Step 2: Split into two groups
group_event_1 <- baseline_unique_headache %>% filter(event == 1)
group_event_0_or_2 <- baseline_unique_headache %>% filter(event %in% c(0, 2))

# Step 3: Total numbers
total_people <- nrow(baseline_unique_headache)
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

# Statistical Tests 
cat("\n=== Statistical Tests ===\n")

# Chi-square tests for categorical variables
sex_test <- chisq.test(table(baseline_unique_headache$sex, baseline_unique_headache$event %in% 1))
income_test <- chisq.test(table(baseline_unique_headache$income_quantile, baseline_unique_headache$event %in% 1))
age_test <- chisq.test(table(baseline_unique_headache$age_group, baseline_unique_headache$event %in% 1))
region_test <- chisq.test(table(baseline_unique_headache$REGIONSKODE, baseline_unique_headache$event %in% 1))

# Wilcoxon test for continuous variable
baseline_unique_headache$event_binary <- factor(ifelse(baseline_unique_headache$event == 1, "event_1", "event_0_or_2"))
charlson_test <- wilcox.test(charlson.index.5yrs ~ event_binary, data = baseline_unique_headache)

# Print p-values
cat("Sex - Chi-square p-value:", sex_test$p.value, "\n")
cat("Income Quantile - Chi-square p-value:", income_test$p.value, "\n")
cat("Charlson Index (5yrs) - Wilcoxon p-value:", charlson_test$p.value, "\n")
cat("Age Group - Chi-square p-value:", age_test$p.value, "\n")
cat("Region - Chi-square p-value:", region_test$p.value, "\n")

# Results for text

# Print values for G-computation results
diff_risk_lm365_headache
diff_risk_lm180_headache
ratio_risk_lm365_headache
ratio_risk_lm180_headache
