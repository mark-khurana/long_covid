# read packages
lapply(c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","future.apply","gridExtra","scales","patchwork","grid"), library, character.only = TRUE)

# Define a helper function
multiply_columns_by_100 <- function(dt, cols) {
  dt[, (cols) := lapply(.SD, function(x) x * 100), .SDcols = cols]
}

# Columns to multiply for each table
cols_mean_diff <- c("estimate", "estimate.boot", "se", "lower", "upper")
cols_diff_extra <- c("estimate.A", "estimate.B")

#Sex-specific -----------
# --- Landmark 365d, Time 180 -
g_formula_model_male_lm365_t180 <- readRDS("g_formula_model_male_landmark365_time_180.rds")
g_formula_model_female_lm365_t180 <- readRDS("g_formula_model_female_landmark365_time_180.rds")

# Mean Risk
mean_risk_male_lm365_t180 <- g_formula_model_male_lm365_t180$meanRisk
mean_risk_female_lm365_t180 <- g_formula_model_female_lm365_t180$meanRisk
mean_risk_male_lm365_t180$sex <- "Male"
mean_risk_female_lm365_t180$sex <- "Female"
mean_risk_lm365_t180_sex <- rbind(mean_risk_male_lm365_t180, mean_risk_female_lm365_t180)
mean_risk_lm365_t180_sex <- mean_risk_lm365_t180_sex[mean_risk_lm365_t180_sex$treatment != "0", ]
multiply_columns_by_100(mean_risk_lm365_t180_sex, cols_mean_diff)

p1_lm365_t180_sex <- ggplot(mean_risk_lm365_t180_sex, aes(x = treatment, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "Sex") +
  ggtitle("Landmark 365d") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Risk Difference
diff_risk_male_lm365_t180 <- g_formula_model_male_lm365_t180$diffRisk
diff_risk_female_lm365_t180 <- g_formula_model_female_lm365_t180$diffRisk
diff_risk_male_lm365_t180$sex <- "Male"
diff_risk_female_lm365_t180$sex <- "Female"
diff_risk_lm365_t180_sex <- rbind(diff_risk_male_lm365_t180, diff_risk_female_lm365_t180)
diff_risk_lm365_t180_sex <- diff_risk_lm365_t180_sex[!(diff_risk_lm365_t180_sex$A == 0 | diff_risk_lm365_t180_sex$B == 0), ]
multiply_columns_by_100(diff_risk_lm365_t180_sex, c(cols_diff_extra, cols_mean_diff))

diff_risk_lm365_t180_sex$contrast <- paste0(diff_risk_lm365_t180_sex$A, " vs ", diff_risk_lm365_t180_sex$B)

p2_lm365_t180_sex <- ggplot(diff_risk_lm365_t180_sex, aes(x = contrast, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom")

# Risk Ratio
ratio_risk_male_lm365_t180 <- g_formula_model_male_lm365_t180$ratioRisk
ratio_risk_female_lm365_t180 <- g_formula_model_female_lm365_t180$ratioRisk
ratio_risk_male_lm365_t180$sex <- "Male"
ratio_risk_female_lm365_t180$sex <- "Female"
ratio_risk_lm365_t180_sex <- rbind(ratio_risk_male_lm365_t180, ratio_risk_female_lm365_t180)
ratio_risk_lm365_t180_sex <- ratio_risk_lm365_t180_sex[!(ratio_risk_lm365_t180_sex$A == 0 | ratio_risk_lm365_t180_sex$B == 0), ]
ratio_risk_lm365_t180_sex$contrast <- paste0(ratio_risk_lm365_t180_sex$A, " vs ", ratio_risk_lm365_t180_sex$B)

p3_lm365_t180_sex <- ggplot(ratio_risk_lm365_t180_sex, aes(x = contrast, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom")

# --- Landmark 730d, Time 180 -
g_formula_model_male_lm730_t180 <- readRDS("g_formula_model_male_landmark730_time_180.rds")
g_formula_model_female_lm730_t180 <- readRDS("g_formula_model_female_landmark730_time_180.rds")

# Mean Risk
mean_risk_male_lm730_t180 <- g_formula_model_male_lm730_t180$meanRisk
mean_risk_female_lm730_t180 <- g_formula_model_female_lm730_t180$meanRisk
mean_risk_male_lm730_t180$sex <- "Male"
mean_risk_female_lm730_t180$sex <- "Female"
mean_risk_lm730_t180_sex <- rbind(mean_risk_male_lm730_t180, mean_risk_female_lm730_t180)
mean_risk_lm730_t180_sex <- mean_risk_lm730_t180_sex[mean_risk_lm730_t180_sex$treatment != "0", ]
multiply_columns_by_100(mean_risk_lm730_t180_sex, cols_mean_diff)


p1_lm730_t180_sex <- ggplot(mean_risk_lm730_t180_sex, aes(x = treatment, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "Sex") +
  ggtitle("Landmark 2yrs") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        legend.position = "bottom")

# Risk Difference
diff_risk_male_lm730_t180 <- g_formula_model_male_lm730_t180$diffRisk
diff_risk_female_lm730_t180 <- g_formula_model_female_lm730_t180$diffRisk
diff_risk_male_lm730_t180$sex <- "Male"
diff_risk_female_lm730_t180$sex <- "Female"
diff_risk_lm730_t180_sex <- rbind(diff_risk_male_lm730_t180, diff_risk_female_lm730_t180)
diff_risk_lm730_t180_sex <- diff_risk_lm730_t180_sex[!(diff_risk_lm730_t180_sex$A == 0 | diff_risk_lm730_t180_sex$B == 0), ]
diff_risk_lm730_t180_sex$contrast <- paste0(diff_risk_lm730_t180_sex$A, " vs ", diff_risk_lm730_t180_sex$B)
multiply_columns_by_100(diff_risk_lm730_t180_sex, c(cols_diff_extra, cols_mean_diff))

p2_lm730_t180_sex <- ggplot(diff_risk_lm730_t180_sex, aes(x = contrast, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "Sex") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")

# Risk Ratio
ratio_risk_male_lm730_t180 <- g_formula_model_male_lm730_t180$ratioRisk
ratio_risk_female_lm730_t180 <- g_formula_model_female_lm730_t180$ratioRisk
ratio_risk_male_lm730_t180$sex <- "Male"
ratio_risk_female_lm730_t180$sex <- "Female"
ratio_risk_lm730_t180_sex <- rbind(ratio_risk_male_lm730_t180, ratio_risk_female_lm730_t180)
ratio_risk_lm730_t180_sex <- ratio_risk_lm730_t180_sex[!(ratio_risk_lm730_t180_sex$A == 0 | ratio_risk_lm730_t180_sex$B == 0), ]
ratio_risk_lm730_t180_sex$contrast <- paste0(ratio_risk_lm730_t180_sex$A, " vs ", ratio_risk_lm730_t180_sex$B)

p3_lm730_t180_sex <- ggplot(ratio_risk_lm730_t180_sex, aes(x = contrast, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "Sex") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")


# --- Landmark 180d, Time 180 -
g_formula_model_male_lm180_t180 <- readRDS("g_formula_model_male_landmark180_time_180.rds")
g_formula_model_female_lm180_t180 <- readRDS("g_formula_model_female_landmark180_time_180.rds")

# Mean Risk
mean_risk_male_lm180_t180 <- g_formula_model_male_lm180_t180$meanRisk
mean_risk_female_lm180_t180 <- g_formula_model_female_lm180_t180$meanRisk
mean_risk_male_lm180_t180$sex <- "Male"
mean_risk_female_lm180_t180$sex <- "Female"
mean_risk_lm180_t180_sex <- rbind(mean_risk_male_lm180_t180, mean_risk_female_lm180_t180)
mean_risk_lm180_t180_sex <- mean_risk_lm180_t180_sex[mean_risk_lm180_t180_sex$treatment != "0", ]
multiply_columns_by_100(mean_risk_lm180_t180_sex, cols_mean_diff)


p1_lm180_t180_sex <- ggplot(mean_risk_lm180_t180_sex, aes(x = treatment, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "Sex") +
  ggtitle("Landmark 2yrs") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        legend.position = "bottom")

# Risk Difference
diff_risk_male_lm180_t180 <- g_formula_model_male_lm180_t180$diffRisk
diff_risk_female_lm180_t180 <- g_formula_model_female_lm180_t180$diffRisk
diff_risk_male_lm180_t180$sex <- "Male"
diff_risk_female_lm180_t180$sex <- "Female"
diff_risk_lm180_t180_sex <- rbind(diff_risk_male_lm180_t180, diff_risk_female_lm180_t180)
diff_risk_lm180_t180_sex <- diff_risk_lm180_t180_sex[!(diff_risk_lm180_t180_sex$A == 0 | diff_risk_lm180_t180_sex$B == 0), ]
diff_risk_lm180_t180_sex$contrast <- paste0(diff_risk_lm180_t180_sex$A, " vs ", diff_risk_lm180_t180_sex$B)
multiply_columns_by_100(diff_risk_lm180_t180_sex, c(cols_diff_extra, cols_mean_diff))

p2_lm180_t180_sex <- ggplot(diff_risk_lm180_t180_sex, aes(x = contrast, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "Sex") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom")

# Risk Ratio
ratio_risk_male_lm180_t180 <- g_formula_model_male_lm180_t180$ratioRisk
ratio_risk_female_lm180_t180 <- g_formula_model_female_lm180_t180$ratioRisk
ratio_risk_male_lm180_t180$sex <- "Male"
ratio_risk_female_lm180_t180$sex <- "Female"
ratio_risk_lm180_t180_sex <- rbind(ratio_risk_male_lm180_t180, ratio_risk_female_lm180_t180)
ratio_risk_lm180_t180_sex <- ratio_risk_lm180_t180_sex[!(ratio_risk_lm180_t180_sex$A == 0 | ratio_risk_lm180_t180_sex$B == 0), ]
ratio_risk_lm180_t180_sex$contrast <- paste0(ratio_risk_lm180_t180_sex$A, " vs ", ratio_risk_lm180_t180_sex$B)

p3_lm180_t180_sex <- ggplot(ratio_risk_lm180_t180_sex, aes(x = contrast, y = estimate, color = sex)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = c("Male" = "#21908C", "Female" = "#440154")) +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "Sex") +
  theme_bw() +
  theme(legend.position = "bottom") 


#Socioeconomic status -----------

# SES groups
ses_groups <- c("Q1", "Q2", "Q3", "Q4")
# Initialize empty lists to store data frames
mean_risk_list_180_SES <- list()
diff_risk_list_180_SES <- list()
ratio_risk_list_180_SES <- list()
mean_risk_list_365_SES <- list()
diff_risk_list_365_SES <- list()
ratio_risk_list_365_SES <- list()
mean_risk_list_730_SES <- list()
diff_risk_list_730_SES <- list()
ratio_risk_list_730_SES <- list()

for (ses in ses_groups) {
  # Load the model RDS
  model <- readRDS(paste0("g_formula_model_SES_", ses, "_landmark365_time_180.rds"))
  
  # Extract Mean Risk and add SES column
  mean_risk <- model$meanRisk
  mean_risk$SES <- ses
  mean_risk_list_365_SES[[ses]] <- mean_risk
  
  # Extract Risk Difference and add SES column
  diff_risk <- model$diffRisk
  diff_risk$SES <- ses
  diff_risk_list_365_SES[[ses]] <- diff_risk
  
  # Extract Risk Ratio and add SES column
  ratio_risk <- model$ratioRisk
  ratio_risk$SES <- ses
  ratio_risk_list_365_SES[[ses]] <- ratio_risk
}

for (ses in ses_groups) {
  # Load the model RDS
  model <- readRDS(paste0("g_formula_model_SES_", ses, "_landmark180_time_180.rds"))
  
  # Extract Mean Risk and add SES column
  mean_risk <- model$meanRisk
  mean_risk$SES <- ses
  mean_risk_list_180_SES[[ses]] <- mean_risk
  
  # Extract Risk Difference and add SES column
  diff_risk <- model$diffRisk
  diff_risk$SES <- ses
  diff_risk_list_180_SES[[ses]] <- diff_risk
  
  # Extract Risk Ratio and add SES column
  ratio_risk <- model$ratioRisk
  ratio_risk$SES <- ses
  ratio_risk_list_180_SES[[ses]] <- ratio_risk
}

for (ses in ses_groups) {
  # Load the model RDS
  model <- readRDS(paste0("g_formula_model_SES_", ses, "_landmark730_time_180.rds"))
  
  # Extract Mean Risk and add SES column
  mean_risk <- model$meanRisk
  mean_risk$SES <- ses
  mean_risk_list_730_SES[[ses]] <- mean_risk
  
  # Extract Risk Difference and add SES column
  diff_risk <- model$diffRisk
  diff_risk$SES <- ses
  diff_risk_list_730_SES[[ses]] <- diff_risk
  
  # Extract Risk Ratio and add SES column
  ratio_risk <- model$ratioRisk
  ratio_risk$SES <- ses
  ratio_risk_list_730_SES[[ses]] <- ratio_risk
}

# Combine all SES groups
mean_risk_lm180_t180_SES <- do.call(rbind, mean_risk_list_180_SES)
diff_risk_lm180_t180_SES <- do.call(rbind, diff_risk_list_180_SES)
ratio_risk_lm180_t180_SES <- do.call(rbind, ratio_risk_list_180_SES)
mean_risk_lm365_t180_SES <- do.call(rbind, mean_risk_list_365_SES)
diff_risk_lm365_t180_SES <- do.call(rbind, diff_risk_list_365_SES)
ratio_risk_lm365_t180_SES <- do.call(rbind, ratio_risk_list_365_SES)
mean_risk_lm730_t180_SES <- do.call(rbind, mean_risk_list_730_SES)
diff_risk_lm730_t180_SES <- do.call(rbind, diff_risk_list_730_SES)
ratio_risk_lm730_t180_SES <- do.call(rbind, ratio_risk_list_730_SES)

# Filter out treatment = 0 for mean risk
mean_risk_lm365_t180_SES <- mean_risk_lm365_t180_SES[mean_risk_lm365_t180_SES$treatment != "0", ]
multiply_columns_by_100(mean_risk_lm365_t180_SES, cols_mean_diff)
diff_risk_lm365_t180_SES <- diff_risk_lm365_t180_SES[!(diff_risk_lm365_t180_SES$A == 0 | diff_risk_lm365_t180_SES$B == 0), ]
diff_risk_lm365_t180_SES$contrast <- paste0(diff_risk_lm365_t180_SES$A, " vs ", diff_risk_lm365_t180_SES$B)
multiply_columns_by_100(diff_risk_lm180_t180_SES, c(cols_diff_extra, cols_mean_diff))
ratio_risk_lm365_t180_SES <- ratio_risk_lm365_t180_SES[!(ratio_risk_lm365_t180_SES$A == 0 | ratio_risk_lm365_t180_SES$B == 0), ]
ratio_risk_lm365_t180_SES$contrast <- paste0(ratio_risk_lm365_t180_SES$A, " vs ", ratio_risk_lm365_t180_SES$B)

mean_risk_lm180_t180_SES <- mean_risk_lm180_t180_SES[mean_risk_lm180_t180_SES$treatment != "0", ]
multiply_columns_by_100(mean_risk_lm180_t180_SES, cols_mean_diff)
diff_risk_lm180_t180_SES <- diff_risk_lm180_t180_SES[!(diff_risk_lm180_t180_SES$A == 0 | diff_risk_lm180_t180_SES$B == 0), ]
diff_risk_lm180_t180_SES$contrast <- paste0(diff_risk_lm180_t180_SES$A, " vs ", diff_risk_lm180_t180_SES$B)
multiply_columns_by_100(diff_risk_lm180_t180_SES, c(cols_diff_extra, cols_mean_diff))
ratio_risk_lm180_t180_SES <- ratio_risk_lm180_t180_SES[!(ratio_risk_lm180_t180_SES$A == 0 | ratio_risk_lm180_t180_SES$B == 0), ]
ratio_risk_lm180_t180_SES$contrast <- paste0(ratio_risk_lm180_t180_SES$A, " vs ", ratio_risk_lm180_t180_SES$B)

mean_risk_lm730_t180_SES <- mean_risk_lm730_t180_SES[mean_risk_lm730_t180_SES$treatment != "0", ]
multiply_columns_by_100(mean_risk_lm180_t180_SES, cols_mean_diff)
diff_risk_lm730_t180_SES <- diff_risk_lm730_t180_SES[!(diff_risk_lm730_t180_SES$A == 0 | diff_risk_lm730_t180_SES$B == 0), ]
diff_risk_lm730_t180_SES$contrast <- paste0(diff_risk_lm730_t180_SES$A, " vs ", diff_risk_lm730_t180_SES$B)
multiply_columns_by_100(diff_risk_lm180_t180_SES, c(cols_diff_extra, cols_mean_diff))
ratio_risk_lm730_t180_SES <- ratio_risk_lm730_t180_SES[!(ratio_risk_lm730_t180_SES$A == 0 | ratio_risk_lm730_t180_SES$B == 0), ]
ratio_risk_lm730_t180_SES$contrast <- paste0(ratio_risk_lm730_t180_SES$A, " vs ", ratio_risk_lm730_t180_SES$B)

# 365 --
# Mean Risks
p1_lm365_t180_SES <- ggplot(mean_risk_lm365_t180_SES, aes(x = treatment, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "") +
  ggtitle("Landmark 365d by SES") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
# Risk Difference
p2_lm365_t180_SES <- ggplot(diff_risk_lm365_t180_SES, aes(x = contrast, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "Income Quartile") +
  theme_bw() +
  theme(legend.position = "bottom")
# Risk Ratio
p3_lm365_t180_SES <- ggplot(ratio_risk_lm365_t180_SES, aes(x = contrast, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "Income Quartile") +
  theme_bw() +
  theme(legend.position = "bottom")

# 180 --
# Mean Risks
p1_lm180_t180_SES <- ggplot(mean_risk_lm180_t180_SES, aes(x = treatment, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "Income Quartile") +
  ggtitle("Landmark 180d by SES") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
# Risk Difference
p2_lm180_t180_SES <- ggplot(diff_risk_lm180_t180_SES, aes(x = contrast, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "Income Quartile") +
  theme_bw() +
  theme(legend.position = "bottom")
# Risk Ratio
p3_lm180_t180_SES <- ggplot(ratio_risk_lm180_t180_SES, aes(x = contrast, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "Income Quartile") +
  theme_bw() +
  theme(legend.position = "bottom")

# 730 --
# Mean Risks
p1_lm730_t180_SES <- ggplot(mean_risk_lm730_t180_SES, aes(x = treatment, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "Income Quartile") +
  ggtitle("Landmark 730d by SES") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")
# Risk Difference
p2_lm730_t180_SES <- ggplot(diff_risk_lm730_t180_SES, aes(x = contrast, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "Income Quartile") +
  theme_bw() +
  theme(legend.position = "bottom")
# Risk Ratio
p3_lm730_t180_SES <- ggplot(ratio_risk_lm730_t180_SES, aes(x = contrast, y = estimate, color = SES)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "Income Quartile") +
  theme_bw() +
  theme(legend.position = "bottom")

#Vaccination status -----------
okabe_ito_colors <- c("0" = "#F0E442", "1" = "#E69F00", "2" = "#56B4E9", "3" = "#009E73")
# vacc groups
vacc_groups <- c("0vacc", "1vacc", "2vacc", "3vacc")

#365 days
mean_risk_list_365_vacc <- list()
diff_risk_list_365_vacc <- list()
ratio_risk_list_365_vacc <- list()

for (vacc in vacc_groups) {
  # Load the model RDS
  model <- readRDS(paste0("g_formula_model_SES_", vacc, "_landmark365_time_180.rds"))
  
  # Clean vacc label (e.g., "0vacc" → "0")
  vacc_label <- gsub("vacc", "", vacc)
  
  # Extract Mean Risk and add cleaned vacc column
  mean_risk <- model$meanRisk
  mean_risk$vacc <- vacc_label
  mean_risk_list_365_vacc[[vacc]] <- mean_risk
  
  # Extract Risk Difference and add cleaned vacc column
  diff_risk <- model$diffRisk
  diff_risk$vacc <- vacc_label
  diff_risk_list_365_vacc[[vacc]] <- diff_risk
  
  # Extract Risk Ratio and add cleaned vacc column
  ratio_risk <- model$ratioRisk
  ratio_risk$vacc <- vacc_label
  ratio_risk_list_365_vacc[[vacc]] <- ratio_risk
}

# Combine all vacc groups
mean_risk_lm365_t180_vacc <- do.call(rbind, mean_risk_list_365_vacc)
diff_risk_lm365_t180_vacc <- do.call(rbind, diff_risk_list_365_vacc)
ratio_risk_lm365_t180_vacc <- do.call(rbind, ratio_risk_list_365_vacc)
multiply_columns_by_100(mean_risk_lm365_t180_vacc, cols_mean_diff)
multiply_columns_by_100(diff_risk_lm365_t180_vacc, c(cols_diff_extra, cols_mean_diff))

# Filter out treatment = 0 for mean risk
mean_risk_lm365_t180_vacc <- mean_risk_lm365_t180_vacc[mean_risk_lm365_t180_vacc$treatment != "0", ]
diff_risk_lm365_t180_vacc <- diff_risk_lm365_t180_vacc[!(diff_risk_lm365_t180_vacc$A == 0 | diff_risk_lm365_t180_vacc$B == 0), ]
diff_risk_lm365_t180_vacc$contrast <- paste0(diff_risk_lm365_t180_vacc$A, " vs ", diff_risk_lm365_t180_vacc$B)
ratio_risk_lm365_t180_vacc <- ratio_risk_lm365_t180_vacc[!(ratio_risk_lm365_t180_vacc$A == 0 | ratio_risk_lm365_t180_vacc$B == 0), ]
ratio_risk_lm365_t180_vacc$contrast <- paste0(ratio_risk_lm365_t180_vacc$A, " vs ", ratio_risk_lm365_t180_vacc$B)

# Plot Mean Risk
p1_lm365_t180_vacc <- ggplot(mean_risk_lm365_t180_vacc, aes(x = treatment, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "# Vaccinations") +
  ggtitle("Landmark 365d by vacc") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Plot Risk Difference
p2_lm365_t180_vacc <- ggplot(diff_risk_lm365_t180_vacc, aes(x = contrast, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "# Vaccinations") +
  theme_bw() +
  theme(legend.position = "bottom")

# Plot Risk Ratio
p3_lm365_t180_vacc <- ggplot(ratio_risk_lm365_t180_vacc, aes(x = contrast, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "# Vaccinations") +
  theme_bw() +
  theme(legend.position = "bottom")




#180 days
mean_risk_list_180_vacc <- list()
diff_risk_list_180_vacc <- list()
ratio_risk_list_180_vacc <- list()

for (vacc in vacc_groups) {
  # Load the model RDS
  model <- readRDS(paste0("g_formula_model_SES_", vacc, "_landmark180_time_180.rds"))
  
  # Clean vacc label (e.g., "0vacc" → "0")
  vacc_label <- gsub("vacc", "", vacc)
  
  # Extract Mean Risk and add cleaned vacc column
  mean_risk <- model$meanRisk
  mean_risk$vacc <- vacc_label
  mean_risk_list_180_vacc[[vacc]] <- mean_risk
  
  # Extract Risk Difference and add cleaned vacc column
  diff_risk <- model$diffRisk
  diff_risk$vacc <- vacc_label
  diff_risk_list_180_vacc[[vacc]] <- diff_risk
  
  # Extract Risk Ratio and add cleaned vacc column
  ratio_risk <- model$ratioRisk
  ratio_risk$vacc <- vacc_label
  ratio_risk_list_180_vacc[[vacc]] <- ratio_risk
}

# Combine all vacc groups
mean_risk_lm180_t180_vacc <- do.call(rbind, mean_risk_list_180_vacc)
diff_risk_lm180_t180_vacc <- do.call(rbind, diff_risk_list_180_vacc)
ratio_risk_lm180_t180_vacc <- do.call(rbind, ratio_risk_list_180_vacc)
multiply_columns_by_100(mean_risk_lm180_t180_vacc, cols_mean_diff)
multiply_columns_by_100(diff_risk_lm180_t180_vacc, c(cols_diff_extra, cols_mean_diff))

# Filter out treatment = 0 for mean risk
mean_risk_lm180_t180_vacc <- mean_risk_lm180_t180_vacc[mean_risk_lm180_t180_vacc$treatment != "0", ]
diff_risk_lm180_t180_vacc <- diff_risk_lm180_t180_vacc[!(diff_risk_lm180_t180_vacc$A == 0 | diff_risk_lm180_t180_vacc$B == 0), ]
diff_risk_lm180_t180_vacc$contrast <- paste0(diff_risk_lm180_t180_vacc$A, " vs ", diff_risk_lm180_t180_vacc$B)
ratio_risk_lm180_t180_vacc <- ratio_risk_lm180_t180_vacc[!(ratio_risk_lm180_t180_vacc$A == 0 | ratio_risk_lm180_t180_vacc$B == 0), ]
ratio_risk_lm180_t180_vacc$contrast <- paste0(ratio_risk_lm180_t180_vacc$A, " vs ", ratio_risk_lm180_t180_vacc$B)

# Plot Mean Risk
p1_lm180_t180_vacc <- ggplot(mean_risk_lm180_t180_vacc, aes(x = treatment, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "# Vaccinations") +
  ggtitle("Landmark 180d by vacc") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Plot Risk Difference
p2_lm180_t180_vacc <- ggplot(diff_risk_lm180_t180_vacc, aes(x = contrast, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "# Vaccinations") +
  theme_bw() +
  theme(legend.position = "bottom")

# Plot Risk Ratio
p3_lm180_t180_vacc <- ggplot(ratio_risk_lm180_t180_vacc, aes(x = contrast, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "") +
  theme_bw() +
  theme(legend.position = "bottom")

#730 days
mean_risk_list_730_vacc <- list()
diff_risk_list_730_vacc <- list()
ratio_risk_list_730_vacc <- list()

for (vacc in vacc_groups) {
  # Load the model RDS
  model <- readRDS(paste0("g_formula_model_SES_", vacc, "_landmark730_time_180.rds"))
  
  # Clean vacc label (e.g., "0vacc" → "0")
  vacc_label <- gsub("vacc", "", vacc)
  
  # Extract Mean Risk and add cleaned vacc column
  mean_risk <- model$meanRisk
  mean_risk$vacc <- vacc_label
  mean_risk_list_730_vacc[[vacc]] <- mean_risk
  
  # Extract Risk Difference and add cleaned vacc column
  diff_risk <- model$diffRisk
  diff_risk$vacc <- vacc_label
  diff_risk_list_730_vacc[[vacc]] <- diff_risk
  
  # Extract Risk Ratio and add cleaned vacc column
  ratio_risk <- model$ratioRisk
  ratio_risk$vacc <- vacc_label
  ratio_risk_list_730_vacc[[vacc]] <- ratio_risk
}

# Combine all vacc groups
mean_risk_lm730_t180_vacc <- do.call(rbind, mean_risk_list_730_vacc)
diff_risk_lm730_t180_vacc <- do.call(rbind, diff_risk_list_730_vacc)
ratio_risk_lm730_t180_vacc <- do.call(rbind, ratio_risk_list_730_vacc)
multiply_columns_by_100(mean_risk_lm730_t180_vacc, cols_mean_diff)
multiply_columns_by_100(diff_risk_lm730_t180_vacc, c(cols_diff_extra, cols_mean_diff))

# Filter out treatment = 0 for mean risk
mean_risk_lm730_t180_vacc <- mean_risk_lm730_t180_vacc[mean_risk_lm730_t180_vacc$treatment != "0", ]
diff_risk_lm730_t180_vacc <- diff_risk_lm730_t180_vacc[!(diff_risk_lm730_t180_vacc$A == 0 | diff_risk_lm730_t180_vacc$B == 0), ]
diff_risk_lm730_t180_vacc$contrast <- paste0(diff_risk_lm730_t180_vacc$A, " vs ", diff_risk_lm730_t180_vacc$B)
ratio_risk_lm730_t180_vacc <- ratio_risk_lm730_t180_vacc[!(ratio_risk_lm730_t180_vacc$A == 0 | ratio_risk_lm730_t180_vacc$B == 0), ]
ratio_risk_lm730_t180_vacc$contrast <- paste0(ratio_risk_lm730_t180_vacc$A, " vs ", ratio_risk_lm730_t180_vacc$B)

# Plot Mean Risk
p1_lm730_t180_vacc <- ggplot(mean_risk_lm730_t180_vacc, aes(x = treatment, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(y = "Absolute Risk, % (95% CI)", x = "Number of infections", color = "# Vaccinations") +
  ggtitle("Landmark 730d by vacc") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

# Plot Risk Difference
p2_lm730_t180_vacc <- ggplot(diff_risk_lm730_t180_vacc, aes(x = contrast, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(x = "Infection Count Comparison", y = "Risk Difference, % (95% CI)", color = "# Vaccinations") +
  theme_bw() +
  theme(legend.position = "bottom")

# Plot Risk Ratio
p3_lm730_t180_vacc <- ggplot(ratio_risk_lm730_t180_vacc, aes(x = contrast, y = estimate, color = vacc)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_color_manual(values = okabe_ito_colors) +
  labs(x = "Infection Count Comparison", y = "Risk Ratio (95% CI)", color = "# Vaccinations") +
  theme_bw() +
  theme(legend.position = "bottom")

#Final effect modification plot ---------

# Risk ratios -------
# Remove legends for grid arrangement
p3_lm180_t180_vacc_noleg <- p3_lm180_t180_vacc + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))
p3_lm365_t180_vacc_noleg <- p3_lm365_t180_vacc + theme(legend.position = "none")+ 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_blank())
p3_lm730_t180_vacc_noleg <- p3_lm730_t180_vacc + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_blank())
p3_lm180_t180_SES_noleg <- p3_lm180_t180_SES + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))
p3_lm365_t180_SES_noleg <- p3_lm365_t180_SES + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_blank())
p3_lm730_t180_SES_noleg <- p3_lm730_t180_SES + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_blank())
p3_lm180_t180_sex_noleg <- p3_lm180_t180_sex + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Landmark 180d")
p3_lm365_t180_sex_noleg <- p3_lm365_t180_sex + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_blank()) + 
  ggtitle("Landmark 1yr")
p3_lm730_t180_sex_noleg <- p3_lm730_t180_sex + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title.y = element_blank()) + 
  ggtitle("Landmark 2yrs")

# --- Extract common legends
#Sex
p3_lm730_t180_sex_legend <- p3_lm730_t180_sex + theme(legend.position = "right")
p3_lm730_t180_sex_legend_figure <- ggplotGrob(p3_lm730_t180_sex_legend)
legend_grobs_sex <- p3_lm730_t180_sex_legend_figure$grobs[which(sapply(p3_lm730_t180_sex_legend_figure$grobs, function(x) x$name) == "guide-box")]
legend_sex <- legend_grobs_sex[[1]]
#SES
p3_lm730_t180_SES_legend <- p3_lm730_t180_SES + theme(legend.position = "right")
p3_lm730_t180_SES_legend_figure <- ggplotGrob(p3_lm730_t180_SES_legend)
legend_grobs_SES <- p3_lm730_t180_SES_legend_figure$grobs[which(sapply(p3_lm730_t180_SES_legend_figure$grobs, function(x) x$name) == "guide-box")]
legend_SES <- legend_grobs_SES[[1]]
#Vacc
p3_lm730_t180_vacc_legend <- p3_lm730_t180_vacc + theme(legend.position = "right")
p3_lm730_t180_vacc_legend_figure <- ggplotGrob(p3_lm730_t180_vacc_legend)
legend_grobs_vacc <- p3_lm730_t180_vacc_legend_figure$grobs[which(sapply(p3_lm730_t180_vacc_legend_figure$grobs, function(x) x$name) == "guide-box")]
legend_vacc <- legend_grobs_vacc[[1]]
grid.newpage()
grid.draw(legend_vacc)

# === Create plot rows without legends ===
sex_row <- arrangeGrob(
  p3_lm180_t180_sex_noleg,
  p3_lm365_t180_sex_noleg,
  p3_lm730_t180_sex_noleg,
  ncol = 3
)

SES_row <- arrangeGrob(
  p3_lm180_t180_SES_noleg,
  p3_lm365_t180_SES_noleg,
  p3_lm730_t180_SES_noleg,
  ncol = 3
)

vacc_row <- arrangeGrob(
  p3_lm180_t180_vacc_noleg,
  p3_lm365_t180_vacc_noleg,
  p3_lm730_t180_vacc_noleg,
  ncol = 3
)

# === Create labels ===
label_A <- textGrob("A", x = unit(0, "npc"), y = unit(1, "npc"),
                    just = c("left", "top"),
                    gp = gpar(fontface = "bold", fontsize = 14))
label_B <- textGrob("B", x = unit(0, "npc"), y = unit(1, "npc"),
                    just = c("left", "top"),
                    gp = gpar(fontface = "bold", fontsize = 14))
label_C <- textGrob("C", x = unit(0, "npc"), y = unit(1, "npc"),
                    just = c("left", "top"),
                    gp = gpar(fontface = "bold", fontsize = 14))

# === Overlay labels on top-left of plot rows ===
sex_row_with_label <- arrangeGrob(
  grobs = list(label_A, sex_row),
  layout_matrix = rbind(c(1, 2)),
  widths = c(0.3, 9)
)

SES_row_with_label <- arrangeGrob(
  grobs = list(label_B, SES_row),
  layout_matrix = rbind(c(1, 2)),
  widths = c(0.3, 9)
)

vacc_row_with_label <- arrangeGrob(
  grobs = list(label_C, vacc_row),
  layout_matrix = rbind(c(1, 2)),
  widths = c(0.3, 9)
)

# === Combine plot rows and legends into 2-column layout ===
row_sex_with_legend_right <- arrangeGrob(
  sex_row_with_label, legend_sex,
  ncol = 2,
  widths = c(9, 1.5)
)

row_SES_with_legend_right <- arrangeGrob(
  SES_row_with_label, legend_SES,
  ncol = 2,
  widths = c(9, 1.5)
)

row_vacc_with_legend_right <- arrangeGrob(
  vacc_row_with_label, legend_vacc,
  ncol = 2,
  widths = c(9, 1.5)
)

# === Final arrangement ===
main_figure_effect_modification <- grid.arrange(
  row_sex_with_legend_right,
  row_SES_with_legend_right,
  row_vacc_with_legend_right,
  ncol = 1
)

grid.newpage()
grid.draw(main_figure_effect_modification)

pdf("main_figure_effect_modification.pdf", width = 10, height = 8)  # Adjust size as needed
grid.draw(main_figure_effect_modification)
dev.off()

# Effect of vaccination, long COVID absolute risks by vaccination group -----
# -----------------------------
# Absolute Risk by Vaccination (exclude 0, keep only vacc = 1,2,3)
# -----------------------------
calculate_abs_risk <- function(mean_risk_df) {
  df <- mean_risk_df
  # Keep only non-0 treatments and vacc = 1,2,3
  df <- df[df$treatment != "0" & df$vacc %in% c("1","2","3"), ]
  
  # Convert treatment to factor for clarity
  df$treatment <- factor(df$treatment, levels = sort(unique(df$treatment)),
                         labels = paste0(sort(unique(df$treatment)), " infection(s)"))
  
  # Keep only relevant columns
  df <- df[, c("treatment", "vacc", "estimate", "lower", "upper")]
  df <- df[order(df$vacc, df$treatment), ]
  return(df)
}

# Absolute risk at Landmark 180d
abs_risk_180 <- calculate_abs_risk(mean_risk_lm180_t180_vacc)
abs_risk_180$time <- "Landmark 180d"

# Absolute risk at Landmark 1yr
abs_risk_1yr <- calculate_abs_risk(mean_risk_lm365_t180_vacc)
abs_risk_1yr$time <- "Landmark 1yr"

# Combine both timepoints
abs_risk_all <- rbind(abs_risk_180, abs_risk_1yr)

# Ensure vacc is factor for plotting order
abs_risk_all$vacc <- factor(abs_risk_all$vacc, levels = c("1", "2", "3"))

# Plot Absolute Risk by vacc and # infections
p_abs_risk <- ggplot(abs_risk_all, aes(x = vacc, y = estimate, shape = treatment)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.3)) +
  facet_wrap(~time) +
  scale_shape_manual(
    values = c(16, 17),
    labels = c("1 Infection", "2 Infections")
  ) +
  labs(x = "Number of Vaccinations", y = "Absolute Risk, % (95% CI)", shape = "Number of Infections") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p_abs_risk)
pdf("absolute_risks_by_vaccination_group.pdf", width = 8, height = 4)  # Adjust size as needed
grid.draw(p_abs_risk)
dev.off()

# Effect of vaccination ONLY FOR DELTA AND BEYOND, long COVID absolute risks by vaccination group -----
g_formula_model_delta_omicron_vaccination_landmark180_time_180 <- readRDS("g_formula_model_delta_omicron_vaccination_landmark180_time_180.rds")
g_formula_model_delta_omicron_vaccination_landmark365_time_180 <- readRDS("g_formula_model_delta_omicron_vaccination_landmark365_time_180.rds")
# Define a helper function
multiply_columns_by_100 <- function(dt, cols) {
  dt[, (cols) := lapply(.SD, function(x) x * 100), .SDcols = cols]
}
mean_risk_delta_omicron_landmark180_t180 <- g_formula_model_delta_omicron_vaccination_landmark180_time_180$meanRisk
mean_risk_delta_omicron_landmark365_t180 <- g_formula_model_delta_omicron_vaccination_landmark365_time_180$meanRisk
cols_mean_diff <- c("estimate", "estimate.boot", "se", "lower", "upper")
multiply_columns_by_100(mean_risk_delta_omicron_landmark180_t180, cols_mean_diff)
multiply_columns_by_100(mean_risk_delta_omicron_landmark365_t180, cols_mean_diff)


# -----------------------------
# Absolute Risk by Vaccination (exclude 0, keep only vacc = 1,2,3), adjusting by number of infections
# -----------------------------

calculate_abs_risk_vacc <- function(mean_risk_df) {
  df <- mean_risk_df
  
  # Keep only treatment groups 1, 2, 3
  df <- df[df$treatment %in% c(1, 2, 3), ]
  
  # Convert treatment to factor with labels
  df$treatment <- factor(df$treatment,
                         levels = c(1, 2, 3),
                         labels = c("1 vaccination", "2 vaccinations", "3 vaccinations"))
  
  # Keep only relevant columns
  df <- df[, c("treatment", "estimate", "lower", "upper")]
  
  # Order by treatment
  df <- df[order(df$treatment), ]
  
  return(df)
}

# Absolute risk at Landmark 180d
abs_risk_180_vacc <- calculate_abs_risk_vacc(mean_risk_delta_omicron_landmark180_t180)
abs_risk_180_vacc$time <- "Landmark 180d"

# Absolute risk at Landmark 1yr
abs_risk_1yr_vacc <- calculate_abs_risk_vacc(mean_risk_delta_omicron_landmark365_t180)
abs_risk_1yr_vacc$time <- "Landmark 1yr"

# Combine both timepoints
abs_risk_all_vacc <- rbind(abs_risk_180_vacc, abs_risk_1yr_vacc)

# Ensure vacc is factor for plotting order
abs_risk_all_vacc$vacc <- factor(abs_risk_all_vacc$vacc, levels = c("1", "2", "3"))

# Plot Absolute Risk by vacc and # infections
p_abs_risk_vacc <- ggplot(abs_risk_all_vacc, aes(x = treatment, y = estimate)) +
  geom_point(size = 2.5, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, position = position_dodge(width = 0.3)) +
  facet_wrap(~time) +
  labs(x = "", y = "Absolute Risk, % (95% CI)") +
  theme_bw() +
  theme(legend.position = "bottom")

print(p_abs_risk_vacc)
pdf("absolute_risks_delta_omicron_by_vaccination_group.pdf", width = 8, height = 4)
grid.draw(p_abs_risk_vacc)
dev.off()
