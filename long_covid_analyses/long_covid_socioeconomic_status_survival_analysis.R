# read packages
lapply(c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","prodlim","cmprsk","future.apply"), library, character.only = TRUE)

#Load data -------
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- 
  subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, infection_count_groups != "3+")
baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups <- droplevels(
  baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups
)

# SOCIOECONOMIC-SPECIFIC ANALYSES --------------------
long_covid_baseline_Q1 <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, income_quantile == "Q1")
long_covid_baseline_Q2 <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, income_quantile == "Q2")
long_covid_baseline_Q3 <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, income_quantile == "Q3")
long_covid_baseline_Q4 <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, income_quantile == "Q4")

#survival analysis for each quartile
long_covid_survival_model_Q1 <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                        infection_count_groups + cum_vacc + age_group + 
                                        REGIONSKODE + charlson.index.5yrs + sex, 
                                      data = long_covid_baseline_Q1,
                                      cause=1)
saveRDS(long_covid_survival_model_Q1, file="long_covid_survival_model_Q1.rds")

long_covid_survival_model_Q2 <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                      infection_count_groups + cum_vacc + age_group + 
                                      REGIONSKODE + charlson.index.5yrs + sex, 
                                    data = long_covid_baseline_Q2,
                                    cause=1)
saveRDS(long_covid_survival_model_Q2, file="long_covid_survival_model_Q2.rds")

long_covid_survival_model_Q3 <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                      infection_count_groups + cum_vacc + age_group + 
                                      REGIONSKODE + charlson.index.5yrs + sex, 
                                    data = long_covid_baseline_Q3,
                                    cause=1)
saveRDS(long_covid_survival_model_Q3, file="long_covid_survival_model_Q3.rds")

long_covid_survival_model_Q4 <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                      infection_count_groups + cum_vacc + age_group + 
                                      REGIONSKODE + charlson.index.5yrs + sex, 
                                    data = long_covid_baseline_Q4,
                                    cause=1)
saveRDS(long_covid_survival_model_Q4, file="long_covid_survival_model_Q4.rds")

#Read back in data 
long_covid_survival_model_Q1 <- readRDS(file="long_covid_survival_model_Q1.rds")
long_covid_survival_model_Q2 <- readRDS(file="long_covid_survival_model_Q2.rds")
long_covid_survival_model_Q3 <- readRDS(file="long_covid_survival_model_Q3.rds")
long_covid_survival_model_Q4 <- readRDS(file="long_covid_survival_model_Q4.rds")

#Convert to data.table
long_covid_baseline_Q1 <- as.data.table(long_covid_baseline_Q1)
long_covid_baseline_Q2 <- as.data.table(long_covid_baseline_Q2)
long_covid_baseline_Q3 <- as.data.table(long_covid_baseline_Q3)
long_covid_baseline_Q4 <- as.data.table(long_covid_baseline_Q4)

# Landmark 365, Times 180
#Q1
g_formula_model_SES_Q1_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q1,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q1,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q1_landmark365_time_180, file="g_formula_model_SES_Q1_landmark365_time_180.rds")

#Q2
g_formula_model_SES_Q2_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q2,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q2,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q2_landmark365_time_180, file="g_formula_model_SES_Q2_landmark365_time_180.rds")

#Q3
g_formula_model_SES_Q3_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q3,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q3,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q3_landmark365_time_180, file="g_formula_model_SES_Q3_landmark365_time_180.rds")

#Q4
g_formula_model_SES_Q4_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q4,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q4,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q4_landmark365_time_180, file="g_formula_model_SES_Q4_landmark365_time_180.rds")

# Landmark 730, Times 180 - NOT DONE, BUT WITH 100 BOOTSTRAPS
#Q1
g_formula_model_SES_Q1_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q1,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q1,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q1_landmark730_time_180, file="g_formula_model_SES_Q1_landmark730_time_180.rds")

#Q2
g_formula_model_SES_Q2_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q2,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q2,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q2_landmark730_time_180, file="g_formula_model_SES_Q2_landmark730_time_180.rds")

#Q3
g_formula_model_SES_Q3_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q3,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q3,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q3_landmark730_time_180, file="g_formula_model_SES_Q3_landmark730_time_180.rds")

#Q4
g_formula_model_SES_Q4_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q4,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q4,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q4_landmark730_time_180, file="g_formula_model_SES_Q4_landmark730_time_180.rds")

# Landmark 180, Times 180
#Q1
g_formula_model_SES_Q1_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q1,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q1,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q1_landmark180_time_180, file="g_formula_model_SES_Q1_landmark180_time_180.rds")

#Q2
g_formula_model_SES_Q2_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q2,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q2,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q2_landmark180_time_180, file="g_formula_model_SES_Q2_landmark180_time_180.rds")

#Q3
g_formula_model_SES_Q3_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q3,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q3,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q3_landmark180_time_180, file="g_formula_model_SES_Q3_landmark180_time_180.rds")

#Q4
g_formula_model_SES_Q4_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_Q4,
  treatment = "infection_count_groups",
  data = long_covid_baseline_Q4,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_Q4_landmark180_time_180, file="g_formula_model_SES_Q4_landmark180_time_180.rds")

