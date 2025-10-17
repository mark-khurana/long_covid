# read packages
lapply(c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","prodlim","cmprsk","future.apply"), library, character.only = TRUE)

#Load data -------
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- 
  subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, infection_count_groups != "3+")
baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups <- droplevels(
  baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups
)

# VACCINATION-SPECIFIC ANALYSES --------------------
long_covid_baseline_0vacc <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, cum_vacc == "0")
long_covid_baseline_1vacc <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, cum_vacc == "1")
long_covid_baseline_2vacc <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, cum_vacc == "2")
long_covid_baseline_3vacc <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, cum_vacc == "3")

#survival analysis for each vaccination group
long_covid_survival_model_0vacc <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                      infection_count_groups + income_quantile + age_group + 
                                      REGIONSKODE + charlson.index.5yrs + sex, 
                                    data = long_covid_baseline_0vacc,
                                    cause=1)
long_covid_survival_model_0vacc
saveRDS(long_covid_survival_model_0vacc, file="long_covid_survival_model_0vacc.rds")

long_covid_survival_model_1vacc <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                      infection_count_groups + income_quantile + age_group + 
                                      REGIONSKODE + charlson.index.5yrs + sex, 
                                    data = long_covid_baseline_1vacc,
                                    cause=1)
long_covid_survival_model_1vacc
saveRDS(long_covid_survival_model_1vacc, file="long_covid_survival_model_1vacc.rds")

long_covid_survival_model_2vacc <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                      infection_count_groups + income_quantile + age_group + 
                                      REGIONSKODE + charlson.index.5yrs + sex, 
                                    data = long_covid_baseline_2vacc,
                                    cause=1)
long_covid_survival_model_2vacc
saveRDS(long_covid_survival_model_2vacc, file="long_covid_survival_model_2vacc.rds")

long_covid_survival_model_3vacc <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                      infection_count_groups + income_quantile + age_group + 
                                      REGIONSKODE + charlson.index.5yrs + sex, 
                                    data = long_covid_baseline_3vacc,
                                    cause=1)
long_covid_survival_model_3vacc
saveRDS(long_covid_survival_model_3vacc, file="long_covid_survival_model_3vacc.rds")

#Read back in data 
long_covid_survival_model_0vacc <- readRDS(file="long_covid_survival_model_0vacc.rds")
long_covid_survival_model_1vacc <- readRDS(file="long_covid_survival_model_1vacc.rds")
long_covid_survival_model_2vacc <- readRDS(file="long_covid_survival_model_2vacc.rds")
long_covid_survival_model_3vacc <- readRDS(file="long_covid_survival_model_3vacc.rds")

#Convert to data.table
long_covid_baseline_0vacc <- as.data.table(long_covid_baseline_0vacc)
long_covid_baseline_1vacc <- as.data.table(long_covid_baseline_1vacc)
long_covid_baseline_2vacc <- as.data.table(long_covid_baseline_2vacc)
long_covid_baseline_3vacc <- as.data.table(long_covid_baseline_3vacc)

# Landmark 365, Times 180
#0vacc
g_formula_model_SES_0vacc_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_0vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_0vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_0vacc_landmark365_time_180, file="g_formula_model_SES_0vacc_landmark365_time_180.rds")

#1vacc
g_formula_model_SES_1vacc_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_1vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_1vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_1vacc_landmark365_time_180, file="g_formula_model_SES_1vacc_landmark365_time_180.rds")

#2vacc
g_formula_model_SES_2vacc_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_2vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_2vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_2vacc_landmark365_time_180, file="g_formula_model_SES_2vacc_landmark365_time_180.rds")

#3vacc
g_formula_model_SES_3vacc_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_3vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_3vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_3vacc_landmark365_time_180, file="g_formula_model_SES_3vacc_landmark365_time_180.rds")

# Landmark 730, Times 180
#0vacc
g_formula_model_SES_0vacc_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_0vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_0vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_0vacc_landmark730_time_180, file="g_formula_model_SES_0vacc_landmark730_time_180.rds")

#1vacc
g_formula_model_SES_1vacc_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_1vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_1vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_1vacc_landmark730_time_180, file="g_formula_model_SES_1vacc_landmark730_time_180.rds")

#2vacc
g_formula_model_SES_2vacc_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_2vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_2vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_2vacc_landmark730_time_180, file="g_formula_model_SES_2vacc_landmark730_time_180.rds")

#3vacc
g_formula_model_SES_3vacc_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_3vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_3vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_3vacc_landmark730_time_180, file="g_formula_model_SES_3vacc_landmark730_time_180.rds")

# Landmark 180, Times 180
#0vacc
g_formula_model_SES_0vacc_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_0vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_0vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_0vacc_landmark180_time_180, file="g_formula_model_SES_0vacc_landmark180_time_180.rds")

#1vacc
g_formula_model_SES_1vacc_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_1vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_1vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_1vacc_landmark180_time_180, file="g_formula_model_SES_1vacc_landmark180_time_180.rds")

#2vacc
g_formula_model_SES_2vacc_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_2vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_2vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_2vacc_landmark180_time_180, file="g_formula_model_SES_2vacc_landmark180_time_180.rds")

#3vacc
g_formula_model_SES_3vacc_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_3vacc,
  treatment = "infection_count_groups",
  data = long_covid_baseline_3vacc,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 100, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_SES_3vacc_landmark180_time_180, file="g_formula_model_SES_3vacc_landmark180_time_180.rds")

