# read packages
lapply(c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","prodlim","cmprsk","future.apply"), library, character.only = TRUE)

#Load data -------
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- 
  subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, infection_count_groups != "3+")
baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups <- droplevels(
  baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups
)

# SEX-SPECIFIC ANALYSES --------------------
long_covid_baseline_male <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, sex == "M")
long_covid_baseline_female <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, sex == "F")

#survival analysis, without including sex
long_covid_survival_model_male <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                           infection_count_groups + cum_vacc + age_group + 
                           REGIONSKODE + charlson.index.5yrs + income_quantile, 
                         data = long_covid_baseline_male,
                         cause=1)
long_covid_survival_model_male
saveRDS(long_covid_survival_model_male, file="long_covid_survival_model_male.rds")

long_covid_survival_model_female <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                            infection_count_groups + cum_vacc + age_group + 
                            REGIONSKODE + charlson.index.5yrs + income_quantile, 
                          data = long_covid_baseline_female,
                          cause=1)
long_covid_survival_model_female
saveRDS(long_covid_survival_model_female, file="long_covid_survival_model_female.rds")

#Read back in data 
long_covid_survival_model_male <- readRDS(file="long_covid_survival_model_male.rds")
long_covid_survival_model_female <- readRDS(file="long_covid_survival_model_female.rds")

#Convert to data.table
long_covid_baseline_male <- as.data.table(long_covid_baseline_male)
long_covid_baseline_female <- as.data.table(long_covid_baseline_female)

# Landmark 365, Times 180 
#Male - Done
g_formula_model_male_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_male,
  treatment = "infection_count_groups",
  data = long_covid_baseline_male,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(365), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 50, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_male_landmark365_time_180, file="g_formula_model_male_landmark365_time_180.rds")

#Female
g_formula_model_female_landmark365_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_female,
  treatment = "infection_count_groups",
  data = long_covid_baseline_female,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,
  times = 180,
  landmark = c(365),
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 50,
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_female_landmark365_time_180, file="g_formula_model_female_landmark365_time_180.rds")

# Landmark 730, Times 180
#Male
g_formula_model_male_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_male,
  treatment = "infection_count_groups",
  data = long_covid_baseline_male,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(730), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 50, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_male_landmark730_time_180, file="g_formula_model_male_landmark730_time_180.rds")

#Female
g_formula_model_female_landmark730_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_female,
  treatment = "infection_count_groups",
  data = long_covid_baseline_female,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,
  times = 180,
  landmark = c(730),
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 50,
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_female_landmark730_time_180, file="g_formula_model_female_landmark730_time_180.rds")

# Landmark 180, Times 180
#Male
g_formula_model_male_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_male,
  treatment = "infection_count_groups",
  data = long_covid_baseline_male,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,  # Formula
  times = 180,
  landmark = c(180), 
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 50, 
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_male_landmark180_time_180, file="g_formula_model_male_landmark180_time_180.rds")

#Female
g_formula_model_female_landmark180_time_180 <- riskRegression::ate(
  event = long_covid_survival_model_female,
  treatment = "infection_count_groups",
  data = long_covid_baseline_female,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,
  times = 180,
  landmark = c(180),
  cause = 1,
  se = TRUE,
  band = FALSE,
  B = 50,
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_female_landmark180_time_180, file="g_formula_model_female_landmark180_time_180.rds")
