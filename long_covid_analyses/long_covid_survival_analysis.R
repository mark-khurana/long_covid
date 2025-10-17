# read packages
lapply(c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","prodlim","cmprsk","future.apply"), library, character.only = TRUE)

# Getting socioeconomic status data ---------------

# Loading in metadata ----------------------------------------------
drv <- dbDriver('Oracle')
conn <- DSTora::OraGenvej('', dbuser = '')
con2 <- DSTora::OraGenvej('', dbuser = '')
con3 <- DSTora::OraGenvej('', dbuser = '')
con4 <- DSTora::OraGenvej('', dbuser = '')

indkomst <- dbGetQuery(con3, "
  SELECT PERSON_ID, REFERENCETID, AEKVIVADISP_13
  FROM X
  WHERE REFERENCETID > TO_DATE('2014-12-31', 'YYYY-MM-DD')
")

#Cohort 1: composite_cum_vacc
#Cohort 2: composite_vacc_group
#Cohort 3: long_covid_cum_vacc
#Cohort 4: long_covid_vacc_group

list.files(pattern = "baseline_tmerged_cohort_")
age_groups <- c("18-30", "30-45", "45-60", "60-75", "75+")
breaks <- c(18, 30, 45, 60, 75, Inf)
# Define helper functions
assign_sex <- function(df) {
  df$sex <- factor(ifelse(df$KOEN == 1, "M", ifelse(df$KOEN == 2, "F", NA)), levels = c("M", "F"))
  df
}

assign_infection_groups <- function(df) {
  df <- df %>%
    mutate(
      # Convert inf_cumulative to numeric to ensure proper comparison
      inf_cumulative = as.numeric(inf_cumulative),
      
      # Categorize infections manually
      infection_count_groups = case_when(
        inf_cumulative == 0 ~ "0",              # For 0 infections
        inf_cumulative == 1 ~ "1",              # For 1 infection
        inf_cumulative == 2 ~ "2",              # For 2 infections
        inf_cumulative >= 3 ~ "3+",             # For 3 or more infections
        TRUE ~ NA_character_                   # If there's any missing or NA value
      ),
      
      # Optionally, relevel to ensure 1 is the reference category
      infection_count_groups = factor(infection_count_groups, levels = c("0", "1", "2", "3+"))
    )
  
  return(df)
}

add_income_before_test <- function(main_df, income_df, quantiles = 4) {
  library(data.table)
  
  # Convert to data.tables
  main_dt <- as.data.table(main_df)
  income_dt <- as.data.table(income_df)
  
  # Ensure dates are in Date format
  income_dt[, REFERENCETID := as.Date(REFERENCETID)]
  main_dt[, first_test_date := as.Date(first_test_date)]
  
  # Filter income: keep only values > 0
  income_dt <- income_dt[AEKVIVADISP_13 > 0]
  
  # Join income with test dates to filter per-person
  income_merged <- merge(
    income_dt,
    main_dt[, .(PERSON_ID, first_test_date)],
    by = "PERSON_ID",
    allow.cartesian = TRUE
  )
  
  # Keep only rows within 3 years before first_test_date
  income_merged <- income_merged[
    REFERENCETID < first_test_date &
      REFERENCETID >= (first_test_date - 365.25 * 3)
  ]
  
  # For each person and test date, get the 3 most recent entries
  income_merged <- income_merged[order(PERSON_ID, first_test_date, -REFERENCETID)]
  income_top3 <- income_merged[, head(.SD, 3), by = .(PERSON_ID, first_test_date)]
  
  # Calculate mean income per person
  person_mean_income <- income_top3[, .(mean_income = mean(AEKVIVADISP_13)), by = PERSON_ID]
  
  # Compute quantiles based on unique individuals
  quantile_bins <- quantile(
    person_mean_income$mean_income,
    probs = seq(0, 1, length.out = quantiles + 1),
    na.rm = TRUE
  )
  
  # Assign quantile labels
  person_mean_income[, income_quantile := cut(
    mean_income,
    breaks = quantile_bins,
    labels = paste0("Q", 1:quantiles),
    include.lowest = TRUE
  )]
  
  # Merge back to main data
  result <- merge(
    main_dt,
    person_mean_income,
    by = "PERSON_ID",
    all.x = FALSE  # Drop those without sufficient income data
  )
  
  return(as.data.frame(result))
}

# Running survival analyses ------------
# Long COVID, cumulative vaccinations (cohort 3), 0  lag, 90-day infection period -------
#Setup
baseline_tmerged_cohort_3_period_90_delay_0 <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0.rds")
baseline_tmerged_cohort_3_period_90_delay_0$REGIONSKODE <- factor(baseline_tmerged_cohort_3_period_90_delay_0$REGIONSKODE)
baseline_tmerged_cohort_3_period_90_delay_0$cum_vacc <- factor(baseline_tmerged_cohort_3_period_90_delay_0$cum_vacc)
baseline_tmerged_cohort_3_period_90_delay_0$age_group <- factor(
  cut(baseline_tmerged_cohort_3_period_90_delay_0$age_at_first_test, breaks, labels = age_groups, right = FALSE), 
  levels = age_groups
) |> relevel(ref = "18-30")
baseline_tmerged_cohort_3_period_90_delay_0$age_group <- cut(
  baseline_tmerged_cohort_3_period_90_delay_0$age_at_first_test, breaks, labels = age_groups, right = FALSE
)
baseline_tmerged_cohort_3_period_90_delay_0 <- baseline_tmerged_cohort_3_period_90_delay_0 |> 
  assign_sex() |> 
  assign_infection_groups()
#Change death to a competing risk
baseline_tmerged_cohort_3_period_90_delay_0 <- baseline_tmerged_cohort_3_period_90_delay_0 %>%
  mutate(event = ifelse(death_date == time_exit & time_exit == tstop & event == 0, 2, event))
# Censor at every time interval
baseline_tmerged_cohort_3_period_90_delay_0 <- baseline_tmerged_cohort_3_period_90_delay_0 %>%
  mutate(event = ifelse(is.na(event), 0, event))
#Removing rows without address information
baseline_tmerged_cohort_3_period_90_delay_0 <- baseline_tmerged_cohort_3_period_90_delay_0[!is.na(baseline_tmerged_cohort_3_period_90_delay_0$REGIONSKODE), ]
#Add income
baseline_tmerged_cohort_3_period_90_delay_0 <- baseline_tmerged_cohort_3_period_90_delay_0 |> 
  add_income_before_test(indkomst, quantiles = 4)
baseline_tmerged_cohort_3_period_90_delay_0 <- baseline_tmerged_cohort_3_period_90_delay_0[!is.na(baseline_tmerged_cohort_3_period_90_delay_0$income_quantile), ]

baseline_tmerged_cohort_3_period_90_delay_0_filtered <- baseline_tmerged_cohort_3_period_90_delay_0
saveRDS(baseline_tmerged_cohort_3_period_90_delay_0_filtered, file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")


#Version 1-------
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")

model_LC_cum_0_90 <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                           infection_count_groups + cum_vacc + age_group + sex + 
                           REGIONSKODE + charlson.index.5yrs + income_quantile, 
                         data = baseline_tmerged_cohort_3_period_90_delay_0_filtered,
                         cause=1)
model_LC_cum_0_90
#saveRDS(model_LC_cum_0_90, file="model_LC_cum_0_90.rds")
model_LC_cum_0_90 <- readRDS(file="model_LC_cum_0_90.rds")

#Convert to data.table
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- as.data.table(baseline_tmerged_cohort_3_period_90_delay_0_filtered)

# Landmark 365, Times 180
filtered_data <- subset(baseline_tmerged_cohort_3_period_90_delay_0_filtered, infection_count_groups == "3+")
lowest_times <- filtered_data[order(filtered_data$tstart), ][1:100, ]

g_formula_model_LC_cum_0_90_landmark365_time_180 <- riskRegression::ate(
  event = model_LC_cum_0_90,
  treatment = "infection_count_groups",
  data = baseline_tmerged_cohort_3_period_90_delay_0_filtered,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1,
  times = 180,
  landmark = c(365),
  cause = 1,  
  se = TRUE,  
  band = FALSE, 
  B = 50,
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_LC_cum_0_90_landmark365_time_180, file="g_formula_model_LC_cum_0_90_landmark365_time_180.rds")
summary(g_formula_model_LC_cum_0_90_landmark365_time_180, type = "meanRisk")
autoplot(g_formula_model_LC_cum_0_90_landmark365_time_180) + 
  ggtitle("ATE Estimates for Infection Status Over Time") +
  theme_minimal()

# Landmark 730, Times 180 - DONE - 50 BOOTSTRAPS
g_formula_model_LC_cum_0_90_landmark720_time_180 <- riskRegression::ate(
  event = model_LC_cum_0_90,
  treatment = "infection_count_groups",
  data = baseline_tmerged_cohort_3_period_90_delay_0_filtered,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1, 
  times = 180,
  landmark = c(720),
  cause = 1,  
  se = TRUE,  
  band = FALSE, 
  B = 50,
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_LC_cum_0_90_landmark720_time_180, file="g_formula_model_LC_cum_0_90_landmark720_time_180.rds")
summary(g_formula_model_LC_cum_0_90_landmark720_time_180, type = "meanRisk")
autoplot(g_formula_model_LC_cum_0_90_landmark720_time_180) + 
  ggtitle("ATE Estimates for Infection Status Over Time") +
  theme_minimal()






# Re-running model with 0-2 infections -----------
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0_filtered.rds")
baseline_tmerged_cohort_3_period_90_delay_0_filtered <- as.data.table(baseline_tmerged_cohort_3_period_90_delay_0_filtered)
baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less <- baseline_tmerged_cohort_3_period_90_delay_0_filtered[baseline_tmerged_cohort_3_period_90_delay_0_filtered$infection_count_groups != "3+", ]
baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less$infection_count_groups <- 
  droplevels(baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less$infection_count_groups)
rm(baseline_tmerged_cohort_3_period_90_delay_0_filtered)


model_LC_cum_0_90_2_or_less_infections <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                                infection_count_groups + cum_vacc + age_group + sex + 
                                                REGIONSKODE + charlson.index.5yrs + income_quantile, 
                                              data = baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less,
                                              cause=1)
model_LC_cum_0_90_2_or_less_infections
#saveRDS(model_LC_cum_0_90_2_or_less_infections, file="model_LC_cum_0_90_2_or_less_infections.rds")
model_LC_cum_0_90_2_or_less_infections <- readRDS(file="model_LC_cum_0_90_2_or_less_infections.rds")


# Landmark 180, Times 180
g_formula_model_LC_cum_0_90_landmark180_time_180 <- riskRegression::ate(
  event = model_LC_cum_0_90_2_or_less_infections,
  treatment = "infection_count_groups",
  data = baseline_tmerged_cohort_3_period_90_delay_0_filtered_2_or_less,  
  formula = Hist(entry = tstart, time = tstop, event = event) ~ 1, 
  times = 180,
  landmark = c(180),
  cause = 1,  
  se = TRUE,  
  band = FALSE, 
  B = 50,
  mc.cores=1,
  verbose = TRUE)
saveRDS(g_formula_model_LC_cum_0_90_landmark180_time_180, file="g_formula_model_LC_cum_0_90_landmark180_time_180.rds")
summary(g_formula_model_LC_cum_0_90_landmark180_time_180, type = "meanRisk")
autoplot(g_formula_model_LC_cum_0_90_landmark180_time_180) + 
  ggtitle("ATE Estimates for Infection Status Over Time") +
  theme_minimal()





























