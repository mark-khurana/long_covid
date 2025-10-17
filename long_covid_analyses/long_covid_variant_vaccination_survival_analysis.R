# read packages
lapply(c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","prodlim","cmprsk","future.apply"), library, character.only = TRUE)

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

#Load data -------
baseline_tmerged_cohort_3_period_90_delay_0 <- readRDS(file="baseline_tmerged_cohort_3_period_90_delay_0.rds")

# Add Variant information ------------
variant_dates <- as.Date(c("2021-02-15", "2021-06-28", "2021-12-20"))
names(variant_dates) <- c("Alpha", "Delta", "Omicron")

# Function to assign starting variant based on first_test_date
get_start_variant <- function(first_date) {
  if (first_date < variant_dates["Alpha"]) "Wildtype"
  else if (first_date < variant_dates["Delta"]) "Alpha"
  else if (first_date < variant_dates["Omicron"]) "Delta"
  else "Omicron"
}

variant_long <- baseline_tmerged_cohort_3_period_90_delay_0 %>%
  distinct(PERSON_ID, first_test_date) %>%
  rowwise() %>%
  mutate(
    # starting variant at time 0
    time  = 0,
    variant = get_start_variant(first_test_date)
  ) %>%
  # add one row per change point that occurs after first_test_date
  bind_rows(
    baseline_tmerged_cohort_3_period_90_delay_0_filtered %>%
      distinct(PERSON_ID, first_test_date) %>%
      rowwise() %>%
      do({
        id <- .$PERSON_ID
        ft <- .$first_test_date
        tibble(
          PERSON_ID = id,
          time = pmax(0, as.numeric(variant_dates - ft)),
          variant = names(variant_dates)
        )
      }) %>% filter(time >= 0)
  ) %>%
  arrange(PERSON_ID, time)
saveRDS(variant_long, file="variant_long.rds")
#variant_long <- readRDS(file="variant_long.rds")

variant_long <- variant_long %>%
  group_by(PERSON_ID, time) %>%
  summarise(variant = last(variant), .groups = "drop")

tm_variant <- tmerge(
  data1 = baseline_tmerged_cohort_3_period_90_delay_0,
  data2 = variant_long,
  id = PERSON_ID,
  variant_period = tdc(time, variant)
)
#saveRDS(tm_variant, file="tm_variant.rds")
tm_variant <- readRDS(file="tm_variant.rds")
new_baseline_with_variants <- tm_variant


#Clean up baseline table:
new_baseline_with_variants$REGIONSKODE <- factor(new_baseline_with_variants$REGIONSKODE)
new_baseline_with_variants$cum_vacc <- factor(new_baseline_with_variants$cum_vacc)
new_baseline_with_variants$age_group <- factor(
  cut(new_baseline_with_variants$age_at_first_test, breaks, labels = age_groups, right = FALSE), 
  levels = age_groups
) |> relevel(ref = "18-30")
new_baseline_with_variants$age_group <- cut(
  new_baseline_with_variants$age_at_first_test, breaks, labels = age_groups, right = FALSE
)
new_baseline_with_variants <- new_baseline_with_variants |> 
  assign_sex() |> 
  assign_infection_groups()
#Change death to a competing risk
new_baseline_with_variants <- new_baseline_with_variants %>%
  mutate(event = ifelse(death_date == time_exit & time_exit == tstop & event == 0, 2, event))
# Censor at every time interval
new_baseline_with_variants <- new_baseline_with_variants %>%
  mutate(event = ifelse(is.na(event), 0, event))
#Removing rows without address information
new_baseline_with_variants <- new_baseline_with_variants[!is.na(new_baseline_with_variants$REGIONSKODE), ]
#Add income
new_baseline_with_variants <- new_baseline_with_variants |> 
  add_income_before_test(indkomst, quantiles = 4)
new_baseline_with_variants <- new_baseline_with_variants[!is.na(new_baseline_with_variants$income_quantile), ]
saveRDS(new_baseline_with_variants, file="new_baseline_with_variants.rds")

new_baseline_with_variants_filtered <- 
  subset(new_baseline_with_variants, infection_count_groups != "3+")
new_baseline_with_variants_filtered$infection_count_groups <- droplevels(
  new_baseline_with_variants_filtered$infection_count_groups
)
saveRDS(new_baseline_with_variants_filtered, file="new_baseline_with_variants_filtered.rds")

# VARIANT-SPECIFIC ANALYSES --------------------
new_baseline_with_variants_filtered <- readRDS(file="new_baseline_with_variants_filtered.rds")

#Variant analyses:
new_baseline_with_variants_filtered <- readRDS(file="new_baseline_with_variants_filtered.rds")
new_baseline_with_variants_filtered <- as.data.table(new_baseline_with_variants_filtered)

long_covid_baseline_wildtype_alpha <- subset(new_baseline_with_variants_filtered, variant_period %in% c("Alpha", "Wildtype"))
long_covid_baseline_wildtype_alpha[, infection_count_groups := relevel(infection_count_groups, ref = "1")]

long_covid_baseline_delta_omicron <- subset(new_baseline_with_variants_filtered, variant_period %in% c("Delta", "Omicron"))
long_covid_baseline_delta_omicron[, infection_count_groups := relevel(infection_count_groups, ref = "1")]

# wildtype_alpha --------
long_covid_survival_model_wildtype_alpha <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                                infection_count_groups + cum_vacc + age_group + 
                                                REGIONSKODE + charlson.index.5yrs + income_quantile, 
                                              data = long_covid_baseline_wildtype_alpha,
                                              cause=1)
#saveRDS(long_covid_survival_model_wildtype_alpha, file="long_covid_survival_model_wildtype_alpha.rds")
long_covid_survival_model_wildtype_alpha <- readRDS(file="long_covid_survival_model_wildtype_alpha.rds")

# Delta, Omicron --------
long_covid_survival_model_delta_omicron <- CSC(Hist(entry = tstart, time = tstop, event = event) ~ 
                                            infection_count_groups + cum_vacc + age_group + 
                                            REGIONSKODE + charlson.index.5yrs + income_quantile, 
                                          data = long_covid_baseline_delta_omicron,
                                          cause=1)
saveRDS(long_covid_survival_model_delta_omicron, file="long_covid_survival_model_delta_omicron.rds")
long_covid_survival_model_delta_omicron <- readRDS(file="long_covid_survival_model_delta_omicron.rds")
