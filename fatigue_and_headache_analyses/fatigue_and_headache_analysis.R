# read packages
libs <- c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","heaven"); lapply(libs, library, character.only = TRUE)

# Loading in metadata ----------------------------------------------
drv <- dbDriver('Oracle')
conn <- DSTora::OraGenvej('', dbuser = '')
con2 <- DSTora::OraGenvej('', dbuser = '')
con3 <- DSTora::OraGenvej('', dbuser = '')

dbListTables(conn, schema = "")
dbListTables(con3, schema = "")

COVID_VACC <- dbReadTable(conn = conn,
                          name = "",
                          schema = '')

lifelines <- dbReadTable(conn = conn,
                         name = "",
                         schema = '')

fatigue_baseline_cohort_with_cci <- readRDS("fatigue_baseline_cohort_with_cci.rds")
headache_baseline_cohort_with_cci <- readRDS("headache_baseline_cohort_with_cci.rds")

#Diagnostic code, fatigue
fatigue_codes <- data.frame(icd_10 = c("R53", "R539", "R539A", "R539C","R539E","R539F","G933")
)
fatigue_codes$diag_code <- gsub("\\.", "", paste0("D", fatigue_codes$icd_10))

fatigue_diag_codes_list <- paste0("'", fatigue_codes$diag_code, "'", collapse = ", ")
fatigue_query_LPR3 <- paste0("SELECT * FROM X ",
                                "WHERE AKTIONSDIAGNOSE IN (", fatigue_diag_codes_list, ") ")
fatigue_diagnoses_LPR3 <- dbGetQuery(con3, fatigue_query_LPR3)

#Diagnostic codes, headache
headache_outcome_codes <- data.frame(icd_10 = c("R51", "R511", "R512",
                                        "R513","R514","R515",
                                        "R516","R517","R518",
                                        "R519")
)
headache_outcome_codes$diag_code <- gsub("\\.", "", paste0("D", headache_outcome_codes$icd_10))

headache_outcome_codes_list <- paste0("'", headache_outcome_codes$diag_code, "'", collapse = ", ")
headache_outcome_query_LPR3 <- paste0("SELECT * FROM X ",
                                       "WHERE AKTIONSDIAGNOSE IN (", headache_outcome_codes_list, ") ")
headache_outcome_diagnoses_LPR3 <- dbGetQuery(con3, headache_outcome_query_LPR3)

#Geographic info
general_address <- dbReadTable(conn = con2,
                               name = "",
                               schema = '')

query <- "SELECT * FROM X WHERE BOP_VTIL > TO_DATE('2020-03-30', 'YYYY-MM-DD')"
geo_address <- dbGetQuery(conn = con2, statement = query)

# Adding region at first test date ------------
# Step 1: Filter geo_address to only relevant PERSON_IDs
relevant_geo_fatigue <- geo_address %>%
  filter(PERSON_ID %in% fatigue_baseline_cohort_with_cci$PERSON_ID)
relevant_geo_headache <- geo_address %>%
  filter(PERSON_ID %in% headache_baseline_cohort_with_cci$PERSON_ID)

# Step 2: Match first_test_date with the right BOP_VFRA and BOP_VTIL range
matched_geo_fatigue <- relevant_geo_fatigue %>%
  inner_join(fatigue_baseline_cohort_with_cci, by = "PERSON_ID") %>%
  filter(first_test_date > BOP_VFRA & first_test_date < BOP_VTIL) %>%
  arrange(PERSON_ID, first_test_date, BOP_VFRA) %>%  # Ensure earliest match
  group_by(PERSON_ID, first_test_date) %>%
  slice(1) %>%  # Take the first matching row per PERSON_ID & first_test_date
  ungroup()
matched_geo_headache <- relevant_geo_headache %>%
  inner_join(headache_baseline_cohort_with_cci, by = "PERSON_ID") %>%
  filter(first_test_date > BOP_VFRA & first_test_date < BOP_VTIL) %>%
  arrange(PERSON_ID, first_test_date, BOP_VFRA) %>%  # Ensure earliest match
  group_by(PERSON_ID, first_test_date) %>%
  slice(1) %>%  # Take the first matching row per PERSON_ID & first_test_date
  ungroup()

# Step 3: Retrieve REGIONSKODE using the matched ID
region_data_fatigue <- matched_geo_fatigue %>%
  select(PERSON_ID, first_test_date, ID) %>%
  left_join(select(general_address, ID, REGIONSKODE), by = "ID") %>%
  select(PERSON_ID, first_test_date, REGIONSKODE)
region_data_headache <- matched_geo_headache %>%
  select(PERSON_ID, first_test_date, ID) %>%
  left_join(select(general_address, ID, REGIONSKODE), by = "ID") %>%
  select(PERSON_ID, first_test_date, REGIONSKODE)

# Step 4: Merge back into the original dataframe
fatigue_baseline_cohort_with_cci_region <- fatigue_baseline_cohort_with_cci %>%
  left_join(region_data_fatigue, by = c("PERSON_ID", "first_test_date"))
headache_baseline_cohort_with_cci_region <- headache_baseline_cohort_with_cci %>%
  left_join(region_data_headache, by = c("PERSON_ID", "first_test_date"))

#Include only individuals that are 18>= in age
fatigue_baseline_cohort_with_cci <- fatigue_baseline_cohort_with_cci %>%
  filter(age_at_first_test >= 18)
saveRDS(fatigue_baseline_cohort_with_cci_region, file="fatigue_baseline_cohort_with_cci_region.rds")
headache_baseline_cohort_with_cci <- headache_baseline_cohort_with_cci %>%
  filter(age_at_first_test >= 18)
saveRDS(headache_baseline_cohort_with_cci_region, file="headache_baseline_cohort_with_cci_region.rds")

# Adding other outcomes ---------------------------------------
fatigue_baseline_cohort_with_cci_region <- readRDS(file="fatigue_baseline_cohort_with_cci_region.rds")
headache_baseline_cohort_with_cci_region <- readRDS(file="headache_baseline_cohort_with_cci_region.rds")

# Step 1: Load baseline data correctly
fatigue_baseline_cohort_with_cci_region <- fatigue_baseline_cohort_with_cci_region %>%
  filter(first_test_date < as.Date("2023-01-01"))
headache_baseline_cohort_with_cci_region <- headache_baseline_cohort_with_cci_region %>%
  filter(first_test_date < as.Date("2023-01-01"))
last_follow_up_day <- as.Date("2022-12-31")
fatigue_baseline_cohort_with_cci_region <- fatigue_baseline_cohort_with_cci_region %>%
  mutate(
    first_test_day = 0,
    last_possible_day = as.numeric(last_follow_up_day - first_test_date)
  )
headache_baseline_cohort_with_cci_region <- headache_baseline_cohort_with_cci_region %>%
  mutate(
    first_test_day = 0,
    last_possible_day = as.numeric(last_follow_up_day - first_test_date)
  )

# Step 2: Add death information
death_dates_fatigue <- lifelines %>%
  inner_join(fatigue_baseline_cohort_with_cci_region %>% select(PERSON_ID, first_test_date), by = "PERSON_ID") %>%
  mutate(
    EVENT_FINAL_DATE = as.Date(EVENT_FINAL_DATE), 
    first_test_date = as.Date(first_test_date)
  ) %>%
  filter(EVENT_CAUSE_FINAL == "Doed") %>%  
  filter(EVENT_FINAL_DATE > first_test_date) %>%  
  group_by(PERSON_ID) %>%
  summarise(
    date_of_death = min(EVENT_FINAL_DATE, na.rm = TRUE),
    .groups = "drop"
  )
fatigue_baseline_cohort_cci_fatigue_death <- fatigue_baseline_cohort_with_cci_region %>%
  left_join(death_dates_fatigue, by = "PERSON_ID") %>%
  mutate(
    death_date = as.numeric(date_of_death - first_test_date)
  )

death_dates_headache <- lifelines %>%
  inner_join(headache_baseline_cohort_with_cci_region %>% select(PERSON_ID, first_test_date), by = "PERSON_ID") %>%
  mutate(
    EVENT_FINAL_DATE = as.Date(EVENT_FINAL_DATE), 
    first_test_date = as.Date(first_test_date)
  ) %>%
  filter(EVENT_CAUSE_FINAL == "Doed") %>%  
  filter(EVENT_FINAL_DATE > first_test_date) %>%  
  group_by(PERSON_ID) %>%
  summarise(
    date_of_death = min(EVENT_FINAL_DATE, na.rm = TRUE),
    .groups = "drop"
  )
headache_baseline_cohort_cci_headache_death <- headache_baseline_cohort_with_cci_region %>%
  left_join(death_dates_headache, by = "PERSON_ID") %>%
  mutate(
    death_date = as.numeric(date_of_death - first_test_date)
  )

# Step 3: Add emigration data
emigration_dates_fatigue <- lifelines %>%
  inner_join(fatigue_baseline_cohort_cci_fatigue_death %>% select(PERSON_ID, first_test_date), by = "PERSON_ID") %>%  # Join first_test_date
  mutate(
    EVENT_FINAL_DATE = as.Date(EVENT_FINAL_DATE),
    first_test_date = as.Date(first_test_date)  
  ) %>%
  filter(EVENT_CAUSE_FINAL == "Udvandret") %>%
  filter(EVENT_FINAL_DATE > first_test_date) %>%
  group_by(PERSON_ID) %>%
  summarise(
    date_of_emigration = min(EVENT_FINAL_DATE, na.rm = TRUE), 
    .groups = "drop"
  )
fatigue_baseline_cohort_cci_fatigue_death_emigration <- fatigue_baseline_cohort_cci_fatigue_death %>%
  left_join(emigration_dates_fatigue, by = "PERSON_ID") %>%
  mutate(
    emigration_date = as.numeric(date_of_emigration - first_test_date)
  )

emigration_dates_headache <- lifelines %>%
  inner_join(headache_baseline_cohort_cci_headache_death %>% select(PERSON_ID, first_test_date), by = "PERSON_ID") %>%  # Join first_test_date
  mutate(
    EVENT_FINAL_DATE = as.Date(EVENT_FINAL_DATE),
    first_test_date = as.Date(first_test_date)  
  ) %>%
  filter(EVENT_CAUSE_FINAL == "Udvandret") %>%
  filter(EVENT_FINAL_DATE > first_test_date) %>%
  group_by(PERSON_ID) %>%
  summarise(
    date_of_emigration = min(EVENT_FINAL_DATE, na.rm = TRUE), 
    .groups = "drop"
  )
headache_baseline_cohort_cci_headache_death_emigration <- headache_baseline_cohort_cci_fatigue_death %>%
  left_join(emigration_dates_headache, by = "PERSON_ID") %>%
  mutate(
    emigration_date = as.numeric(date_of_emigration - first_test_date)
  )


#fatigue
first_fatigue_diagnosis <- fatigue_diagnoses_LPR3 %>%
  inner_join(fatigue_baseline_cohort_cci_fatigue_death_emigration %>% select(PERSON_ID, first_test_date), by = "PERSON_ID") %>% 
  mutate(
    DATO_START = as.Date(DATO_START),
    first_test_date = as.Date(first_test_date)
  ) %>%
  filter(DATO_START > first_test_date) %>%
  group_by(PERSON_ID) %>%
  summarise(
    first_fatigue_diagnosis = min(DATO_START, na.rm = TRUE), 
    corresponding_AKTIONSDIAGNOSE = AKTIONSDIAGNOSE[which.min(DATO_START)],
    .groups = "drop"
  ) 
fatigue_baseline_cohort_fatigue <- fatigue_baseline_cohort_cci_fatigue_death_emigration %>%
  left_join(first_fatigue_diagnosis, by = "PERSON_ID") %>%
  mutate(
    diagnosis_days = as.numeric(first_fatigue_diagnosis - first_test_date)
  )

#headache
first_headache_diagnosis <- headache_outcome_diagnoses_LPR3 %>%
  inner_join(headache_baseline_cohort_cci_headache_death_emigration %>% select(PERSON_ID, first_test_date), by = "PERSON_ID") %>% 
  mutate(
    DATO_START = as.Date(DATO_START),
    first_test_date = as.Date(first_test_date)
  ) %>%
  filter(DATO_START > first_test_date) %>%
  group_by(PERSON_ID) %>%
  summarise(
    first_headache_diagnosis = min(DATO_START, na.rm = TRUE), 
    corresponding_AKTIONSDIAGNOSE = AKTIONSDIAGNOSE[which.min(DATO_START)],
    .groups = "drop"
  ) 
headache_baseline_cohort_headache_outcome <- headache_baseline_cohort_cci_headache_death_emigration %>%
  left_join(first_headache_diagnosis, by = "PERSON_ID") %>%
  mutate(
    diagnosis_days = as.numeric(first_headache_diagnosis - first_test_date)
  )

#Save
saveRDS(fatigue_baseline_cohort_fatigue, file="fatigue_baseline_cohort_fatigue.rds")
saveRDS(headache_baseline_cohort_headache_outcome, file="headache_baseline_cohort_headache_outcome.rds")

#fatigue, adding event
fatigue_cohort_with_outcomes <- fatigue_baseline_cohort_fatigue %>%
  mutate(
    time_exit = pmin(
      last_possible_day,    
      diagnosis_days,  
      death_date,           
      emigration_date,      
      na.rm = TRUE          
    ),
    time_entry = 0,
    event = ifelse(is.na(diagnosis_days) | time_exit != diagnosis_days, 0, 1)  # Event occurs if time_exit is due to fatigue diagnosis
  )
baseline_fatigue <- tmerge(
  data1 = fatigue_cohort_with_outcomes,
  data2 = fatigue_cohort_with_outcomes,
  id = PERSON_ID,
  tstart = time_entry,  # First test day is day 0
  tstop = time_exit,
  event = event(time_exit, event)
)

#headache endpoint, adding event
headache_cohort_with_outcomes <- headache_baseline_cohort_headache_outcome %>%
  mutate(
    time_exit = pmin(
      last_possible_day,    
      diagnosis_days,  
      death_date,           
      emigration_date,      
      na.rm = TRUE          
    ),
    time_entry = 0,
    event = ifelse(is.na(diagnosis_days) | time_exit != diagnosis_days, 0, 1)  # Event occurs if time_exit is due to long_covid diagnosis
  )
baseline_headache <- tmerge(
  data1 = headache_cohort_with_outcomes,
  data2 = headache_cohort_with_outcomes,
  id = PERSON_ID,
  tstart = time_entry,  # First test day is day 0
  tstop = time_exit,
  event = event(time_exit, event)
)

saveRDS(baseline_fatigue, file="baseline_fatigue.rds")
saveRDS(baseline_headache, file="baseline_headache.rds")

baseline_fatigue <- readRDS(file="baseline_fatigue.rds")
baseline_headache <- readRDS(file="baseline_headache.rds")

# Parallelized version -----------
# Add vaccination as a time-varying covariate ---------------
# Load Data
vaccination_data <- COVID_VACC

# Convert to data.table
setDT(fatigue_baseline_cohort_with_cci_region)
setDT(headache_baseline_cohort_with_cci_region)
setDT(vaccination_data)

# Filter vaccination data for relevant PERSON_IDs
vaccination_data <- vaccination_data[PERSON_ID %in% fatigue_baseline_cohort_with_cci_region$PERSON_ID |
                                       PERSON_ID %in% headache_baseline_cohort_with_cci_region$PERSON_ID]

# Merge vaccination data into long format
vaccination_long <- melt(vaccination_data, 
                         id.vars = "PERSON_ID",
                         measure.vars = c("FIRST_VACCINEDATE", "SECOND_VACCINEDATE", "THIRD_VACCINEDATE"),
                         variable.name = "vaccination_type",
                         value.name = "vaccination_date")

first_test_dates_fatigue <- fatigue_baseline_cohort_with_cci_region[, .(PERSON_ID, first_test_date)]
first_test_dates_headache <- headache_baseline_cohort_with_cci_region[, .(PERSON_ID, first_test_date)]

# Merge vaccination_long with first_test_dates
vaccination_long_fatigue <- merge(vaccination_long, first_test_dates_fatigue, by = "PERSON_ID")
vaccination_long_headache <- merge(vaccination_long, first_test_dates_headache, by = "PERSON_ID")

# Convert dates
vaccination_long_fatigue[, `:=`(first_test_date = as.Date(first_test_date),
                        vaccination_date = as.Date(vaccination_date))]
vaccination_long_headache[, `:=`(first_test_date = as.Date(first_test_date),
                                vaccination_date = as.Date(vaccination_date))]


# Add vaccination information -------
n_cores <- 8
cl <- makeCluster(n_cores)
registerDoParallel(cl)

#Fatigue parallelization

# Process in batches for memory efficiency
unique_ids <- unique(vaccination_long_fatigue$PERSON_ID)
n_batches <- 10
id_chunks <- split(unique_ids, cut(seq_along(unique_ids), n_batches, labels = FALSE))

# Process and save each batch separately
for (batch in seq_along(id_chunks)) {
  cat("Processing batch", batch, "of", n_batches, "\n")
  
  batch_results <- foreach(id = id_chunks[[batch]], .combine = 'rbind', .packages = 'data.table') %dopar% {
    person_data <- vaccination_long_fatigue[PERSON_ID == id]
    if (nrow(person_data) == 0) return(NULL)
    
    first_test_date <- unique(person_data$first_test_date)
    if (length(first_test_date) != 1 || is.na(first_test_date)) return(NULL)
    
    person_data <- person_data[order(vaccination_date)]
    past_vaccinations <- person_data[vaccination_date <= first_test_date]
    cumulative_vaccinations <- nrow(past_vaccinations)
    
    if (cumulative_vaccinations == 0) {
      vaccination_group_t0 <- "Unvaccinated"
    } else {
      last_vacc_date <- max(past_vaccinations$vaccination_date, na.rm = TRUE)
      weeks_since_last_vacc <- as.numeric(first_test_date - last_vacc_date) / 7
      vaccination_group_t0 <- ifelse(weeks_since_last_vacc <= 18,
                                     paste(cumulative_vaccinations, "< 18 weeks since vaccination"),
                                     paste(cumulative_vaccinations, ">= 18 weeks since vaccination"))
    }
    
    t_events <- data.table(PERSON_ID = id, time = 0, 
                           cumulative_vaccinations = cumulative_vaccinations, 
                           vaccination_group = vaccination_group_t0)
    
    future_vaccinations <- person_data[vaccination_date > first_test_date]
    
    for (i in 1:nrow(future_vaccinations)) {
      vacc_date <- future_vaccinations$vaccination_date[i]
      time_since_first_test <- as.numeric(vacc_date - first_test_date)
      cumulative_vaccinations <- min(cumulative_vaccinations + 1, 3)  # Max is 3 vaccinations
      
      t_events <- rbind(t_events, data.table(
        PERSON_ID = id,
        time = time_since_first_test,
        cumulative_vaccinations = cumulative_vaccinations,
        vaccination_group = paste(cumulative_vaccinations, "< 18 weeks since vaccination")
      ))
      
      time_18wks <- time_since_first_test + (18 * 7)
      next_vacc_date <- ifelse(i < nrow(future_vaccinations), future_vaccinations$vaccination_date[i + 1], NA)
      
      if (is.na(next_vacc_date) || next_vacc_date > vacc_date + (18 * 7)) {
        t_events <- rbind(t_events, data.table(
          PERSON_ID = id,
          time = time_18wks,
          cumulative_vaccinations = cumulative_vaccinations,
          vaccination_group = paste(cumulative_vaccinations, ">= 18 weeks since vaccination")
        ))
      }
    }
    return(t_events)
  }
  
  batch_filename <- paste0("fatigue_vaccination_batch_", batch, ".rds")
  saveRDS(batch_results, file = batch_filename)
  
  rm(batch_results)
  gc()
}

stopCluster(cl)

# Load and merge all batches
batch_files <- list.files(pattern = "fatigue_vaccination_batch_.*\\.rds")
fatigue_vaccination_final <- rbindlist(lapply(batch_files, readRDS), use.names = TRUE, fill = TRUE)
fatigue_vaccination_final <- fatigue_vaccination_final[!is.na(time)]

saveRDS(fatigue_vaccination_final, file = "fatigue_vaccination_final.rds")


# Merge vaccination data into cohort
baseline_fatigue_cum_vacc <- tmerge(
  data1 = baseline_fatigue,
  data2 = fatigue_vaccination_final,
  id = PERSON_ID,
  cum_vacc = tdc(time, cumulative_vaccinations)
)

baseline_fatigue_vacc_group <- tmerge(
  data1 = baseline_fatigue,
  data2 = fatigue_vaccination_final,
  id = PERSON_ID,
  vacc_group = tdc(time, vaccination_group)
)

saveRDS(baseline_fatigue_cum_vacc, file="baseline_fatigue_cum_vacc.rds")
saveRDS(baseline_fatigue_vacc_group, file="baseline_fatigue_vacc_group.rds")


# Headache parallelization---
n_cores <- 8
cl <- makeCluster(n_cores)
registerDoParallel(cl)
unique_ids <- unique(vaccination_long_headache$PERSON_ID)
n_batches <- 10
id_chunks <- split(unique_ids, cut(seq_along(unique_ids), n_batches, labels = FALSE))

# Process and save each batch separately
for (batch in seq_along(id_chunks)) {
  cat("Processing batch", batch, "of", n_batches, "\n")
  
  batch_results <- foreach(id = id_chunks[[batch]], .combine = 'rbind', .packages = 'data.table') %dopar% {
    person_data <- vaccination_long_headache[PERSON_ID == id]
    if (nrow(person_data) == 0) return(NULL)
    
    first_test_date <- unique(person_data$first_test_date)
    if (length(first_test_date) != 1 || is.na(first_test_date)) return(NULL)
    
    person_data <- person_data[order(vaccination_date)]
    past_vaccinations <- person_data[vaccination_date <= first_test_date]
    cumulative_vaccinations <- nrow(past_vaccinations)
    
    if (cumulative_vaccinations == 0) {
      vaccination_group_t0 <- "Unvaccinated"
    } else {
      last_vacc_date <- max(past_vaccinations$vaccination_date, na.rm = TRUE)
      weeks_since_last_vacc <- as.numeric(first_test_date - last_vacc_date) / 7
      vaccination_group_t0 <- ifelse(weeks_since_last_vacc <= 18,
                                     paste(cumulative_vaccinations, "< 18 weeks since vaccination"),
                                     paste(cumulative_vaccinations, ">= 18 weeks since vaccination"))
    }
    
    t_events <- data.table(PERSON_ID = id, time = 0, 
                           cumulative_vaccinations = cumulative_vaccinations, 
                           vaccination_group = vaccination_group_t0)
    
    future_vaccinations <- person_data[vaccination_date > first_test_date]
    
    for (i in 1:nrow(future_vaccinations)) {
      vacc_date <- future_vaccinations$vaccination_date[i]
      time_since_first_test <- as.numeric(vacc_date - first_test_date)
      cumulative_vaccinations <- min(cumulative_vaccinations + 1, 3)  # Max is 3 vaccinations
      
      t_events <- rbind(t_events, data.table(
        PERSON_ID = id,
        time = time_since_first_test,
        cumulative_vaccinations = cumulative_vaccinations,
        vaccination_group = paste(cumulative_vaccinations, "< 18 weeks since vaccination")
      ))
      
      time_18wks <- time_since_first_test + (18 * 7)
      next_vacc_date <- ifelse(i < nrow(future_vaccinations), future_vaccinations$vaccination_date[i + 1], NA)
      
      if (is.na(next_vacc_date) || next_vacc_date > vacc_date + (18 * 7)) {
        t_events <- rbind(t_events, data.table(
          PERSON_ID = id,
          time = time_18wks,
          cumulative_vaccinations = cumulative_vaccinations,
          vaccination_group = paste(cumulative_vaccinations, ">= 18 weeks since vaccination")
        ))
      }
    }
    return(t_events)
  }
  
  batch_filename <- paste0("headache_vaccination_batch_", batch, ".rds")
  saveRDS(batch_results, file = batch_filename)
  
  rm(batch_results)
  gc()
}

stopCluster(cl)

# Load and merge all batches
batch_files <- list.files(pattern = "headache_vaccination_batch_.*\\.rds")
headache_vaccination_final <- rbindlist(lapply(batch_files, readRDS), use.names = TRUE, fill = TRUE)
headache_vaccination_final <- headache_vaccination_final[!is.na(time)]

saveRDS(headache_vaccination_final, file = "headache_vaccination_final.rds")


# Merge vaccination data into long COVID cohort
baseline_headache_cum_vacc <- tmerge(
  data1 = baseline_headache,
  data2 = headache_vaccination_final,
  id = PERSON_ID,
  cum_vacc = tdc(time, cumulative_vaccinations)
)

baseline_headache_vacc_group <- tmerge(
  data1 = baseline_headache,
  data2 = headache_vaccination_final,
  id = PERSON_ID,
  vacc_group = tdc(time, vaccination_group)
)

saveRDS(baseline_headache_cum_vacc, file="baseline_headache_cum_vacc.rds")
saveRDS(baseline_headache_vacc_group, file="baseline_headache_vacc_group.rds")



# Add number of SARS-CoV-2 infections as a time-varying covariate ---------------
covid_tests <- readRDS(file="COVID_TEST.rds")

#Get baseline stuff
fatigue_baseline_cohort_with_cci_region <- readRDS("fatigue_baseline_cohort_with_cci_region.rds")
headache_baseline_cohort_with_cci_region <- readRDS("headache_baseline_cohort_with_cci_region.rds")

# Convert to data.table
covid_tests <- as.data.table(covid_tests)
fatigue_baseline_cohort_with_cci_region <- as.data.table(fatigue_baseline_cohort_with_cci_region)
headache_baseline_cohort_with_cci_region <- as.data.table(headache_baseline_cohort_with_cci_region)
covid_tests_fatigue <- covid_tests[PERSON_ID %in% fatigue_baseline_cohort_with_cci_region$PERSON_ID]
covid_tests_headache <- covid_tests[PERSON_ID %in% headache_baseline_cohort_with_cci_region$PERSON_ID]

# Extract first_test_date into a named vector
first_test_date_lookup_fatigue <- setNames(fatigue_baseline_cohort_with_cci_region$first_test_date, 
                                   fatigue_baseline_cohort_with_cci_region$PERSON_ID)
first_test_date_lookup_headache <- setNames(headache_baseline_cohort_with_cci_region$first_test_date, 
                                            headache_baseline_cohort_with_cci_region$PERSON_ID)
# Define infection scenarios
infection_periods <- c(90)
delay_months <- c(0)

#Subsetting to only include adults:
fatigue_baseline_cohort_with_cci_region <- fatigue_baseline_cohort_with_cci_region[age_at_first_test >= 18]
headache_baseline_cohort_with_cci_region <- headache_baseline_cohort_with_cci_region[age_at_first_test >= 18]


process_infection_scenario_simple <- function(period, covid_tests, cohort, output_prefix = "infection_period") {
  
  library(data.table)
  
  # Prepare covid_tests and cohort
  covid_tests <- covid_tests[, .(PERSON_ID, PRDATE_ADJUSTED, SVARRESULTAT)]
  cohort <- cohort[, .(PERSON_ID, first_test_date)]
  
  # Merge first_test_date onto covid_tests
  setkey(covid_tests, PERSON_ID)
  setkey(cohort, PERSON_ID)
  covid_tests <- covid_tests[cohort, nomatch = 0]
  
  # Make sure dates are properly formatted
  covid_tests[, PRDATE_ADJUSTED := as.Date(PRDATE_ADJUSTED)]
  cohort[, first_test_date := as.Date(first_test_date)]
  
  # Keep negative tests close to first test date (within 3 days)
  covid_tests <- covid_tests[
    (SVARRESULTAT == 0L & abs(as.integer(PRDATE_ADJUSTED - first_test_date)) <= 3) |
      (SVARRESULTAT == 1L)  # Keep ALL positives regardless of date
  ]
  
  # Order
  setorder(covid_tests, PERSON_ID, PRDATE_ADJUSTED)
  
  # Prepare output
  results_list <- vector("list", nrow(cohort))
  
  person_ids <- cohort$PERSON_ID
  total <- length(person_ids)
  
  for (i in seq_along(person_ids)) {
    id <- person_ids[i]
    first_test <- cohort[PERSON_ID == id, first_test_date]
    person_tests <- covid_tests[PERSON_ID == id]
    
    # Always start with time = 0
    events <- data.table(PERSON_ID = id, time = 0, infections_cumulative = 0L)
    
    infection_count <- 0L
    
    if (nrow(person_tests) > 0) {
      last_infection_date <- as.Date(NA)
      
      for (j in seq_len(nrow(person_tests))) {
        test_date <- person_tests$PRDATE_ADJUSTED[j]
        result <- person_tests$SVARRESULTAT[j]
        days_since_first <- as.integer(test_date - first_test)
        
        if (result == 1L) {  # Positive
          if (is.na(last_infection_date) || (test_date - last_infection_date) > period) {
            infection_count <- infection_count + 1L
            last_infection_date <- test_date
            events <- rbind(events, data.table(PERSON_ID = id, time = days_since_first, infections_cumulative = infection_count))
          }
        }
      }
    }
    
    results_list[[i]] <- events
    
    # Progress print every 50,000 people
    if (i %% 50000 == 0) {
      cat(sprintf("[%s] Processed %d / %d people\n", Sys.time(), i, total))
    }
  }
  
  # Bind and save
  final_result <- rbindlist(results_list, use.names = TRUE)
  saveRDS(final_result, file = sprintf("%s_%d_delay_0.rds", output_prefix, period))
  
  cat(sprintf("[%s] Completed processing all %d people!\n", Sys.time(), total))
}


process_infection_scenario_simple(
  period = 90,
  covid_tests = covid_tests_fatigue,
  cohort = fatigue_baseline_cohort_with_cci_region,
  output_prefix = "fatigue_infection_period"
)

process_infection_scenario_simple(
  period = 90,
  covid_tests = covid_tests_headache,
  cohort = headache_baseline_cohort_with_cci_region,
  output_prefix = "headache_infection_period"
)

fatigue_infection_episodes <- readRDS(file="fatigue_infection_period_90_delay_0.rds")
headache_infection_episodes <- readRDS(file="headache_infection_period_90_delay_0.rds")

# Cleaning rows
# Step 1: Find PERSON_IDs with multiple rows at time == 0
duplicate_person_ids_fatigue <- fatigue_infection_episodes %>%
  filter(time == 0) %>%
  group_by(PERSON_ID) %>%
  tally() %>%
  filter(n > 1) %>%
  pull(PERSON_ID)
duplicate_person_ids_headache <- headache_infection_episodes %>%
  filter(time == 0) %>%
  group_by(PERSON_ID) %>%
  tally() %>%
  filter(n > 1) %>%
  pull(PERSON_ID)
# Step 2: Identify the rows you want to REMOVE (where time == 0, PERSON_ID duplicated, and infections_cumulative == 0)
rows_to_remove_fatigue <- fatigue_infection_episodes %>%
  filter(PERSON_ID %in% duplicate_person_ids_fatigue, time == 0, infections_cumulative == 0)
rows_to_remove_headache <- headache_infection_episodes %>%
  filter(PERSON_ID %in% duplicate_person_ids_headache, time == 0, infections_cumulative == 0)
# Step 3: Remove these rows from the original data
fatigue_infection_episodes_clean <- anti_join(fatigue_infection_episodes, rows_to_remove_fatigue, by = c("PERSON_ID", "time", "infections_cumulative"))
headache_infection_episodes_clean <- anti_join(headache_infection_episodes, rows_to_remove_headache, by = c("PERSON_ID", "time", "infections_cumulative"))

saveRDS(fatigue_infection_episodes_clean, file="fatigue_clean_infection_period_90_delay_0.rds")
saveRDS(headache_infection_episodes_clean, file="headache_clean_infection_period_90_delay_0.rds")

#FINAL MERGING STEP ------------
# --- Get vaccination data, cleaned infection data and baseline cohorts ---

fatigue_vaccination_final <- readRDS(file = "fatigue_vaccination_final.rds")
headache_vaccination_final <- readRDS(file = "headache_vaccination_final.rds")

fatigue_infection_episodes_clean <- readRDS(file="fatigue_clean_infection_period_90_delay_0.rds")
headache_infection_episodes_clean <- readRDS(file="headache_clean_infection_period_90_delay_0.rds")

baseline_fatigue <- readRDS(file="baseline_fatigue.rds")
baseline_headache <- readRDS(file="baseline_headache.rds")

#Merged vaccination and infection data
merged_data_fatigue <- merge(fatigue_vaccination_final, fatigue_infection_episodes_clean, by = c("PERSON_ID", "time"), all = TRUE)
merged_data_headache <- merge(headache_vaccination_final, headache_infection_episodes_clean, by = c("PERSON_ID", "time"), all = TRUE)

#read in basic data
fatigue_baseline_cohort_fatigue <- readRDS(file="fatigue_baseline_cohort_fatigue.rds")
headache_baseline_cohort_headache_outcome <- readRDS(file="headache_baseline_cohort_headache_outcome.rds")


#Subsetting to only include adults:
fatigue_baseline_cohort_fatigue <- subset(fatigue_baseline_cohort_fatigue, age_at_first_test >= 18)
headache_baseline_cohort_headache_outcome <- subset(headache_baseline_cohort_headache_outcome, age_at_first_test >= 18)

#fatigue, adding event
fatigue_cohort_with_outcomes <- fatigue_baseline_cohort_fatigue %>%
  mutate(
    time_exit = pmin(
      last_possible_day,    
      diagnosis_days,  
      death_date,           
      emigration_date,      
      na.rm = TRUE          
    ),
    time_entry = 0,
    event = ifelse(is.na(diagnosis_days) | time_exit != diagnosis_days, 0, 1)  # Event occurs if time_exit is due to fatigue diagnosis
  )
baseline_fatigue <- tmerge(
  data1 = fatigue_cohort_with_outcomes,
  data2 = fatigue_cohort_with_outcomes,
  id = PERSON_ID,
  tstart = time_entry,  # First test day is day 0
  tstop = time_exit,
  event = event(time_exit, event)
)
fatigue_cum_vacc <- tmerge(
  data1 = baseline_fatigue,
  data2 = fatigue_vaccination_final,
  id = PERSON_ID,
  cum_vacc = tdc(time, cumulative_vaccinations)
)
fatigue_cum_vacc_inf <- tmerge(
  data1 = fatigue_cum_vacc,
  data2 = fatigue_infection_episodes_clean,
  id = PERSON_ID,
  inf_cumulative = tdc(time, infections_cumulative)
)
nrow(fatigue_cum_vacc_inf[fatigue_cum_vacc_inf$event == 1, ])
saveRDS(fatigue_cum_vacc_inf, file="baseline_tmerged_fatigue_cohort_period_90_delay_0.rds")


#headache endpoint, adding event
headache_cohort_with_outcomes <- headache_baseline_cohort_headache_outcome %>%
  mutate(
    time_exit = pmin(
      last_possible_day,    
      diagnosis_days,  
      death_date,           
      emigration_date,      
      na.rm = TRUE          
    ),
    time_entry = 0,
    event = ifelse(is.na(diagnosis_days) | time_exit != diagnosis_days, 0, 1)  # Event occurs if time_exit is due to fatigue diagnosis
  )
baseline_headache <- tmerge(
  data1 = headache_cohort_with_outcomes,
  data2 = headache_cohort_with_outcomes,
  id = PERSON_ID,
  tstart = time_entry,  # First test day is day 0
  tstop = time_exit,
  event = event(time_exit, event)
)

headache_cum_vacc <- tmerge(
  data1 = baseline_headache,
  data2 = headache_vaccination_final,
  id = PERSON_ID,
  cum_vacc = tdc(time, cumulative_vaccinations)
)
headache_cum_vacc_inf <- tmerge(
  data1 = headache_cum_vacc,
  data2 = headache_infection_episodes_clean,
  id = PERSON_ID,
  inf_cumulative = tdc(time, infections_cumulative)
)
saveRDS(headache_cum_vacc_inf, file="baseline_tmerged_headache_cohort_period_90_delay_0.rds")
