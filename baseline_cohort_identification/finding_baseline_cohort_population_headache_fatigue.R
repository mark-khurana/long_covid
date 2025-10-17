# read packages
lapply(c("dplyr","tidyverse","data.table","dbplyr","lubridate","ggsignif","purrr","tidyr","furrr","DSTora","ROracle","DSTcolectica","DSTdb","DBI","parallel","doParallel","foreach","Rcpp","future","survival","riskRegression","ggplot2","prodlim","cmprsk","future.apply"), library, character.only = TRUE)

# Loading in metadata ----------------------------------------------
drv <- dbDriver('Oracle')
conn <- DSTora::OraGenvej('', dbuser = '')
con2 <- DSTora::OraGenvej('', dbuser = '')
con3 <- DSTora::OraGenvej('', dbuser = '')

COVID_TEST <- dbReadTable(conn = conn,
                          name = "",
                          schema = '')

COVID_VACC <- dbReadTable(conn = conn,
                          name = "",
                          schema = '')

lifelines <- dbReadTable(conn = conn,
                         name = "",
                         schema = '')

lifelines_koen <- dbReadTable(conn = conn,
                              name = "",
                              schema = '')
# Getting baseline cohorts from previous steps -----------
step_6_f <- readRDS("step_6_f.rds")

# Finding baseline fatigue cohort -----------------
# List of fatigue ICD-10 codes: 
fatigue_codes <- data.frame(icd_10 = c("R53", "R539", "R539A", "R539C","R539E","R539F","G933")
)
fatigue_codes$diag_code <- gsub("\\.", "", paste0("D", fatigue_codes$icd_10))
fatigue_diag_codes_list <- paste0("'", fatigue_codes$diag_code, "'", collapse = ", ")
fatigue_query_LPR3 <- paste0("SELECT * FROM D282171.PSD_LPR3_KONTAKTER ",
                          "WHERE AKTIONSDIAGNOSE IN (", fatigue_diag_codes_list, ") ")
fatigue_diagnoses_LPR3 <- dbGetQuery(con3, fatigue_query_LPR3)
fatigue_query_LPR2 <- paste0("SELECT * FROM D222008.LPRDIAG_PID ",
                          "WHERE c_DIAG IN (", fatigue_diag_codes_list, ") ")
fatigue_diagnoses_LPR2 <- dbGetQuery(conn, fatigue_query_LPR2)
fatigue_diagnoses_LPR2 <- fatigue_diagnoses_LPR2 %>%
  rename(AKTIONSDIAGNOSE = C_DIAG)

# Load required package
setDT(fatigue_diagnoses_LPR3)
setDT(fatigue_diagnoses_LPR2)
setDT(step_6_f)
fatigue_diagnoses_LPR3[, DATO_START := as.Date(DATO_START)]  # Convert DATO_START to Date format
step_6_f[, first_test_date := as.Date(first_test_date)]    # Ensure first_test_date is Date format
step_6_f[, follow_up_start_one_month := as.Date(follow_up_start_one_month)]    # Ensure first_test_date is Date format

# Step 1: Subset to only PERSON_IDs that appear in either fatigue_diagnoses_LPR3 or fatigue_diagnoses_LPR2
# Get PERSON_IDs that appear in either LPR dataframe
relevant_persons_LPR3 <- unique(fatigue_diagnoses_LPR3$PERSON_ID)
relevant_persons_LPR2 <- unique(fatigue_diagnoses_LPR2$PERSON_ID)
relevant_persons <- unique(c(relevant_persons_LPR3, relevant_persons_LPR2))
# Subset step_6_f to only relevant PERSON_IDs
step_6_f_relevant <- step_6_f[PERSON_ID %in% relevant_persons]
remove_ids <- vector()
# Step 2: Loop through each relevant PERSON_ID in the subsetted dataframe
for (person_id in unique(step_6_f_relevant$PERSON_ID)) {
  # Get first test date for this PERSON_ID
  first_follow_up <- step_6_f_relevant[PERSON_ID == person_id, first_test_date]
  
  # Check if there are any corresponding rows in fatigue_diagnoses_LPR3 where DATO_START is before first test date
  if (any(fatigue_diagnoses_LPR3[PERSON_ID == person_id, DATO_START < first_follow_up])) {
    remove_ids <- c(remove_ids, person_id)
  }
}
# Step 3: Remove those from the main cohort based on LPR3 condition
fatigue_baseline_cohort <- step_6_f[!PERSON_ID %in% remove_ids]
removed_LPR3 <- length(remove_ids)
cat("Removed", removed_LPR3, "individuals based on fatigue_diagnoses_LPR3.\n")

# Step 4: Now do the same for fatigue_diagnoses_LPR2 (year-based filtering)
remove_ids_LPR2 <- vector()
# Loop through each relevant PERSON_ID in the subsetted dataframe again
for (person_id in unique(step_6_f_relevant$PERSON_ID)) {
  # Get first test year for this PERSON_ID
  first_test_year <- year(step_6_f_relevant[PERSON_ID == person_id, first_test_date])
  
  # Check if there are any corresponding rows in fatigue_diagnoses_LPR2 where AAR is before first test year
  if (any(fatigue_diagnoses_LPR2[PERSON_ID == person_id, AAR < first_test_year])) {
    remove_ids_LPR2 <- c(remove_ids_LPR2, person_id)
  }
}
# Step 5: Remove those from the cohort based on LPR2 condition
fatigue_baseline_cohort <- fatigue_baseline_cohort[!PERSON_ID %in% remove_ids_LPR2]
removed_LPR2 <- length(remove_ids_LPR2)
cat("Removed", removed_LPR2, "individuals based on fatigue_diagnoses_LPR2.\n")
saveRDS(fatigue_baseline_cohort, "fatigue_baseline_cohort.rds")

# Finding baseline headache cohort -----------------
# List of headache ICD-10 codes: 
headache_codes <- data.frame(icd_10 = c("R51", "R511", "R512",
                                        "R513","R514","R515",
                                        "R516","R517","R518",
                                        "R519")
)
headache_codes$diag_code <- gsub("\\.", "", paste0("D", headache_codes$icd_10))
headache_diag_codes_list <- paste0("'", headache_codes$diag_code, "'", collapse = ", ")
headache_query_LPR3 <- paste0("SELECT * FROM D282171.PSD_LPR3_KONTAKTER ",
                             "WHERE AKTIONSDIAGNOSE IN (", headache_diag_codes_list, ") ")
headache_diagnoses_LPR3 <- dbGetQuery(con3, headache_query_LPR3)
headache_query_LPR2 <- paste0("SELECT * FROM D222008.LPRDIAG_PID ",
                             "WHERE c_DIAG IN (", headache_diag_codes_list, ") ")
headache_diagnoses_LPR2 <- dbGetQuery(conn, headache_query_LPR2)
headache_diagnoses_LPR2 <- headache_diagnoses_LPR2 %>%
  rename(AKTIONSDIAGNOSE = C_DIAG)

# Load required package
setDT(headache_diagnoses_LPR3)
setDT(headache_diagnoses_LPR2)
setDT(step_6_f)
headache_diagnoses_LPR3[, DATO_START := as.Date(DATO_START)]  # Convert DATO_START to Date format
step_6_f[, first_test_date := as.Date(first_test_date)]    # Ensure first_test_date is Date format

# Step 1: Subset to only PERSON_IDs that appear in either headache_diagnoses_LPR3 or headache_diagnoses_LPR2
# Get PERSON_IDs that appear in either LPR dataframe
relevant_persons_LPR3 <- unique(headache_diagnoses_LPR3$PERSON_ID)
relevant_persons_LPR2 <- unique(headache_diagnoses_LPR2$PERSON_ID)
relevant_persons <- unique(c(relevant_persons_LPR3, relevant_persons_LPR2))
# Subset step_6_f to only relevant PERSON_IDs
step_6_f_relevant <- step_6_f[PERSON_ID %in% relevant_persons]
remove_ids <- vector()
# Step 2: Loop through each relevant PERSON_ID in the subsetted dataframe
for (person_id in unique(step_6_f_relevant$PERSON_ID)) {
  # Get first test date for this PERSON_ID
  first_follow_up <- step_6_f_relevant[PERSON_ID == person_id, first_test_date]
  
  # Check if there are any corresponding rows in headache_diagnoses_LPR3 where DATO_START is before first test date
  if (any(headache_diagnoses_LPR3[PERSON_ID == person_id, DATO_START < first_follow_up])) {
    remove_ids <- c(remove_ids, person_id)
  }
}
# Step 3: Remove those from the main cohort based on LPR3 condition
headache_baseline_cohort <- step_6_f[!PERSON_ID %in% remove_ids]
removed_LPR3 <- length(remove_ids)
cat("Removed", removed_LPR3, "individuals based on headache_diagnoses_LPR3.\n")
# Step 4: Now do the same for headache_diagnoses_LPR2 (year-based filtering)
remove_ids_LPR2 <- vector()
# Loop through each relevant PERSON_ID in the subsetted dataframe again
for (person_id in unique(step_6_f_relevant$PERSON_ID)) {
  # Get first test year for this PERSON_ID
  first_test_year <- year(step_6_f_relevant[PERSON_ID == person_id, first_test_date])
  
  # Check if there are any corresponding rows in headache_diagnoses_LPR2 where AAR is before first test year
  if (any(headache_diagnoses_LPR2[PERSON_ID == person_id, AAR < first_test_year])) {
    remove_ids_LPR2 <- c(remove_ids_LPR2, person_id)
  }
}
# Step 5: Remove those from the cohort based on LPR2 condition
headache_baseline_cohort <- headache_baseline_cohort[!PERSON_ID %in% remove_ids_LPR2]
removed_LPR2 <- length(remove_ids_LPR2)
cat("Removed", removed_LPR2, "individuals based on headache_diagnoses_LPR2.\n")
saveRDS(headache_baseline_cohort, "headache_baseline_cohort.rds")

# Add CCI ---------------
#Also from previous code
diagnoses_LPR3 <- readRDS("diagnoses_LPR3.rds")
diagnoses_LPR2 <- readRDS("diagnoses_LPR2.rds")

# Standardize column names and format dates
diagnoses_LPR2[, AKTIONSDIAGNOSE := C_DIAG]
diagnoses_LPR2[, diagnosis_date := as.Date(paste0(AAR, "-01-01"))]
diagnoses_LPR3[, diagnosis_date := as.Date(DATO_START)]

# Combine both datasets
diagnoses_combined <- rbindlist(list(
  diagnoses_LPR3[, .(PERSON_ID, AKTIONSDIAGNOSE, diagnosis_date)],
  diagnoses_LPR2[, .(PERSON_ID, AKTIONSDIAGNOSE, diagnosis_date)]
))

# Define charlson.codes with codes starting with "D"
charlson.codes <- list(
  myocardial.infarction = c('410', 'DI21', 'DI22'),
  heart.failure = c('42709', '42710', '42711', '42719', '42899', '78249', 'DI099', 'DI110', 'DI130', 'DI132', 'DI255', 'DI425', 'DI426', 'DI427', 'DI429', 'DI428A', 'DP290', 'DI43', 'DI50', 'DE105', 'DE115', 'DE125', 'DE135', 'DE145'),
  peripheral.vascular.disease = c('440', '441', '442', '443', '444', '445', 'DI70', 'DI71', 'DI72', 'DI731', 'DI738', 'DI739', 'DI77', 'DI790', 'DI792', 'DK551', 'DK558', 'DK559', 'DZ958', 'DZ959'),
  cerebrovascular.disease = c(paste0('43', 0:8), paste0('DI6', 0:9), 'DG45', 'DG46', 'DH340'),
  dementia = c('290', paste0('DF0', 0:3), 'DG30', 'DF051', 'DG311'),
  chronic.pulmonary.disease = c(paste0('51', 5:8), paste0('49', 0:3), paste0('DJ4', 0:7), paste0('DJ6', 0:7), 'DJ684', 'DI278', 'DI279', 'DJ84', 'DJ701', 'DJ703', 'DJ920', 'DJ953', 'DJ961', 'DJ982', 'DJ983'), 
  rheumatic.disease = c('712', '716', '734', '446', '13599', 'DM05', 'DM06', 'DM08', 'DM09', 'DM30', 'DM31', 'DM32', 'DM33', 'DM34', 'DM35', 'DM36', 'D86'),
  peptic.ulcer.disease = c('53091', '53098', paste0('53', 1:4), 'DK25', 'DK26', 'DK27', 'DK28', 'DK221'),
  mild.liver.disease = c('571', '57301', '57304', 'DB18', 'DK700', 'DK701', 'DK702', 'DK709', 'DK703', 'DK713', 'DK714', 'DK715', 'DK717', 'DK73', 'DK74', 'DK760', 'DK762', 'DK763', 'DK764', 'DK769', 'DZ944'),
  severe.liver.disease = c('07000', '07002', '07004', '07006', '07008', '57300', '45601', '45602', '45603', '45604', '45605', '45606', '45607', '45608', '45609', 'DB150', 'DB160', 'DB162', 'DB190', 'DI850', 'DI859', 'DI864', 'DI982', 'DK704', 'DK711', 'DK721', 'DK729', 'DK765', 'DK766', 'DK767'), 
  diabetes.without.complications = c('24900', '24906', '24907', '24909', '25000', '25006', '25007', '25009', 'DE100', 'DE101', 'DE108', 'DE109', 'DE110', 'DE111', 'DE119', 'DE120', 'DE121', 'DE129', 'DE130', 'DE131', 'DE139', 'DE140', 'DE141', 'DE149'),
  diabetes.with.complications = c(paste0('2490', 1:5), '24908', paste0('2500', 1:5), '25008', paste0('DE10', 2:7), paste0('DE11', 2:8), paste0('DE12', 2:8), paste0('DE13', 2:8), paste0('DE14', 2:8)),
  hemiplegia.paraplegia = c('344', paste0('DG83', 0:4), 'DG81', 'DG82', 'DG041', 'DG114', 'DG801', 'DG802', 'DG839'),
  renal.disease = c('403', '404', paste0('58', 0:4), '59009', '59319', paste0('7531', 0:9), '792', paste0('DN03', 2:7), paste0('DN05', 2:7), 'DZ490', 'DZ491', 'DZ492', 'DN18', 'DN19', 'DI120', 'DI131', 'DI132', 'DN250', 'DZ940', 'DZ992', 'DN26'),
  any.malignancy = c(paste0('1', 40:72), paste0(174:194), '27559', paste0('DC', 0:3), paste0('DC4', 0:9), 'DC5', 'DC6', paste0('DC7', 0:6), 'DC86', 'DC97'),
  metastatic.solid.tumor = c(paste0('19', 5:9), paste0('DC', 77:80)),
  AIDS.HIV = c('07983', 'DB20', 'DB21', 'DB22', 'DB23', 'DB24'),
  leukemia = c(paste0('20', 4:7), paste0('DC9', 1:5)),
  lymphoma = c(paste0('20', 0:3), '27559', paste0('DC8', 1:5), 'DC88', 'DC90', 'DC96')
)

add_trailing_digits <- function(codes) {
  extended_codes <- unlist(lapply(codes, function(code) {
    c(code, paste0(code, 0:9))  # Keep the original code and append 0-9
  }))
  return(extended_codes)
}
charlson.codes.extended <- lapply(charlson.codes, add_trailing_digits)
# Function to filter out the codes starting with "D"
filter_codes_starting_with_D <- function(codes) {
  filtered_codes <- lapply(codes, function(code_list) {
    code_list[startsWith(code_list, "D")]
  })
  return(filtered_codes)
}
# Convert charlson.codes into a data.table for efficient filtering
charlson_codes_filtered <- as.data.table(unlist(filter_codes_starting_with_D(charlson.codes.extended)))

# fatigue Group --
diagnoses_filtered_fatigue <- diagnoses_combined[
  PERSON_ID %in% fatigue_baseline_cohort$PERSON_ID & 
    AKTIONSDIAGNOSE %in% unlist(charlson_codes_filtered)
]
fatigue_diagnoses <- diagnoses_filtered_fatigue %>%
  inner_join(fatigue_baseline_cohort %>% select(PERSON_ID, first_test_date), by = "PERSON_ID")

fatigue_charlson_results_5yrs <- charlsonIndex(
  data = fatigue_diagnoses,
  ptid = "PERSON_ID",
  vars = "AKTIONSDIAGNOSE",
  data.date = "diagnosis_date",
  charlson.date = "first_test_date",
  look.back = 5,
  ccodes = charlson.codes.extended
)
fatigue_charlson_results_10yrs <- charlsonIndex(
  data = fatigue_diagnoses,
  ptid = "PERSON_ID",
  vars = "AKTIONSDIAGNOSE",
  data.date = "diagnosis_date",
  charlson.date = "first_test_date",
  look.back = 10,
  ccodes = charlson.codes.extended
)
fatigue_baseline_cohort_with_cci <- fatigue_baseline_cohort %>%
  left_join(fatigue_charlson_results_5yrs[[1]] %>% rename(charlson.index.5yrs = charlson.index), by = c("PERSON_ID", "first_test_date")) %>%
  mutate(charlson.index.5yrs = ifelse(is.na(charlson.index.5yrs), 0, charlson.index.5yrs)) %>%
  left_join(fatigue_charlson_results_10yrs[[1]] %>% rename(charlson.index.10yrs = charlson.index), by = c("PERSON_ID", "first_test_date")) %>%
  mutate(charlson.index.10yrs = ifelse(is.na(charlson.index.10yrs), 0, charlson.index.10yrs))
saveRDS(fatigue_baseline_cohort_with_cci, "fatigue_baseline_cohort_with_cci.rds")

# headache Group --
diagnoses_filtered_headache <- diagnoses_combined[
  PERSON_ID %in% headache_baseline_cohort$PERSON_ID & 
    AKTIONSDIAGNOSE %in% unlist(charlson_codes_filtered)
]
headache_diagnoses <- diagnoses_filtered_headache %>%
  inner_join(headache_baseline_cohort %>% select(PERSON_ID, first_test_date), by = "PERSON_ID")

headache_charlson_results_5yrs <- charlsonIndex(
  data = headache_diagnoses,
  ptid = "PERSON_ID",
  vars = "AKTIONSDIAGNOSE",
  data.date = "diagnosis_date",
  charlson.date = "first_test_date",
  look.back = 5,
  ccodes = charlson.codes.extended
)
headache_charlson_results_10yrs <- charlsonIndex(
  data = headache_diagnoses,
  ptid = "PERSON_ID",
  vars = "AKTIONSDIAGNOSE",
  data.date = "diagnosis_date",
  charlson.date = "first_test_date",
  look.back = 10,
  ccodes = charlson.codes.extended
)
headache_baseline_cohort_with_cci <- headache_baseline_cohort %>%
  left_join(headache_charlson_results_5yrs[[1]] %>% rename(charlson.index.5yrs = charlson.index), by = c("PERSON_ID", "first_test_date")) %>%
  mutate(charlson.index.5yrs = ifelse(is.na(charlson.index.5yrs), 0, charlson.index.5yrs)) %>%
  left_join(headache_charlson_results_10yrs[[1]] %>% rename(charlson.index.10yrs = charlson.index), by = c("PERSON_ID", "first_test_date")) %>%
  mutate(charlson.index.10yrs = ifelse(is.na(charlson.index.10yrs), 0, charlson.index.10yrs))
saveRDS(headache_baseline_cohort_with_cci, "headache_baseline_cohort_with_cci.rds")