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

# Getting baseline cohorts -----------
# Step 1: Subset data where PRDATE_ADJUSTED is before 1 Jan 2023 and CASEDEF is "SARS2" or "SARSG"
step_1_baseline_cohort <- COVID_TEST %>%
  filter(PRDATE_ADJUSTED < as.Date("2023-01-01") & CASEDEF %in% c("SARS2", "SARSG"))
# Step 2: Keep the first PRDATE_ADJUSTED for each unique PERSON_ID
step_2_b <- step_1_baseline_cohort %>%
  group_by(PERSON_ID) %>%
  summarise(first_test_date = min(PRDATE_ADJUSTED), .groups = "drop")
# Step 3: Filter step_2_b to only include PERSON_IDs present in both lifelines and lifelines_koen
step_3_c <- step_2_b %>%
  filter(PERSON_ID %in% lifelines$PERSON_ID & PERSON_ID %in% lifelines_koen$PERSON_ID)
saveRDS(step_1_baseline_cohort, "step_1_baseline_cohort.rds")
saveRDS(step_2_b, "step_2_b.rds")
saveRDS(step_3_c, "step_3_c.rds")
# Step 4: Keep only PERSON_IDs that are also in COVID_VACC
step_4_d <- step_3_c %>%
  filter(PERSON_ID %in% COVID_VACC$PERSON_ID)
# Step 5: Handle multiple rows in lifelines and add birthday, sex, and age at first test
setDT(step_4_d)
setDT(lifelines)
setDT(lifelines_koen)
lifelines_unique <- lifelines[, .(BIRTHDAY = min(BIRTHDAY)), by = PERSON_ID]
lifelines_koen_unique <- lifelines_koen[, .(KOEN = first(KOEN)), by = PERSON_ID]
step_5_e <- merge(step_4_d, lifelines_unique, by = "PERSON_ID", all.x = TRUE)
step_5_e <- merge(step_5_e, lifelines_koen_unique[, .(PERSON_ID, KOEN)], by = "PERSON_ID", all.x = TRUE)
step_5_e[, age_at_first_test := as.numeric(difftime(first_test_date, BIRTHDAY, units = "days")) / 365.25]
step_5_e <- step_5_e[, .(PERSON_ID, first_test_date, BIRTHDAY, age_at_first_test, KOEN)]
step_6_f <- step_5_e[!is.na(BIRTHDAY) & !is.na(KOEN)]
saveRDS(step_6_f, "step_6_f.rds")
step_6_f <- readRDS("step_6_f.rds")

# Finding baseline long covid cohort -----------------
long_covid_codes <- data.frame(
  icd_10 = c("B94.8A"))
long_covid_codes$diag_code <- gsub("\\.", "", paste0("D", long_covid_codes$icd_10))

long_covid_diag_codes_list <- paste0("'", long_covid_codes$diag_code, "'", collapse = ", ")
long_covid_query_LPR3 <- paste0("SELECT * FROM D282171.PSD_LPR3_KONTAKTER ",
                                "WHERE AKTIONSDIAGNOSE IN (", long_covid_diag_codes_list, ") ")
long_covid_diagnoses_LPR3 <- dbGetQuery(con3, long_covid_query_LPR3)
# Load required package
setDT(long_covid_diagnoses_LPR3)
setDT(step_6_f)
long_covid_diagnoses_LPR3[, DATO_START := as.Date(DATO_START)]
step_6_f[, first_test_date := as.Date(first_test_date)]

# Step 1: Find relevant PERSON_IDs
relevant_persons_LPR3 <- unique(long_covid_diagnoses_LPR3$PERSON_ID)
relevant_persons <- unique(c(relevant_persons_LPR3))
# Subset step_6_f
step_6_f_relevant_long_covid <- step_6_f[PERSON_ID %in% relevant_persons]
remove_ids <- vector()
# Step 2: Remove based on LPR3 condition
for (person_id in unique(step_6_f_relevant_long_covid$PERSON_ID)) {
  first_test <- step_6_f_relevant_long_covid[PERSON_ID == person_id, first_test_date]
  if (any(long_covid_diagnoses_LPR3[PERSON_ID == person_id, DATO_START < first_test])) {
    remove_ids <- c(remove_ids, person_id)
  }
}
# Step 3: Remove from cohort
long_covid_baseline_cohort <- step_6_f[!PERSON_ID %in% remove_ids]
removed_LPR3 <- length(remove_ids)
cat("Removed", removed_LPR3, "individuals based on long_covid_diagnoses_LPR3.\n")
# Step 4:
length1 <- nrow(long_covid_baseline_cohort)
#Remove rows where the first_test_date is < 2020-04-01, since this is the first date of implementation in Denmark
long_covid_baseline_cohort <- long_covid_baseline_cohort[first_test_date >= as.Date("2020-04-01")]
length2 <- nrow(long_covid_baseline_cohort)
# Save results
saveRDS(long_covid_baseline_cohort, "long_covid_baseline_cohort.rds")

# New code ---------------
library(comorbidity)
library(heaven)
drv <- dbDriver('Oracle')
conn <- DSTora::OraGenvej('', dbuser = '')
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

# Query LPR3KONTAKTER
query_LPR3 <- "SELECT PERSON_ID, AKTIONSDIAGNOSE, DATO_START FROM D222008.LPR3KONTAKTER WHERE EXTRACT(YEAR FROM DATO_START) >= 2010"
diagnoses_LPR3 <- as.data.table(dbGetQuery(conn, query_LPR3))

# Query LPRDIAG_PID
query_LPR2 <- "SELECT PERSON_ID, C_DIAG, AAR FROM D222008.LPRDIAG_PID WHERE AAR >= 2010"
diagnoses_LPR2 <- as.data.table(dbGetQuery(conn, query_LPR2))

# Filter diagnoses to only include those in charlson.codes.extended
diagnoses_LPR3 <- diagnoses_LPR3[AKTIONSDIAGNOSE %in% unlist(charlson.codes.extended)]
diagnoses_LPR2 <- diagnoses_LPR2[C_DIAG %in% unlist(charlson.codes.extended)]

saveRDS(diagnoses_LPR3, file="diagnoses_LPR3.rds")
saveRDS(diagnoses_LPR2, file="diagnoses_LPR2.rds")
#diagnoses_LPR3 <- readRDS("diagnoses_LPR3.rds")
#diagnoses_LPR2 <- readRDS("diagnoses_LPR2.rds")

# Standardize column names and format dates
diagnoses_LPR2[, AKTIONSDIAGNOSE := C_DIAG]
diagnoses_LPR2[, diagnosis_date := as.Date(paste0(AAR, "-01-01"))]
diagnoses_LPR3[, diagnosis_date := as.Date(DATO_START)]

# Combine both datasets
diagnoses_combined <- rbindlist(list(
  diagnoses_LPR3[, .(PERSON_ID, AKTIONSDIAGNOSE, diagnosis_date)],
  diagnoses_LPR2[, .(PERSON_ID, AKTIONSDIAGNOSE, diagnosis_date)]
))

# long COVID Group --------------
diagnoses_filtered_long_covid <- diagnoses_combined[
  PERSON_ID %in% long_covid_baseline_cohort$PERSON_ID & 
    AKTIONSDIAGNOSE %in% unlist(charlson_codes_filtered)
]
long_covid_diagnoses <- diagnoses_filtered_long_covid %>%
  inner_join(long_covid_baseline_cohort %>% select(PERSON_ID, first_test_date), by = "PERSON_ID")

long_covid_charlson_results_5yrs <- charlsonIndex(
  data = long_covid_diagnoses,
  ptid = "PERSON_ID",
  vars = "AKTIONSDIAGNOSE",
  data.date = "diagnosis_date",
  charlson.date = "first_test_date",
  look.back = 5,
  ccodes = charlson.codes.extended
)
long_covid_charlson_results_10yrs <- charlsonIndex(
  data = long_covid_diagnoses,
  ptid = "PERSON_ID",
  vars = "AKTIONSDIAGNOSE",
  data.date = "diagnosis_date",
  charlson.date = "first_test_date",
  look.back = 10,
  ccodes = charlson.codes.extended
)
long_covid_baseline_cohort_with_cci <- long_covid_baseline_cohort %>%
  left_join(long_covid_charlson_results_5yrs[[1]] %>% rename(charlson.index.5yrs = charlson.index), by = c("PERSON_ID", "first_test_date")) %>%
  mutate(charlson.index.5yrs = ifelse(is.na(charlson.index.5yrs), 0, charlson.index.5yrs)) %>%
  left_join(long_covid_charlson_results_10yrs[[1]] %>% rename(charlson.index.10yrs = charlson.index), by = c("PERSON_ID", "first_test_date")) %>%
  mutate(charlson.index.10yrs = ifelse(is.na(charlson.index.10yrs), 0, charlson.index.10yrs))
saveRDS(long_covid_baseline_cohort_with_cci, "long_covid_baseline_cohort_with_cci.rds")