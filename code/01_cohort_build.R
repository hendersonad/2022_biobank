library(tidyverse)
library(data.table)
library(ukbtools)
library(lubridate)
library(glue)
library(irr)
library(skimr)
library(cowplot)
library(gt)
library(gtsummary)

dir.create(path = here::here("out"), showWarnings = F)
dir.create(path = here::here("out/tables"), showWarnings = F)
if (Sys.info()["user"] == "lsh1510922") {
 if (Sys.info()["sysname"] == "Darwin") {
   datapath <- "/Volumes/EHR Group/GPRD_GOLD/Ali/2022_biobank/"
 }
 if (Sys.info()["sysname"] == "Windows") {
   datapath <- "Z:/GPRD_GOLD/Ali/2022_biobank/"
 }
} else {
  datapath <- "Z:/GPRD_GOLD/Ali/2022_biobank/"
}
source(here::here("functions/fn_twoXtwo.R"))
               
# read in data and codelists ----------------------------------------------
ukb <- read_rds(paste0(datapath, "ukb669156.rds"))
my_ukb_key <- ukb_df_field("ukb669156", path = datapath)

anxiety_codelist <- read.delim(here::here("codelist/eczema_psych_anxiety_HES.txt"), sep = ",")
depression_codelist <- read.delim(here::here("codelist/eczema_psych_depression_HES.txt"), sep = ",")
eczema_codelist <- read.delim(here::here("codelist/eczema_psych_eczemaDx_HES.txt"), sep = ",")
eczema_codelist$ICD10codealternative <- str_remove_all(eczema_codelist$ICD10code, "\\.")
psoriasis_codelist <- read.delim(here::here("codelist/psoriasis_icd.txt"), sep = ",")
psoriasis_codelist$ICD10codealternative <- str_to_upper(str_remove_all(psoriasis_codelist$icd, "\\."))
smi_codelist <- read.delim(here::here("codelist/ICD10codes-SMI.txt"), sep = ",")
  names(smi_codelist)[2] <- "ICD10codealternative"

# descriptive data --------------------------------------------------------
ukb_descriptive <- ukb %>% 
  select(f.eid, 
         age_at_recruit = f.21022.0.0, 
         age_at_assess = f.21003.0.0,
         sex = f.31.0.0,
         deprivation = f.189.0.0,
         study_entry = f.53.0.0,
         ethnicity = f.21000.0.0,
         hh_inc = f.738.0.0,
         emplyment = f.6142.0.0,
         qualifications = f.6138.0.0
  ) %>% 
  mutate(age_cat = cut_number(age_at_assess, n = 6))


# baseline assessment -----------------------------------------------------
get_diagnoses_baseline <- function(field_no = "f.20002", vartype = "", diagnosis_code = NA, diagnosis = ""){
  if(vartype == ""){stop("Specify whether vartype is cat, num or txt (categorical, numeric or text")}
  if(vartype == "cat" & all(is.na(diagnosis_code))){stop("Categorical var but no diagnosis_code to look for")}
  
  # get field at first assessment - the ".0" ensures it is the first visit and therefore baseline
  expr0 <- glue(field_no, ".0")
  
  # get field and split into all the arrays (allowing for multiple codes per f.eid)
  baseline <- ukb %>% 
    select(f.eid, contains(expr0)) %>% 
    pivot_longer(cols = contains(expr0), values_to = "field_code", 
                 names_to = c("instance", "array"),
                 names_pattern = glue(field_no, ".(.).(.*)")) 
  
  # if categorical then look to see if the code in each array matches any in diagnosis_code and return [0, 1]
  if(vartype == "cat"){
    baseline <- baseline %>% 
      mutate("{diagnosis}" := as.numeric(field_code %in% diagnosis_code)) 
  }else if(vartype != "cat"){ #or just return the value in that field 
    baseline <- baseline %>% 
      mutate("{diagnosis}" := field_code) 
  }
  
  # if looking at non-cancer illness assessment field (f.20002) then also get the age of any diagnosis
  if(str_detect(field_no,"f.20002")) {
    # age at diagnosis is stored in f.87
    dates <- ukb %>% 
      select(f.eid, starts_with("f.87.0"), starts_with("f.20002.0")) %>% 
      pivot_longer(cols = c(starts_with("f.87.0"), starts_with("f.20002.0")), 
                   names_to = c(".value", "array"),
                   names_pattern = "(.*).0.(.*)") %>% 
      rename("date" = "f.87", "field_code" = "f.20002")
    # f.34.0.0 and f.52.0.0 give month and year of birth 
    # f.87 codes as EITHER the age at diagnosis or the year of diagnosis (for some reason)
    #so if it is >1000 assume it's a year and calculate the age of diagnosis
    dates_dob <- dates %>% 
      drop_na() %>% 
      filter(field_code %in% diagnosis_code) %>% 
      left_join(dplyr::select(ukb, f.eid, yob = f.34.0.0, mob = f.52.0.0), by = "f.eid") %>% 
      mutate(dob = lubridate::ymd(paste0(yob, as.character(mob), "01")),
             adultyear = lubridate::year(dob+(18*365.25)),
             age_diagnosis = ifelse(date<1000, date, date-yob)) %>% 
      select(f.eid, array, field_code, age_diagnosis)
    
    ## convert to data.table for speeeeeed
    setDT(baseline)
    setDT(dates_dob)
    # merge baseline diagnoses with date of diagnosis for any codes in diagnosis_code
    baseline[dates_dob, on = c('f.eid', 'field_code', 'array'), age_diagnosis := i.age_diagnosis]
    
    # get max(diagnosis) i.e. a 1 or 0
    dt_1row_diag <- baseline[, lapply(.SD, max), .SDcols = c(eval(diagnosis)), by = f.eid]
    # get min(age_diagnosis) i.e. first age they got a diagnosis (if any)
    dt_1row_age <- baseline[baseline[, .I[which.min(age_diagnosis)], f.eid]$V1, c("f.eid", "age_diagnosis")]
    # left_join the diagnoses data with the age data (1 row per f.eid)
    dt_1row <- dt_1row_age[dt_1row_diag, on = "f.eid"]
    setnames(dt_1row, "age_diagnosis", glue("age_", diagnosis))
    
    # export as tibble
    dt_1row %>% as_tibble()
  }else{
    # in an easier land of mutually exclusive categories or numeric variables, just get the max value per f.eid and return them 
    dt_ukb_diagnosis <- setDT(baseline)
    dt_1row <- dt_ukb_diagnosis[, lapply(.SD, max), .SDcols = eval(diagnosis), by = f.eid]
    dt_1row %>% as_tibble()
  }
}

## Diagnoses in initial assessment (these f.20002 fields will be a bit slow because of the age_diagnosis work)
eczema <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = 1452, diagnosis = "eczema")
psoriasis <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = 1453, diagnosis = "psoriasis")
anxiety <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = 1287, diagnosis = "anxiety")
depression <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = 1286, diagnosis = "depression")
smi <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = c(1289,1291,1290), diagnosis = "smi")
asthma <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = 1111, diagnosis = "asthma")
alcohol <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = c(1604,1408), diagnosis = "alcohol") # "alcoholic liver disease / alcoholic cirrhosis"	or "alcohol dependency"
insomnia <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = c(1616,1123), diagnosis = "insomnia") # "insomnia" or "sleep apnoeia"
psoarth <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = 1477, diagnosis = "psoarth") 
hayfever <- get_diagnoses_baseline(field_no = "f.20002", vartype = "cat", diagnosis_code = 1387, diagnosis = "hayfever_allergicrhinitis") 

# supplement data with touchscreen ----------------------------------------
insomnia_touch <- get_diagnoses_baseline(field_no = "f.1200", vartype = "txt", diagnosis = "insomnia_touchscreen") 
alcohol_touch <- get_diagnoses_baseline(field_no = "f.1558", vartype = "txt", diagnosis = "alcohol_touchscreen") 
allergic_touch <- get_diagnoses_baseline(field_no = "f.6152", vartype = "cat", diagnosis_code = "Hayfever, allergic rhinitis or eczema", diagnosis = "allergic_touchscreen") 

sleep_duration <- get_diagnoses_baseline(field_no = "f.1160", vartype = "num", diagnosis = "sleep_duration") 
smoking <- get_diagnoses_baseline(field_no = "f.20116", vartype = "txt", diagnosis = "smoking") 
mod_exercise <- get_diagnoses_baseline(field_no = "f.884", vartype = "num", diagnosis = "mod_exercise")
vig_exercise <- get_diagnoses_baseline(field_no = "f.904", vartype = "num", diagnosis = "vig_exercise")
met_score <- get_diagnoses_baseline(field_no = "f.22035", vartype = "num", diagnosis = "met_score")

socsupport_visits <- get_diagnoses_baseline(field_no = "f.1031", vartype = "txt", diagnosis = "socsup_visits")
socsupport_activity <- get_diagnoses_baseline(field_no = "f.6160", vartype = "cat", diagnosis_code = c("None of the above", "Prefer not to answer"), diagnosis = "socsup_activity")
  socsupport_activity$socsup_activity <- 1 - socsupport_activity$socsup_activity # reverse the ordering so 0 = "None of the above" or "Prefer not to answer" and 1 = "any activity"

bmi <- get_diagnoses_baseline(field_no = "f.23104", vartype = "num", diagnosis = "bmi")

healthrating <- get_diagnoses_baseline(field_no = "f.2178", vartype = "txt", diagnosis = "healthrating")

baseline_assessment_cohort <- as_tibble(ukb_descriptive) %>% 
  left_join(eczema, by = "f.eid") %>% 
  left_join(psoriasis, by = "f.eid") %>% 
  left_join(anxiety, by = "f.eid") %>% 
  left_join(depression, by = "f.eid") %>% 
  left_join(smi, by = "f.eid") %>% 
  left_join(asthma, by = "f.eid") %>% 
  left_join(psoarth, by = "f.eid") %>% 
  left_join(hayfever, by = "f.eid") %>% 
  left_join(allergic_touch, by = "f.eid") %>% 
  left_join(bmi, by = "f.eid") %>% 
  left_join(mod_exercise, by = "f.eid") %>% 
  left_join(vig_exercise, by = "f.eid") %>% 
  left_join(met_score, by = "f.eid") %>% 
  left_join(alcohol, by = "f.eid") %>% 
  left_join(alcohol_touch, by = "f.eid") %>% 
  left_join(insomnia, by = "f.eid") %>% 
  left_join(insomnia_touch, by = "f.eid") %>% 
  left_join(sleep_duration, by = "f.eid") %>% 
  left_join(smoking, by = "f.eid") %>% 
  left_join(socsupport_activity, by = "f.eid") %>% 
  left_join(socsupport_visits, by = "f.eid")  %>% 
  left_join(healthrating, by = "f.eid") 

twoXtwo(baseline_assessment_cohort, "eczema", "insomnia")
twoXtwo(baseline_assessment_cohort, "eczema", "insomnia_touchscreen")
twoXtwo(baseline_assessment_cohort, "allergic_touchscreen", "insomnia_touchscreen")

twoXtwo(baseline_assessment_cohort, "age_cat", "alcohol")
twoXtwo(baseline_assessment_cohort, "age_cat", "alcohol_touchscreen")

twoXtwo(baseline_assessment_cohort, "eczema", "smoking")
twoXtwo(baseline_assessment_cohort, "eczema", "allergic_touchscreen") # !!!! WHAT? there are 3,000 with an eczema diagnosis at interview who did not tick the touchscreen questiojn - maybe misread it? 
twoXtwo(baseline_assessment_cohort, "hayfever_allergicrhinitis", "allergic_touchscreen") 

baseline_assessment_cohort %>% 
  group_by(eczema) %>% 
  drop_na() %>% 
  summarise(min(sleep_duration), quantile(sleep_duration, 0.25), mean(sleep_duration), quantile(sleep_duration, 0.75), max(sleep_duration))

ggplot(baseline_assessment_cohort, aes(x = bmi, group = factor(eczema), fill = factor(eczema))) +
  geom_density(alpha = 0.5)
ggplot(baseline_assessment_cohort, aes(x = bmi, group = factor(psoriasis), fill = factor(psoriasis))) +
  geom_density(alpha = 0.5)

## convert some to factor
baseline_assessment_cohort <- baseline_assessment_cohort %>% 
  mutate_at(c("eczema", "psoriasis", "anxiety", "depression", "smi", "mod_exercise", "vig_exercise", "alcohol", "insomnia", "psoarth", "asthma", "hayfever_allergicrhinitis", "socsup_activity"), ~as.factor(.))

baseline_assessment_cohort <- baseline_assessment_cohort %>% 
  mutate(year_entry = year(study_entry))

# supplement data with HES ------------------------------------------------
hes <- ukb %>% 
  select(f.eid, study_entry = f.53.0.0, starts_with("f.41270.0"), starts_with("f.41280.0")) %>% 
  pivot_longer(cols = c(starts_with("f.41270.0"), starts_with("f.41280.0")), 
               names_to = c(".value", "array"),
               names_pattern = "f.412(.*)0.0.(.*)") %>% 
  rename("icd10" = `7`, "diag_date" = `8`) %>% 
  drop_na()

setDT(hes)

get_hes_baseline <- function(codelist = NA, diagnosis = "anxiety"){
  searchcodes <- codelist %>% dplyr::select(ICD10codealternative) %>% pull()
  dates_dob_hes <- hes %>% 
    filter(diag_date <= study_entry) %>% 
    filter(icd10 %in% searchcodes) %>% 
    left_join(dplyr::select(ukb, f.eid, yob = f.34.0.0, mob = f.52.0.0), by = "f.eid") %>% 
    mutate(dob = lubridate::ymd(paste0(yob, as.character(mob), "01")),
           age_diagnosis = as.numeric((diag_date - dob)/365.25)) %>% 
    dplyr::select(f.eid, array, icd10, age_diagnosis)
  
  ## convert to data.table for speeeeeed
  dtdiagnosis <- hes
  setDT(dates_dob_hes)
  # merge baseline diagnoses with date of diagnosis for any codes in diagnosis_code
  dtdiagnosis <- merge.data.table(hes, dates_dob_hes, by = c('f.eid', 'icd10', 'array')) 
  dtdiagnosis[, eval(diagnosis) := 1]
  
  # get max(diagnosis) i.e. a 1 or 0
  hesdt_1row_diag <- dtdiagnosis[, lapply(.SD, max), .SDcols = c(eval(diagnosis)), by = f.eid]
  # get min(age_diagnosis) i.e. first age they got a diagnosis (if any)
  hesdt_1row_age <- dtdiagnosis[dtdiagnosis[, .I[which.min(age_diagnosis)], f.eid]$V1, c("f.eid", "age_diagnosis")]
  # left_join the diagnoses data with the age data (1 row per f.eid)
  hesdt_1row <- hesdt_1row_age[hesdt_1row_diag, on = "f.eid"]
  setnames(hesdt_1row, "age_diagnosis", glue("age_", diagnosis))
  
  # export as tibble
  hesdt_1row %>% as_tibble()
}

hes_anxiety <- get_hes_baseline(codelist = anxiety_codelist, diagnosis = "hes_anxiety")
hes_depression <- get_hes_baseline(codelist = depression_codelist, diagnosis = "hes_depression")
hes_eczema <- get_hes_baseline(codelist = eczema_codelist, diagnosis = "hes_eczema")
hes_psoriasis <- get_hes_baseline(codelist = psoriasis_codelist, diagnosis = "hes_psoriasis")
hes_smi <- get_hes_baseline(codelist = smi_codelist, diagnosis = "hes_smi")


baseline_assessment_cohort <- baseline_assessment_cohort %>% 
  left_join(hes_anxiety, by = "f.eid") %>% 
  left_join(hes_depression, by = "f.eid") %>% 
  left_join(hes_eczema, by = "f.eid") %>% 
  left_join(hes_psoriasis, by = "f.eid") %>% 
  left_join(hes_smi, by = "f.eid") 

baseline_assessment_cohort <- baseline_assessment_cohort %>% 
  mutate_at(c("hes_eczema", "hes_psoriasis", "hes_anxiety", "hes_depression", "hes_smi"), ~as.factor(tidyr::replace_na(., replace = 0))) 

# treatments --------------------------------------------------------------
# trt <- ukb %>% 
#   select(f.eid, starts_with("f.20003.0")) %>% 
#   pivot_longer(cols = c(starts_with("f.20003.0")), 
#                names_to = c("instance", "array"),
#                names_pattern = "f.20003.(.).(.*)")
# ukb_insomnia <- read_csv(here::here("codelist/ukb_insomnia.csv"))
# trt_insomnia <- trt %>% 
#   filter(value %in% ukb_insomnia$coding)
# sleep_test <- baseline_assessment_cohort %>% 
#   left_join(trt_insomnia, by = "f.eid")
# sleep_test$sleep_test <- as.numeric(!is.na(sleep_test$value))
# 
# twoXtwo(sleep_test, "eczema", "sleep_test")

# get mental health survey questionnaire ----------------------------------
ukb_mhealth <- ukb %>% 
  select(f.eid, 
         date_mh_survey = f.20400.0.0, 
         ever_anxious_worried = f.20421.0.0,
         ever_depressed_sad = f.20446.0.0,
         ever_soughthelp = f.20499.0.0,
         happy = f.20458.0.0
  ) %>% 
  filter(!is.na(date_mh_survey))
ukb_mhealth %>% dim

baseline_assessment_cohort <- baseline_assessment_cohort %>% 
  left_join(ukb_mhealth, by = "f.eid")

# save baseline_assessment_cohort -----------------------------------------
arrow::write_parquet(baseline_assessment_cohort, sink = paste0(datapath, "cohort_data/ukb_baseline.parquet"))
x <- data.frame(varnames = names(baseline_assessment_cohort))
write_csv(x, here::here("ukb_baseline.parquet_definition.csv"))
