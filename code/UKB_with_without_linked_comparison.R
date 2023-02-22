library(arrow)
library(tidyverse)
library(skimr)
library(gtsummary)
baseline_assessment_cohort <- read_parquet(paste0(datapath, "cohort_data/ukb_baseline.parquet"))
ukb_gp_combined <- read_parquet(paste0(datapath, "cohort_data/ukb_gp_linked.parquet"))

baseline_assessment_cohort %>% skim()


a <- baseline_assessment_cohort %>% 
  select(sex, ethnicity, emplyment, age_at_recruit, deprivation, year_entry, phq9_interest, gad7_irritability, eczema, psoriasis, anxiety, depression) %>% 
  tbl_summary()

b <- baseline_assessment_cohort %>% 
  filter(!f.eid %in% ukb_gp_combined$f.eid) %>% 
  select(sex, ethnicity, emplyment, age_at_recruit, deprivation, year_entry, phq9_interest, gad7_irritability, eczema, psoriasis, anxiety, depression) %>% 
  tbl_summary()

c <- ukb_gp_combined %>% 
  filter(f.eid %in% ukb_gp_combined$f.eid) %>% 
  select(sex, ethnicity, emplyment, age_at_recruit, deprivation, year_entry, phq9_interest, gad7_irritability, eczema, psoriasis, anxiety, depression) %>% 
  tbl_summary()

d <- ukb_gp_combined %>% 
  filter(is.na(phq9_interest)) %>% 
  select(sex, ethnicity, emplyment, age_at_recruit, deprivation, year_entry, phq9_interest, gad7_irritability, eczema, psoriasis, anxiety, depression) %>% 
  tbl_summary()

e <- ukb_gp_combined %>% 
  filter(!is.na(phq9_interest)) %>% 
  select(sex, ethnicity, emplyment, age_at_recruit, deprivation, year_entry, phq9_interest, gad7_irritability, eczema, psoriasis, anxiety, depression) %>% 
  tbl_summary()

merged <- tbl_merge(list(a,b,c,d,e))
merged

# Mental health survey respondents vs non-respondents
a <- baseline_assessment_cohort %>% 
  select(sex, ethnicity, emplyment, age_at_recruit, deprivation, year_entry, phq9_interest, gad7_irritability, eczema, psoriasis, anxiety, depression) %>% 
  tbl_summary()

b <- baseline_assessment_cohort %>% 
  filter(f.eid %in% ukb_gp_combined$f.eid) %>% 
  select(sex, ethnicity, emplyment, age_at_recruit, deprivation, year_entry, phq9_interest, gad7_irritability, eczema, psoriasis, anxiety, depression) %>% 
  tbl_summary()

c <- baseline_assessment_cohort %>% 
  filter(!f.eid %in% ukb_gp_combined$f.eid) %>% 
  select(sex, ethnicity, emplyment, age_at_recruit, deprivation, year_entry, phq9_interest, gad7_irritability, eczema, psoriasis, anxiety, depression) %>% 
  tbl_summary()

merged <- tbl_merge(list(a,b,c))
merged
