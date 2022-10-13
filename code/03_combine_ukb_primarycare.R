#' Combine the GP records and UKB cohort data at baseline
#' 

require("here")
source(here::here("file_paths.R"))
source(here::here("functions/fn_twoXtwo.R"))

library(tidyverse)

# load UKB baseline data (compiled in "cohort_build.R" --------------------
ukb_base <- arrow::read_parquet(paste0(datapath, "cohort_data/ukb_baseline.parquet"))

# load GP data for multiple conditions ------------------------------------
eczema_gp <- readRDS("eczema_test.rds")

# load list of IDs with linked data ---------------------------------------
linked_ids <- readr::read_table(paste0(datapath, "cohort_data/linkage_ids.txt"))

# subset UKb on just those with linkage -----------------------------------
ukb_base_linked <- ukb_base %>% 
  filter(f.eid %in% linked_ids$f.eid)

ukb_base_linked %>% glimpse()
## cross_match UKB and GP
eczema_combined <- ukb_base_linked %>% 
  left_join(eczema_gp, by = "f.eid") %>% 
  dplyr::select(f.eid, study_entry, year_entry, age_at_recruit, sex, ethnicity, hh_inc, qualifications, age_cat, bmi, mod_exercise, vig_exercise, met_score, alcohol, insomnia, sleep_duration, smoking, socsup_visits, 
                age_eczema, eczema, age_hes_eczema, hes_eczema,  event_dt_gp = event_dt, desc, eczema_gp) %>% 
  mutate(eczema_gp = replace_na(eczema_gp, 0)) %>% 
  mutate(dob = as.Date(paste(year_entry-age_at_recruit, "07", "01", sep = "-"))) %>% 
  mutate(age_eczema_gp = as.numeric((event_dt_gp - dob)/365.25))

## DEAL WITH NEGATIVE GP AGEs :(
table(eczema_combined$age_eczema_gp < -1)
eczema_combined <- eczema_combined %>% 
  filter(age_eczema_gp>-1 | is.na(age_eczema_gp))

twoXtwo(eczema_combined, "eczema", "eczema_gp")

ggplot(eczema_combined, aes(x = age_at_recruit , y = age_eczema_gp, colour = factor(eczema))) +
  geom_point()

ggplot(eczema_combined, aes(x = age_eczema_gp, fill = factor(eczema))) +
  geom_density(alpha = 0.4)

twoXtwo(ukb_base, "eczema", "insomnia")
