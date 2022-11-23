#' Combine the GP records and UKB cohort data at baseline
#' 

require("here")
require("ggVennDiagram")
source(here::here("file_paths.R"))
source(here::here("functions/fn_twoXtwo.R"))

library(janitor)
library(tidyverse)

# load UKB baseline data (compiled in "cohort_build.R" --------------------
ukb_base <- arrow::read_parquet(paste0(datapath, "cohort_data/ukb_baseline.parquet"))

# load GP data for multiple conditions ------------------------------------
eczema_gp <- readRDS(paste0(datapath, "primarycare_data/eczema_extract.rds"))
psoriasis_gp <- readRDS(paste0(datapath, "primarycare_data/psoriasis_extract.rds"))
anxiety_gp <- readRDS(paste0(datapath, "primarycare_data/anxiety_extract.rds"))
depression_gp <- readRDS(paste0(datapath, "primarycare_data/depression_extract.rds"))

# load eczema treatments and use CPRD eczema definition -------------------
eczema_trt_gp <- readRDS(paste0(datapath, "primarycare_data/all_eczema_Rx_in_ukb.rds"))
names(eczema_trt_gp)[names(eczema_trt_gp)=="event_dt"] <- "issue_date"

## combine trt data with the diagnosis code
## filter out treatments that precede diagnosis
## then count the number of incidents per patid and filter if don't have 2 treatments 
eczema_alg_gp <- eczema_gp %>% 
  left_join(eczema_trt_gp, by = "f.eid") %>%
  group_by(f.eid) %>%
  mutate(event_record = 1:n()) %>% 
  filter(event_record == 2) %>% 
  ungroup() %>% 
  mutate(data_gp = 1,
         eczema_date = NA) %>% 
  mutate_at("eczema_date", ~pmax(event_dt, issue_date)) %>% 
  dplyr::select(f.eid, event_dt = eczema_date, data_gp, desc = desc.x, trt2nd = desc.y)
ukb_dob <- ukb_base %>% 
  dplyr::select(f.eid, year_entry, age_at_recruit) %>% 
  mutate(dob = as.Date(paste(year_entry-age_at_recruit, "07", "01", sep = "-"))) 


# load list of IDs with linked data ---------------------------------------
if(file.exists(paste0(datapath, "cohort_data/linkage_ids.txt"))==FALSE){
  gp_clinical <- arrow::read_parquet(file = paste0(datapath, "primarycare_data/gp_clinical.parquet"))
  linked_ids <- unique(gp_clinical$eid)
  readr::write_delim(data.frame(f.eid = linked_ids), file = paste0(datapath, "cohort_data/linkage_ids.txt"))
}else{
  linked_ids <- readr::read_delim(paste0(datapath, "cohort_data/linkage_ids.txt"), delim = "\t")
}

# subset UKb on just those with linkage -----------------------------------
ukb_base_linked <- ukb_base %>% 
  filter(f.eid %in% linked_ids$f.eid) %>% 
  mutate(eczema_alg = eczema)

match_ukb_gp <- function(condition = "eczema"){
  gp_data <- get(paste0(condition, "_gp"))
  
  ## cross_match UKB and GP
  data_combine <- ukb_base_linked %>% 
    left_join(gp_data, by = "f.eid") %>% 
    dplyr::select(f.eid, study_entry, year_entry, age_at_recruit, sex, ethnicity, hh_inc, qualifications, age_cat, bmi, mod_exercise, vig_exercise, met_score, alcohol, insomnia, sleep_duration, smoking, socsup_visits,
                  date_mh_survey, ever_anxious_worried, ever_depressed_sad, ever_soughthelp, happy,
                  contains(condition), event_dt_gp = event_dt, desc, data_gp) %>% 
    mutate(dob = as.Date(paste(year_entry-age_at_recruit, "07", "01", sep = "-"))) %>% 
    mutate(age_gp = as.numeric((event_dt_gp - dob)/365.25)) %>% 
    mutate(gp_pre_recruit = as.numeric(age_gp <= age_at_assess)) %>% 
    mutate(gp_pre_questionnaire = as.numeric(event_dt_gp <= date_mh_survey))
  
  cat("Number of GP records pre-UKB recruitment \n") 
  data_combine$gp_pre_recruit %>% table(useNA = "ifany") %>% print()
  cat("Number of GP records pre-MH survey \n") 
  data_combine$gp_pre_questionnaire %>% table(useNA = "ifany") %>% print()
  
  data_combine <- data_combine %>% 
    mutate(data_gp = ifelse(gp_pre_recruit == 0, 0, data_gp)) %>% 
    mutate(data_gp = replace_na(data_gp, 0)) %>% 
    mutate(data_gp2016 = ifelse(gp_pre_questionnaire == 0, 0, data_gp)) %>% 
    mutate(data_gp2016 = replace_na(data_gp2016, 0))
  
  ## DEAL WITH NEGATIVE GP AGEs :(
  cat("number of GP records with negative age (remove these) \n") 
  table(data_combine$age_gp < -1) %>% print()
  data_combine <- data_combine %>% 
    filter(age_gp>-1 | is.na(age_gp))
  
  ### WHY ARE THERE NO PEOPLE WITHOUT ECZEMA == 0 
  twoXtwo(data_combine, eval(condition), "data_gp")
  p1 <- ggplot(data_combine, aes(x = age_at_recruit , y = age_gp, colour = factor(eval(condition)))) +
    labs(x = "Age at recruitment", y = "Age of GP record", colour = glue::glue("{condition}")) +
    geom_point(alpha = 0.1, size = 1, shape = 2) + 
    theme(legend.position = "none")
  p2 <- ggplot(data_combine, aes(x = age_gp, fill = factor(eval(condition)))) +
    labs(x = "Age of GP record", colour = glue::glue("{condition}")) +
    geom_density(alpha = 0.4) 
  cowplot::plot_grid(p1, p2, ncol = 2)
  return(list(data = data_combine, p1=p1, p2=p2))
}

eczema_combine <- match_ukb_gp("eczema")
eczema_alg_combine <- match_ukb_gp("eczema_alg")
psoriasis_combine <- match_ukb_gp("psoriasis")
anxiety_combine <- match_ukb_gp("anxiety")
depression_combine <- match_ukb_gp("depression")

  
analysis_fn <- function(dataset, interviewvar){
  twoXtwo(dataset, interviewvar, "data_gp")
  x = list(
    dataset$f.eid[dataset[,interviewvar]==1],
    dataset$f.eid[dataset$data_gp == 1]
    )
  
  ggVennDiagram(x, category.names = c("Interview", "Linked GP data")) +
    scale_color_brewer(palette = "Paired") +
    labs(fill = stringr::str_to_title(interviewvar)) +
    theme(legend.position = "left")
}

p1 <- analysis_fn(eczema_combine$data, "eczema")
p2 <- analysis_fn(psoriasis_combine$data, "psoriasis")
p3 <- analysis_fn(anxiety_combine$data, "anxiety")
p4 <- analysis_fn(depression_combine$data, "depression")
p5 <- analysis_fn(eczema_alg_combine$data, "eczema_alg")

pdf(here::here("out/venn_diagrams.pdf"), 8,8)
  cowplot::plot_grid(p1, p5, p2, p3, p4, labels = "AUTO", ncol = 2)
dev.off()


# histograms of diagnoses --------------------------------------------------
pdf(here::here("out/linked_gp_dates.pdf"), 8,8)
  cowplot::plot_grid(
    eczema_combine$p1, eczema_combine$p2,
    psoriasis_combine$p1, psoriasis_combine$p2,
    anxiety_combine$p1, anxiety_combine$p2,
    depression_combine$p1, depression_combine$p2,
    eczema_alg_combine$p1, eczema_alg_combine$p2,
    ncol = 2,
    labels = "auto"
  )
dev.off()

# combine the merged datasets onto the baseline data  ---------------------
# And export

ukb_gp_combined <- ukb_base_linked %>% 
  left_join(dplyr::select(eczema_combine$data, f.eid, eczema_gp = data_gp, eczema_dt_gp = event_dt_gp, eczema_gp_pre16 = data_gp2016), by = "f.eid") %>% 
  left_join(dplyr::select(psoriasis_combine$data, f.eid, psoriasis_gp = data_gp, psoriasis_dt_gp = event_dt_gp, psoriasis_gp_pre16 = data_gp2016), by = "f.eid") %>% 
  left_join(dplyr::select(anxiety_combine$data, f.eid, anxiety_gp = data_gp, anxiety_dt_gp = event_dt_gp, anxiety_gp_pre16 = data_gp2016), by = "f.eid") %>% 
  left_join(dplyr::select(depression_combine$data, f.eid, depression_gp = data_gp, depression_dt_gp = event_dt_gp, depression_gp_pre16 = data_gp2016), by = "f.eid") %>% 
  left_join(dplyr::select(eczema_alg_combine$data, f.eid, eczema_alg_gp = data_gp, eczema_alg_dt_gp = event_dt_gp, eczema_alg_gp_pre16 = data_gp2016), by = "f.eid") 


arrow::write_parquet(ukb_gp_combined, sink = paste0(datapath, "cohort_data/ukb_gp_linked.parquet"))

