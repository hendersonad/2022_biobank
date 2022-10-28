#' Combine the GP records and UKB cohort data at baseline
#' 

require("here")
require("ggVennDiagram")
source(here::here("file_paths.R"))
source(here::here("functions/fn_twoXtwo.R"))

library(tidyverse)

# load UKB baseline data (compiled in "cohort_build.R" --------------------
ukb_base <- arrow::read_parquet(paste0(datapath, "cohort_data/ukb_baseline.parquet"))

# load GP data for multiple conditions ------------------------------------
eczema_gp <- readRDS(paste0(datapath, "primarycare_data/eczema_extract.rds"))
psoriasis_gp <- readRDS(paste0(datapath, "primarycare_data/psoriasis_extract.rds"))
anxiety_gp <- readRDS(paste0(datapath, "primarycare_data/anxiety_extract.rds"))
depression_gp <- readRDS(paste0(datapath, "primarycare_data/depression_extract.rds"))

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
  filter(f.eid %in% linked_ids$f.eid)
match_ukb_gp <- function(condition = "eczema"){
  gp_data <- get(paste0(condition, "_gp"))
  
  ## cross_match UKB and GP
  data_combine <- ukb_base_linked %>% 
    left_join(gp_data, by = "f.eid") %>% 
    dplyr::select(f.eid, study_entry, year_entry, age_at_recruit, sex, ethnicity, hh_inc, qualifications, age_cat, bmi, mod_exercise, vig_exercise, met_score, alcohol, insomnia, sleep_duration, smoking, socsup_visits,
                  contains(condition), event_dt_gp = event_dt, desc, data_gp) %>% 
    mutate(data_gp = replace_na(data_gp, 0)) %>% 
    mutate(dob = as.Date(paste(year_entry-age_at_recruit, "07", "01", sep = "-"))) %>% 
    mutate(age_gp = as.numeric((event_dt_gp - dob)/365.25)) %>% 
    mutate(gp_pre_recruit = as.numeric(age_gp <= age_at_recruit))
  
  cat("Number of GP records pre-UKB recruitment \n") 
  data_combine$gp_pre_recruit %>% table(useNA = "ifany") %>% print()
  data_combine <- data_combine %>% 
    mutate(gp_pre_recruit = replace_na(gp_pre_recruit, 0)) %>% 
    filter(gp_pre_recruit != 1)
  
  ## DEAL WITH NEGATIVE GP AGEs :(
  cat("number of GP records with negative age (remove these) \n") 
  table(data_combine$age_gp < -1) %>% print()
  data_combine <- data_combine %>% 
    filter(age_gp>-1 | is.na(age_gp))
  
  ### WHY ARE THERE NO PEOPLE WITHOUT ECZEMA == 0 
  twoXtwo(data_combine, eval(condition), "data_gp")
  p1 <- ggplot(data_combine, aes(x = age_at_recruit , y = age_gp, colour = factor(eval(condition)))) +
    geom_point(alpha = 0.1)
  print(p1)
  p2 <- ggplot(data_combine, aes(x = age_gp, fill = factor(eval(condition)))) +
    geom_density(alpha = 0.4)  
  print(p2)
  data_combine
}

eczema_combine <- match_ukb_gp("eczema")
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
p1 <- analysis_fn(eczema_combine, "eczema")
p2 <- analysis_fn(psoriasis_combine, "psoriasis")
p3 <- analysis_fn(anxiety_combine, "anxiety")
p4 <- analysis_fn(depression_combine, "depression")

pdf(here::here("out/venn_diagrams.pdf"), 8,8)
  cowplot::plot_grid(p1, p2, p3, p4, labels = "AUTO")
dev.off()
