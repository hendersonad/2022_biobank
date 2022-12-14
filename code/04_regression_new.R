
# Prepare -----------------------------------------------------------------

library(tidyverse)
library(here)
library(arrow)
library(glue)

source(here::here("file_paths.R"))

ukb_gp <- arrow::read_parquet(file = paste0(datapath, "cohort_data/ukb_gp_linked.parquet"))

ukb_gp$ethnicity2 <- as.character(ukb_gp$ethnicity) %>% stringr::str_to_lower() %>% factor()
levels(ukb_gp$ethnicity2) <- c("black/black british/caribbean", "asian/asian british", "black/black british/caribbean", "mixed/other", "white/british/irish", "asian/asian british", "asian/asian british", 
                               "black/black british/caribbean", "white/british/irish", "black/black british/caribbean", "asian/asian british", "na/no answer", "asian/asian british", "white/british/irish",
                               "mixed/other", "mixed/other", "asian/asian british", "na/no answer", "white/british/irish", "mixed/other", "mixed/other", "mixed/other")


ukb_gp$eczema_union <- (as.numeric(ukb_gp$eczema)-1) + as.numeric(ukb_gp$eczema_gp)
ukb_gp$eczema_intersect <- ifelse(ukb_gp$eczema_union==2, 1, 0)
ukb_gp$eczema_union[ukb_gp$eczema_union==2] <- 1

ukb_gp$eczema_pre16_union <- (as.numeric(ukb_gp$eczema)-1) + as.numeric(ukb_gp$eczema_gp_pre16)
ukb_gp$eczema_pre16_intersect <- ifelse(ukb_gp$eczema_pre16_union==2, 1, 0)
ukb_gp$eczema_pre16_union[ukb_gp$eczema_pre16_union==2] <- 1

ukb_gp$eczema_alg_union <- (as.numeric(ukb_gp$eczema_alg)-1) + as.numeric(ukb_gp$eczema_alg_gp)
ukb_gp$eczema_alg_intersect <- ifelse(ukb_gp$eczema_alg_union==2, 1, 0)
ukb_gp$eczema_alg_union[ukb_gp$eczema_alg_union==2] <- 1

ukb_gp$eczema_alg_pre16_union <- (as.numeric(ukb_gp$eczema_alg)-1) + as.numeric(ukb_gp$eczema_alg_gp_pre16)
ukb_gp$eczema_alg_pre16_intersect <- ifelse(ukb_gp$eczema_alg_pre16_union==2, 1, 0)
ukb_gp$eczema_alg_pre16_union[ukb_gp$eczema_alg_pre16_union==2] <- 1

ukb_gp$psoriasis_union <- (as.numeric(ukb_gp$psoriasis)-1) + as.numeric(ukb_gp$psoriasis_gp)
ukb_gp$psoriasis_intersect <- ifelse(ukb_gp$psoriasis_union==2, 1, 0)
ukb_gp$psoriasis_union[ukb_gp$psoriasis_union==2] <- 1

ukb_gp$psoriasis_pre16_union <- (as.numeric(ukb_gp$psoriasis)-1) + as.numeric(ukb_gp$psoriasis_gp_pre16)
ukb_gp$psoriasis_pre16_intersect <- ifelse(ukb_gp$psoriasis_pre16_union==2, 1, 0)
ukb_gp$psoriasis_pre16_union[ukb_gp$psoriasis_pre16_union==2] <- 1

ukb_gp$depression_union <- (as.numeric(ukb_gp$depression)-1) + as.numeric(ukb_gp$depression_gp)
ukb_gp$depression_intersect <- ifelse(ukb_gp$depression_union==2, 1, 0)
ukb_gp$depression_union[ukb_gp$depression_union==2] <- 1

ukb_gp$anxiety_union <- (as.numeric(ukb_gp$anxiety)-1) + as.numeric(ukb_gp$anxiety_gp)
ukb_gp$anxiety_intersect <- ifelse(ukb_gp$anxiety_union==2, 1, 0)
ukb_gp$anxiety_union[ukb_gp$anxiety_union==2] <- 1






# Run regression ----------------------------------------------------------

#All combinations of exposures and outcomes
comb1 <- expand_grid(
  exposure=c("eczema", "eczema_union", "eczema_alg", "eczema_alg_gp", "eczema_alg_union",
             "psoriasis", "psoriasis_union"),
  outcome=c("depression","depression_gp", 
            "anxiety", "anxiety_gp")
)

comb2 <- expand_grid(
  exposure=c("eczema_alg","eczema_alg_gp_pre16", "eczema_alg_union", "eczema_alg_gp_pre16"),
  outcome=c("ever_anxious_worried","ever_depressed_sad", "depression_gp_pre16", "anxiety_gp_pre16")
)

#Funciton to run regressions
runreg <- function(.outcome, .exposure) {
  glm(glue("{.outcome} ~ {.exposure} + age_at_assess + sex + deprivation + ethnicity2"),
      data = ukb_gp_sample , family = "binomial") %>% 
    broom::tidy(exponentiate = T, conf.int = T) %>% 
    mutate(exposure=.exposure, outcome=.outcome) %>% 
    filter(str_detect(term, eval(.exposure))) 
}

#Map regression function to all combinations of exposure and outcome
results_regression <- map2_df(grid$outcome, grid$exposure, runreg)
write_csv(results_mhealthsurvey, "out/results_mhealthsurvey.csv")