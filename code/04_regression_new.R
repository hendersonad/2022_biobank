# Replaces 04_regression and 05_analyse_mhealthsurvey
# Prepare -----------------------------------------------------------------

library(tidyverse)
library(here)
library(arrow)
library(glue)
library(gtsummary)

source(here::here("file_paths.R"))
source(here::here("functions/fn_twoXtwo.R"))


ukb_gp <- arrow::read_parquet(file = paste0(datapath, "cohort_data/ukb_gp_linked.parquet"))

#PHQ-9 GAD-7
levels(ukb_gp$phq9_appetite)

ukb_gp <- ukb_gp %>% 
  mutate(across(starts_with(c("phq9", "gad7")),
                ~ fct_recode(.x, NULL="Prefer not to answer") %>% #Recode "Prefer not to answer to NULL"
                  as.numeric),
         phq9=rowSums(across(starts_with("phq9"))),
         gad7=rowSums(across(starts_with("gad7"))))




ukb_gp$ethnicity2 <- as.character(ukb_gp$ethnicity) %>% stringr::str_to_lower() %>% factor()
levels(ukb_gp$ethnicity2) <- c(
  "black/black british/caribbean", "asian/asian british", "black/black british/caribbean", "mixed/other", "white/british/irish", "asian/asian british", "asian/asian british", 
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
  exposure=c("eczema_alg", "eczema_alg_gp", "eczema_alg_union",
             "psoriasis", "psoriasis_gp", "psoriasis_union"),
  outcome=c("depression","depression_gp", 
            "anxiety", "anxiety_gp")
)

comb2 <- expand_grid(
  exposure=c("eczema_alg", "eczema_alg_gp_pre16", "eczema_alg_pre16_union", 
             "psoriasis", "psoriasis_gp_pre16", "psoriasis_pre16_union"),
  outcome=c("ever_depressed_sad", "depression_gp_pre16", 
            "ever_anxious_worried", "anxiety_gp_pre16")
)
grid <- rbind(comb1, comb2)

#Function to run regressions
runreg <- function(.outcome, .exposure) {
  glm(glue("{.outcome} ~ {.exposure} + age_at_assess + sex + deprivation + ethnicity2"),
      data = ukb_gp , family = "binomial") %>% 
    broom::tidy(exponentiate = T, conf.int = T) %>% 
    mutate(exposure=.exposure, outcome=.outcome) %>% 
    filter(str_detect(term, eval(.exposure))) 
}

#Map regression function to all combinations of exposure and outcome
results_regression <- map2_df(grid$outcome, grid$exposure, runreg)

#Add group labels
results_regression <- results_regression %>% 
  mutate(
    exposure_group=case_when(
      str_detect(exposure, "eczema") ~ "eczema",
      str_detect(exposure, "psoriasis") ~ "psoriasis"),
    outcome_group=case_when(
      str_detect(outcome, "dep") ~ "depression",
      str_detect(outcome, "anx") ~ "anxiety"),
    outcome_defined_in=case_when(
      str_detect(outcome, "gp") ~ "linked GP data",
      !str_detect(outcome, "gp") ~ "UK Biobank"),
    timepoint=case_when(
      str_detect(outcome, "ever|pre16") ~ "follow-up survey",
      !str_detect(outcome, "ever|pre16") ~ "initial interview",
    ))
write_csv(results_regression, "out/results_regression.csv")
#results_regression <- read_csv("out/results_regression.csv")

#Linear regression
lm(phq9 ~ eczema_alg_pre16_union, data = ukb_gp) %>% 
  broom::tidy(exponentiate=TRUE, conf.int=TRUE)

# Forest Plot --------------------------------------------------------------------

#Function to plot a grid of forest plots
xlims <- c(0.5,2)
plot_forest_grid <- function(x) {ggplot(x, aes(y = outcome_defined_in, x = estimate, xmin = conf.low, xmax = conf.high)) +
    geom_errorbar() +
    geom_point() +
    geom_vline(xintercept = 1, lty = 2) +
    coord_cartesian(xlim=xlims) +
    xlab("Odds ratio (with 95% confidence interval)") +
    ylab("Outcome defined in") +
    theme_bw() +
    facet_grid(rows = vars(exposure_group), cols = vars(outcome_group))}

#2008
results_regression %>% 
  filter(timepoint=="initial interview",
         str_detect(exposure, "union")) %>% 
  plot_forest_grid()
ggsave("out/forest_plot_initial.png", width = 8, height = 2.5)

#2016
results_regression %>% 
  filter(outcome_defined_in == "UK Biobank",
    timepoint=="follow-up survey",
         str_detect(exposure, "union")) %>% 
  plot_forest_grid()
ggsave("out/forest_plot_follow_up.png", width = 8, height = 2)



# Two x Two tables -------------------------------------------------------
tbl_cross_pct <- function(...) {tbl_cross(..., percent = "row", missing = "no")}

#2006
##Eczema
###Anxiety
ukb_gp[!is.na(ukb_gp$anxiety),] %>% 
  tbl_cross_pct("eczema_alg_union", "anxiety")
ukb_gp[!is.na(ukb_gp$anxiety),] %>% 
  tbl_cross_pct("eczema_alg_union", "anxiety_gp")

###Depression
ukb_gp[!is.na(ukb_gp$depression),] %>% 
  tbl_cross_pct("eczema_alg_union", "depression")
ukb_gp[!is.na(ukb_gp$depression),] %>% 
  tbl_cross_pct("eczema_alg_union", "depression_gp")

##Psoriasis
###Anxiety
ukb_gp[!is.na(ukb_gp$anxiety),] %>% 
  tbl_cross_pct("psoriasis_union", "anxiety")
ukb_gp[!is.na(ukb_gp$anxiety),] %>% 
  tbl_cross_pct("psoriasis_union", "anxiety_gp")

###Depression
ukb_gp[!is.na(ukb_gp$depression),] %>% 
  tbl_cross_pct("psoriasis_union", "depression")
ukb_gp[!is.na(ukb_gp$depression),] %>% 
  tbl_cross_pct("psoriasis_union", "depression_gp")





#2016
##Eczema
###Anxiety
ukb_gp[!is.na(ukb_gp$ever_anxious_worried),] %>% 
  tbl_cross_pct("eczema_alg_pre16_union", "ever_anxious_worried")

ukb_gp[!is.na(ukb_gp$ever_anxious_worried),] %>% 
  tbl_cross_pct("eczema_alg_pre16_union", "anxiety_gp_pre16")

###Depression
ukb_gp[!is.na(ukb_gp$ever_depressed_sad),] %>% 
  tbl_cross_pct("eczema_alg_pre16_union", "ever_depressed_sad")

ukb_gp[!is.na(ukb_gp$ever_depressed_sad),] %>% 
  tbl_cross_pct("eczema_alg_pre16_union", "depression_gp_pre16")


##Psoriasis
###Anxiety
ukb_gp[!is.na(ukb_gp$ever_anxious_worried),] %>% 
  tbl_cross_pct("psoriasis_union", "ever_anxious_worried")

ukb_gp[!is.na(ukb_gp$ever_anxious_worried),] %>% 
  tbl_cross_pct("psoriasis_union", "anxiety_gp_pre16")

###Depression
ukb_gp[!is.na(ukb_gp$ever_depressed_sad),] %>% 
  tbl_cross_pct("psoriasis_union", "ever_depressed_sad")

ukb_gp[!is.na(ukb_gp$ever_depressed_sad),] %>% 
  tbl_cross_pct("psoriasis_union", "depression_gp_pre16")
