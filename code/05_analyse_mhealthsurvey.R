#' Use combined datasets between UKB and GP records to run simple regressions
#' but use multiple versions of the exposure and outcome depending on the source 
#' of the data (UKB interview only, linked GP, combined, + MH questionnaire?)
#' 
library(tidyverse)
library(here)
library(arrow)
library(glue)

source(here::here("file_paths.R"))

ukb_gp <- arrow::read_parquet(file = paste0(datapath, "cohort_data/ukb_gp_linked.parquet"))



mh_analysis <- ukb_gp %>% 
  dplyr::select(f.eid, age_at_assess, sex, deprivation, ethnicity,
                contains(c("eczema", "psoriasis", "anxiety", "depression")),
                date_mh_survey, ever_anxious_worried, ever_depressed_sad, ever_soughthelp, happy)


mh_analysis$ethnicity2 <- as.character(mh_analysis$ethnicity) %>% stringr::str_to_lower() %>% factor()
levels(mh_analysis$ethnicity2) <- c("black/black british/caribbean", "asian/asian british", "black/black british/caribbean", "mixed/other", "white/british/irish", "asian/asian british", "asian/asian british", 
                               "black/black british/caribbean", "white/british/irish", "black/black british/caribbean", "asian/asian british", "na/no answer", "asian/asian british", "white/british/irish",
                               "mixed/other", "mixed/other", "asian/asian british", "na/no answer", "white/british/irish", "mixed/other", "mixed/other", "mixed/other")


mh_analysis$eczema_union <- (as.numeric(mh_analysis$eczema)-1) + as.numeric(mh_analysis$eczema_gp)
mh_analysis$eczema_intersect <- ifelse(mh_analysis$eczema_union==2, 1, 0)
mh_analysis$eczema_union[mh_analysis$eczema_union==2] <- 1

mh_analysis$eczema_alg_union <- (as.numeric(mh_analysis$eczema_alg)-1) + as.numeric(mh_analysis$eczema_alg_gp)
mh_analysis$eczema_alg_intersect <- ifelse(mh_analysis$eczema_alg_union==2, 1, 0)
mh_analysis$eczema_alg_union[mh_analysis$eczema_alg_union==2] <- 1

mh_analysis$psoriasis_union <- (as.numeric(mh_analysis$psoriasis)-1) + as.numeric(mh_analysis$psoriasis_gp)
mh_analysis$psoriasis_intersect <- ifelse(mh_analysis$psoriasis_union==2, 1, 0)
mh_analysis$psoriasis_union[mh_analysis$psoriasis_union==2] <- 1

levels(mh_analysis$happy)[1:2] <- NA
levels(mh_analysis$happy)[1:3] <- "happy"
levels(mh_analysis$happy)[2:5] <- "unhappy"
mh_analysis$happy <- factor(mh_analysis$happy, levels = c("unhappy", "happy"))

levels(mh_analysis$ever_anxious_worried)[1:2] <- NA

levels(mh_analysis$ever_depressed_sad)[1] <- NA

levels(mh_analysis$ever_soughthelp)[1:2] <- NA

run_comparison_regressions <- function(.x, .y){
  fm1 <- "{.y} ~ {.x} + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  fm2 <- "{.y} ~ {.x}_gp_pre16 + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  
  mod1 <- glm(fm1, data = mh_analysis, family = "binomial")
  mod2 <- glm(fm2, data = mh_analysis, family = "binomial")
  
  res1 <- broom::tidy(mod1, exponentiate = T, conf.int = T)
  res2 <- broom::tidy(mod2, exponentiate = T, conf.int = T)
  
  results <- filter(res1, str_detect(term, eval(.x))) %>% 
    bind_rows(
      filter(res2, str_detect(term, eval(.x)))
    )
  
  results$model <- factor(1:2, levels = 1:2, labels = c("UKB only", "Linked GP only"))
  ggplot(results, aes(y = model, x = estimate, xmin = conf.low, xmax = conf.high, colour = term, fill = term)) +
    geom_errorbar() +
    geom_point() +
    labs(title = glue::glue("{.x} ~ {.y}")) +
    geom_vline(xintercept = 1, lty = 2) +
    xlim(0, NA)
}
p1 <- run_comparison_regressions("eczema_alg", "ever_anxious_worried")
p2 <- run_comparison_regressions("eczema_alg", "ever_depressed_sad")
p3 <- run_comparison_regressions("eczema_alg", "ever_soughthelp")
p4 <- run_comparison_regressions("eczema_alg", "happy")
p5 <- run_comparison_regressions("psoriasis", "ever_anxious_worried")
p6 <- run_comparison_regressions("psoriasis", "ever_depressed_sad")
p7 <- run_comparison_regressions("psoriasis", "ever_soughthelp")
p8 <- run_comparison_regressions("psoriasis", "happy")
pdf(here::here("out/mh_analysis.pdf"), 12, 6)
  cowplot::plot_grid(p1, p2, p3, p4,
                     p5, p6, p7, p8, ncol = 2, labels = "AUTO")
dev.off()
