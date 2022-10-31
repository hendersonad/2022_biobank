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

ukb_gp$ethnicity2 <- as.character(ukb_gp$ethnicity) %>% stringr::str_to_lower() %>% factor()
levels(ukb_gp$ethnicity2) <- c("black/black british/caribbean", "asian/asian british", "black/black british/caribbean", "mixed/other", "white/british/irish", "asian/asian british", "asian/asian british", 
                               "black/black british/caribbean", "white/british/irish", "black/black british/caribbean", "asian/asian british", "na/no answer", "asian/asian british", "white/british/irish",
                               "mixed/other", "mixed/other", "asian/asian british", "na/no answer", "white/british/irish", "mixed/other", "mixed/other", "mixed/other")


ukb_gp$eczema_union <- (as.numeric(ukb_gp$eczema)-1) + as.numeric(ukb_gp$eczema_gp)
ukb_gp$eczema_intersect <- ifelse(ukb_gp$eczema_union==2, 1, 0)
ukb_gp$eczema_union[ukb_gp$eczema_union==2] <- 1

ukb_gp$psoriasis_union <- (as.numeric(ukb_gp$psoriasis)-1) + as.numeric(ukb_gp$psoriasis_gp)
ukb_gp$psoriasis_intersect <- ifelse(ukb_gp$psoriasis_union==2, 1, 0)
ukb_gp$psoriasis_union[ukb_gp$psoriasis_union==2] <- 1

ukb_gp$depression_union <- (as.numeric(ukb_gp$depression)-1) + as.numeric(ukb_gp$depression_gp)
ukb_gp$depression_intersect <- ifelse(ukb_gp$depression_union==2, 1, 0)
ukb_gp$depression_union[ukb_gp$depression_union==2] <- 1

ukb_gp$anxiety_union <- (as.numeric(ukb_gp$anxiety)-1) + as.numeric(ukb_gp$anxiety_gp)
ukb_gp$anxiety_intersect <- ifelse(ukb_gp$anxiety_union==2, 1, 0)
ukb_gp$anxiety_union[ukb_gp$anxiety_union==2] <- 1

table(ukb_gp$psoriasis_gp, ukb_gp$depression_gp, useNA = "ifany") %>% prop.table()
table(ukb_gp$psoriasis_gp, ukb_gp$depression_union) %>% prop.table()
run_comparison_regressions <- function(.x, .y){
  fm1 <- "{.y} ~ {.x} + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  fm2 <- "{.y}_gp ~ {.x}_gp + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  fm3 <- "{.y}_union ~ {.x}_union + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  fm4 <- "{.y}_union ~ {.x}_gp + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  
  mod1 <- glm(fm1, data = ukb_gp, family = "binomial")
  mod2 <- glm(fm2, data = ukb_gp, family = "binomial")
  mod3 <- glm(fm3, data = ukb_gp, family = "binomial")
  mod4 <- glm(fm4, data = ukb_gp, family = "binomial")
  
  res1 <- broom::tidy(mod1, exponentiate = T, conf.int = T)
  res2 <- broom::tidy(mod2, exponentiate = T, conf.int = T)
  res3 <- broom::tidy(mod3, exponentiate = T, conf.int = T)
  res4 <- broom::tidy(mod4, exponentiate = T, conf.int = T)
  
  results <- filter(res1, str_detect(term, eval(.x))) %>% 
    bind_rows(
      filter(res2, str_detect(term, eval(.x)))
    ) %>% 
    bind_rows(
      filter(res3, str_detect(term, eval(.x)))
    ) %>% 
    bind_rows(
      filter(res4, str_detect(term, eval(.x)))
    ) 
  
  results$model <- factor(1:4, levels = 1:4, labels = c("UKB only", "Linked GP only", "Either data", "x(GP)~y(either)"))
  ggplot(results, aes(y = model, x = estimate, xmin = conf.low, xmax = conf.high, colour = term, fill = term)) +
    geom_errorbar() +
    geom_point() +
    labs(title = glue::glue("{.x} ~ {.y}")) +
    geom_vline(xintercept = 1, lty = 2) +
    xlim(0, 2) +
    theme_ali()
}
p1 <- run_comparison_regressions("eczema", "depression")
p2 <- run_comparison_regressions("eczema", "anxiety")
p3 <- run_comparison_regressions("psoriasis", "depression")
p4 <- run_comparison_regressions("psoriasis", "anxiety")
pdf(here::here("out/OR_comparison.pdf"), 8, 4)
  cowplot::plot_grid(p1, p2, p3, p4, ncol = 2, labels = "AUTO")
dev.off()
