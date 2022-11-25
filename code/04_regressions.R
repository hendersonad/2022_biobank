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

ukb_gp$eczema_alg_union <- (as.numeric(ukb_gp$eczema_alg)-1) + as.numeric(ukb_gp$eczema_alg_gp)
ukb_gp$eczema_alg_intersect <- ifelse(ukb_gp$eczema_alg_union==2, 1, 0)
ukb_gp$eczema_alg_union[ukb_gp$eczema_alg_union==2] <- 1

ukb_gp$psoriasis_union <- (as.numeric(ukb_gp$psoriasis)-1) + as.numeric(ukb_gp$psoriasis_gp)
ukb_gp$psoriasis_intersect <- ifelse(ukb_gp$psoriasis_union==2, 1, 0)
ukb_gp$psoriasis_union[ukb_gp$psoriasis_union==2] <- 1

ukb_gp$depression_union <- (as.numeric(ukb_gp$depression)-1) + as.numeric(ukb_gp$depression_gp)
ukb_gp$depression_intersect <- ifelse(ukb_gp$depression_union==2, 1, 0)
ukb_gp$depression_union[ukb_gp$depression_union==2] <- 1

ukb_gp$anxiety_union <- (as.numeric(ukb_gp$anxiety)-1) + as.numeric(ukb_gp$anxiety_gp)
ukb_gp$anxiety_intersect <- ifelse(ukb_gp$anxiety_union==2, 1, 0)
ukb_gp$anxiety_union[ukb_gp$anxiety_union==2] <- 1

run_comparison_regressions <- function(.x, .y){
  fm1 <- "{.y} ~ {.x}_union + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  fm2 <- "{.y}_gp ~ {.x}_union + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  fm4 <- "{.y}_union ~ {.x}_union + age_at_assess + sex + deprivation + ethnicity2" %>% glue()
  
  mod1 <- glm(fm1, data = ukb_gp, family = "binomial")
  mod2 <- glm(fm2, data = ukb_gp, family = "binomial")
  mod4 <- glm(fm4, data = ukb_gp, family = "binomial")
  
  res1 <- broom::tidy(mod1, exponentiate = T, conf.int = T)
  res2 <- broom::tidy(mod2, exponentiate = T, conf.int = T)
  res4 <- broom::tidy(mod4, exponentiate = T, conf.int = T)
  
  results <- filter(res1, str_detect(term, eval(.x))) %>% 
    bind_rows(
      filter(res2, str_detect(term, eval(.x)))
    ) %>% 
    bind_rows(
      filter(res4, str_detect(term, eval(.x)))
    ) 
  
  results$model <- factor(1:3, levels = 1:3, labels = c("UKB only", "Linked GP only", "x(GP)~y(GP|UKB)"))
  results$x <- glue("{.x}")
  results$y <- glue("{.y}")
  results
}
res1 <- run_comparison_regressions("eczema_alg", "depression")
res2 <- run_comparison_regressions("eczema_alg", "anxiety")
res3 <- run_comparison_regressions("psoriasis", "depression")
res4 <- run_comparison_regressions("psoriasis", "anxiety")

results <- res1 %>% 
  bind_rows(res2) %>% 
  bind_rows(res3) %>% 
  bind_rows(res4) %>% 
  mutate(across(c(x,y), as.character))

write_csv(results, "out/OR_results.csv")

forest_plot_grid <- ggplot(results, aes(y = model, x = estimate, xmin = conf.low, xmax = conf.high)) +
  geom_errorbar() +
  geom_point() +
  #labs(title = glue::glue("{.y} ~ {.x}")) +
  geom_vline(xintercept = 1, lty = 2) +
  xlim(1, NA) +
  xlab("Odds ratio (with 95% confidence interval)") +
  ylab("Outcome defined in") +
  scale_y_discrete(labels=c("UKB only"="UKB interview",
                            "Linked GP only"="linked GP records",
                            "x(GP)~y(GP|UKB)"="either data source")) +
  theme_bw() +
  facet_grid(rows = vars(x), cols = vars(y),
             labeller = labeller(x = c(eczema_alg="Exposure: Eczema", psoriasis="Exposure: Psoriasis"),
                                 y = c(anxiety="Outcome: Anxiety", depression="Outcome: Depression")))
forest_plot_grid

pdf(here::here("out/OR_comparison_v2.pdf"), 8, 4)
forest_plot_grid
dev.off()

names(ukb_gp)
twoXtwo(ukb_gp, "eczema_alg_union", "anxiety")
twoXtwo(ukb_gp, "eczema_alg_union", "anxiety_gp")
twoXtwo(ukb_gp, "eczema_alg_union", "ever_anxious_worried")

twoXtwo(ukb_gp, "eczema_alg_union", "depression")
twoXtwo(ukb_gp, "eczema_alg_union", "depression_gp")
twoXtwo(ukb_gp, "eczema_alg_union", "ever_depressed_sad")

glimpse(ukb_gp)
