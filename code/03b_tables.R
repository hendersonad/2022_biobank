# build temp table 1  -----------------------------------------------------
table1_ecz <- baseline_assessment_cohort %>%
  ungroup() %>%
  dplyr::select(-f.eid, -study_entry, -date_mh_survey) %>%
  tbl_summary(
    by = eczema,
    statistic = list(
      all_continuous() ~ "{p50} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "Median (IQR) or Frequency (%)")

table1_ecz

gt_tab <- table1_ecz %>%
  as_gt() %>% 
  cols_align(columns = 6:8, "right") %>% 
  gt::gtsave(
    filename = paste0("ukb_baseline_eczema.html"),
    path = here::here("out/tables")
  )

table1_pso <- baseline_assessment_cohort %>%
  ungroup() %>%
  dplyr::select(-f.eid, -study_entry, -date_mh_survey) %>%
  tbl_summary(
    by = psoriasis,
    statistic = list(
      all_continuous() ~ "{p50} ({p25}-{p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_footnote(all_stat_cols() ~ "Median (IQR) or Frequency (%)")

table1_pso

gt_tab <- table1_pso %>%
  as_gt() %>% 
  cols_align(columns = 6:8, "right") %>% 
  gt::gtsave(
    filename = paste0("ukb_baseline_psoriasis.html"),
    path = here::here("out/tables")
  )
