# Load libraries
library(targets)
library(arrow)
library(gt)
library(kableExtra)
library(tidyverse)

results_regression$outcome
outcome_labels <- tribble(
  ~outcome, ~outcome_label,
  "depression", "Self-reported diagnosis",
  "depression_gp", "≥ 1 diagnosis pre-interview",
  "anxiety","Self-reported diagnosis",
  "anxiety_gp", "≥ 1 diagnosis pre-interview", 
  "ever_depressed_sad", "Ever symptoms ≥ 1 week",
  "ever_anxious_worried", "Ever symptoms ≥ 1 week",
  "phq9_greater_9", "PHQ-9/GAD-7 ≥ 10",
  "gad7_greater_9", "PHQ-9/GAD-7 ≥ 10",
  "depression_gp_pre16", "≥ 1 diagnosis pre-survey",
  "anxiety_gp_pre16", "≥ 1 diagnosis pre-survey") %>% 
  mutate(outcome_label=as_factor(outcome_label))

#Define function to add forest-plot graphics to table
fptable <- function(data) {
  data %>% 
    mutate(image = kableExtra::spec_pointrange(x = estimate, 
                                               xmin = conf.low, 
                                               xmax = conf.high, 
                                               vline = 1,
                                               lim = c(min(conf.low), 
                                                       max(conf.high)),
                                               cex = .75,
                                               width = 200,
                                               col = "black",
                                               pch = 16),
           image = map(image, "svg_text"),
           image = map(image, ~gt::html(as.character(.x)))) 
  
}


footnote_or <- "Odds ratios (95% confidence intervals) estimated from logistic regression for having a mental illness (adjusted for age, sex, deprivation and ethnicity) comparing people with the respective skin disease to people without the respective skin disease"
footnote_nobs <- 'Number of observations that went into the model. Observations with missing values were dropped. "Prefer not to answer" and "Do not know" were treated as missing values. For the follow-up survey timepoint, only used GP data where all of the questions of the mental health follow-up survey were answered'
footnote_mental_health_survey <- "At the Initial interview timepoint, outcomes are defined either as a self-reported previous doctor's diagnosis, or at least 1 diagnosis code in linked GP data prior to the interview. At the 2016 mental health follow-up survey (70,878 responded), outcomes are defined either as a score of ≥10 in the PHQ-9 score for depression/the GAD-7 score for anxiety, which take into account symptoms in the 2 weeks prior to the survey, or at least 1 diagnosis code in linked GP data prior to the follow-up survey"

#2016: PHQ9, GAD7 with comparison
fptable <- results_regression %>% 
  filter(str_detect(exposure, "union"), !str_detect(outcome, "ever_")) %>% 
  left_join(outcome_labels) %>% 
  select(-term, -statistic, -p.value, -std.error, -outcome, -exposure) %>% 
  fptable() %>% 
  pivot_wider(names_from = exposure_group, values_from = c(estimate, conf.low, conf.high, image, nobs)) %>% 
  select(outcome_defined_in, timepoint, outcome_group, outcome_label,
         estimate_eczema, conf.low_eczema, conf.high_eczema, image_eczema, nobs_eczema,
         estimate_psoriasis, conf.low_psoriasis, conf.high_psoriasis, image_psoriasis, nobs_psoriasis) %>% 
  arrange(desc(timepoint), desc(outcome_group)) %>% 
  group_by(outcome_defined_in) %>% 
  gt(rowname_col = "outcome_group") %>% 
  fmt_number(columns = c(estimate_eczema, conf.low_eczema, conf.high_eczema),
             decimals = 2) %>% 
  fmt_number(columns = c(estimate_psoriasis, conf.low_psoriasis, conf.high_psoriasis),
             decimals = 2) %>% 
  fmt_number(columns = nobs_eczema, decimals = 0) %>% 
  fmt_number(columns = nobs_psoriasis, decimals = 0) %>% 
  cols_merge(columns = c(estimate_eczema, conf.low_eczema, conf.high_eczema), 
             pattern = "{1} ({2}-{3})") %>% 
  cols_merge(columns = c(estimate_psoriasis, conf.low_psoriasis, conf.high_psoriasis), 
             pattern = "{1} ({2}-{3})") %>% 
  cols_label(outcome_label="Outcome definition",
             image_eczema = "",
             estimate_eczema = "OR (95%CI)",
             nobs_eczema="n",
             image_psoriasis = "",
             estimate_psoriasis = "OR (95%CI)",
             nobs_psoriasis="n") %>% 
  tab_options(footnotes.font.size = "75%") %>% 
  tab_footnote(locations = cells_column_labels(columns = c(estimate_eczema, estimate_psoriasis)),
               footnote = footnote_or) %>% 
  tab_footnote(locations = cells_column_labels(columns = c(nobs_eczema, nobs_psoriasis)),
               footnote = footnote_nobs) %>% 
  tab_footnote(locations = cells_column_labels(columns = outcome_label),
               footnote = footnote_mental_health_survey) %>% 
  tab_spanner(label="Exposure: Eczema", columns=c(estimate_eczema, image_eczema, nobs_eczema)) %>% 
  tab_spanner(label="Exposure: Psoriasis", columns=c(estimate_psoriasis, image_psoriasis, nobs_psoriasis)) %>% 
  tab_style(
    style = list(cell_fill(color = "#D9E1E2")),
    locations = cells_body(columns = everything(), rows = timepoint == "initial interview")
  )

fptable
gtsave(fptable, "out/fptable.png", vwidth=1250)
gtsave(fptable, "out/fptable.html")

