# Load libraries
library(targets)
library(arrow)
library(gt)
library(kableExtra)
library(tidyverse)


outcome_labels <- tribble(
  ~outcome, ~outcome_label,
  "ever_depressed_sad", "Ever depressed/sad ≥ 1 week",
  "ever_anxious_worried", "Ever anxious/worried ≥ 1 week",
  "phq9_greater_9", "PHQ-9 ≥ 10 in last 2 weeks",
  "gad7_greater_9", "GAD-7 ≥ 10 in last 2 weeks") %>% 
  mutate(outcome_label=as_factor(outcome_label))

#Define function to add forest-plot graphics to table
fptable <- function(data) {
  data %>% 
    mutate(image = kableExtra::spec_pointrange(x = estimate, 
                                               xmin = conf.low, 
                                               xmax = conf.high, 
                                               vline = 1,
                                               lim = c(min(0,min(conf.low)), 
                                                       2),
                                               cex = .75,
                                               width = 400,
                                               col = "black",
                                               pch = 16),
           image = map(image, "svg_text"),
           image = map(image, ~gt::html(as.character(.x)))) 
  
}

#Define function to make table
gtable <- function(x, caption) {
  x %>% 
    gt() %>% 
    fmt_number(columns = c(estimate, conf.low, conf.high),
               decimals = 2) %>% 
    cols_merge(columns = c(estimate, conf.low, conf.high), 
               pattern = "{1} ({2}-{3})") %>% 
    tab_footnote(locations = cells_column_labels(columns = estimate),
                 footnote = footnote_or) %>% 
    cols_label(image = "",
               estimate = "OR (95%CI)") %>% 
    tab_options(footnotes.font.size = "75%")
}

footnote_or <- "Odds ratios (95% confidence intervals) estimated from logistic regression for having a mental illness (adjusted for age, sex, deprivation and ethnicity) comparing people with the respective skin disease to people without the respective skin disease"
footnote_outcomedef_a <- "Outcome is defined in two different data sources: as a previous doctors diagnosis reported at the UKB interview around, and at least 1 diagnosis code in linked GP data."
footnote_mental_health_survey <- "Outcomes measured at UK Biobank 2016 meantal health follow-up survey (70,878 responded), either as self-reported symptoms of ever having felt depressed or sad/anxious or worried for more than one week or a score of ≥10 in the PHQ-9 score for depression/the GAD-7 score for anxiety, which take into account symptoms in the 2 weeks prior to the survey"


#2008
fptable_2008 <- results_regression %>% 
  filter(timepoint=="initial interview",
         str_detect(exposure, "union")) %>% 
  fptable() %>% 
  select(estimate, conf.low, conf.high, image, exposure_group, outcome_defined_in, outcome_group) %>% 
  group_by(outcome_group, exposure_group) %>% 
  gtable() %>% 
  cols_label(outcome_defined_in="Outcome defined in") %>% 
  tab_footnote(locations = cells_column_labels(columns = outcome_defined_in),
               footnote = footnote_outcomedef_a)
fptable_2008
gtsave(fptable_2008, "out/fptable_2008.png", vwidth=600)


#2016
fptable_2016 <- results_regression %>% 
  left_join(outcome_labels) %>% 
  filter(outcome_defined_in == "UK Biobank",
         timepoint=="follow-up survey",
         str_detect(exposure, "union")) %>% 
  fptable() %>% 
  select(outcome_label,estimate, conf.low, conf.high, image, exposure_group, outcome_group) %>% 
  group_by(outcome_group, exposure_group) %>% 
  gtable() %>% 
  cols_label(outcome_label="Outcome") %>% 
  tab_footnote(locations = cells_column_labels(columns = outcome_label),
               footnote = footnote_mental_health_survey)
fptable_2016
gtsave(fptable_2016, "out/fptable_2016.png", vwidth=600)

#2016: PHQ9, GAD7 with comparison
fptable_2016comp <- results_regression %>% 
  filter(timepoint=="follow-up survey",
         str_detect(exposure, "union"),
         !str_detect(outcome, "ever")) %>% 
  fptable() %>% 
  select(estimate, conf.low, conf.high, image, exposure_group, outcome_defined_in, outcome_group) %>% 
  group_by(outcome_group, exposure_group) %>% 
  gtable()
fptable_2016comp
gtsave(fptable_2016comp, "out/fptable_2016comp.png")

