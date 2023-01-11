library(tidyverse)
library(data.table)
library(ukbtools)
library(irr)
library(skimr)
library(summarytools)
library(cowplot)

dir.create(path = here::here("out"), showWarnings = F)

ukb <- read_rds(paste0(datapath, "ukb669156.rds"))
my_ukb_key <- ukb_df_field("ukb669156", path = datapath)

twoXtwo <- function(df, exp, out){
  df1 <- df %>% 
    ungroup() %>% 
    dplyr::select(exp = {{ exp }}, out = {{ out }})
  tab <- table(df1$exp, df1$out)
  tab_p <- prop.table(tab,1)
  tibble(
    exposure = exp,
    val = rownames(tab),
    No = tab[, 1],
    No_pc = tab_p[, 1] * 100,
    Yes = tab[, 2],
    Yes_pc = tab_p[, 2] * 100,
    Miss = tab[, 3]
  )
}

# basic info about participants -------------------------------------------
ukb_descriptive <- ukb %>% 
  select(f.eid, 
         age_at_recruit = f.21022.0.0, 
         age_at_assess = f.21003.0.0,
         sex = f.31.0.0,
         deprivation = f.189.0.0,
         study_entry = f.53.0.0,
         ethnicity = f.21000.0.0,
         hh_inc = f.738.0.0,
         emplyment = f.6142.0.0,
         qualifications = f.6138.0.0
         ) %>% 
  mutate(age_cat = cut_number(age_at_assess, n = 6))

ukb_n <- dim(ukb_descriptive)[1]
ukb_n_age <- ukb_descriptive %>% 
  group_by(age_cat) %>% 
  summarise(n = n())
ukb_n_sex <- ukb_descriptive %>% 
  group_by(sex) %>% 
  summarise(n = n())

dfSummary(ukb_descriptive)

theme_ukb <- function(){
    theme(legend.position = "none", 
          axis.title = element_text(face = "bold"), 
          panel.grid = element_blank())
}
cols <- c("Female" = "grey35", "Male" = "hotpink")
fills <- c("Femaled" = "grey35", "Male" = "hotpink")

p_sex <- ggplot(ukb_descriptive, aes(y = sex, fill = sex)) + 
  geom_bar(aes(x = (..count..)*100/sum(..count..)), position = "dodge", na.rm = TRUE, width = 0.7, alpha = 1) + 
  labs(y = "Sex", x = "%") +
  scale_fill_manual(values = c("grey35", "hotpink"), na.value = "grey65") + 
  scale_x_continuous(limits = c(0,100)) +
  theme_ukb()

p_age <- ggplot(ukb_descriptive, aes(x = age_at_assess, fill = sex, col = sex)) +
  geom_density(alpha = 0.25, na.rm = TRUE) +
  scale_colour_manual(values = cols,
                      aesthetics = c("color", "fill")) + 
  labs(x = "Age at assessment", y = "Density") +
  theme_ukb()

p_entry <- ggplot(ukb_descriptive, aes(x = study_entry, fill = sex, col = sex)) +
  geom_density(alpha = 0.25, na.rm = TRUE) +
  scale_colour_manual(values = cols,
                      aesthetics = c("color", "fill")) + 
  labs(x = "Study entry", y = "Density") +
  theme_ukb()

p_dep <- ggplot(ukb_descriptive, aes(x = deprivation, fill = sex, col = sex)) +
  geom_density(alpha = 0.25, na.rm = TRUE) +
  scale_colour_manual(values = cols,
                      aesthetics = c("color", "fill")) + 
  labs(x = "Townsend deprivation score", y = "Density") +
  theme_ukb()

p_qual <- ggplot(ukb_descriptive, aes(y = qualifications, fill = sex)) +
  geom_bar(aes(x = (..count..)/1000), position = "dodge", na.rm = TRUE, width = 0.7) + 
  scale_fill_manual(values = c("grey35", "hotpink")) + 
  labs(y = "Qualifications", x = "Count (thousands)") +
  theme_ukb()

p_income <- ggplot(ukb_descriptive, aes(y = hh_inc, fill = sex)) +
  geom_bar(aes(x = (..count..)*100/sum(..count..)), position = "dodge", na.rm = TRUE, width = 0.7) + 
  scale_fill_manual(values = c("grey35", "hotpink")) + 
  labs(y = "Household Income", x = "%") +
  theme_ukb()

p_ethnicity <- ggplot(ukb_descriptive, aes(y = ethnicity, fill = sex)) + 
  geom_bar(aes(x = (..count..)*100/sum(..count..)), position = "dodge", na.rm = TRUE, width = 0.7) + 
  scale_fill_manual(values = c("grey35", "hotpink")) + 
  labs(y = "Ethnic Background", x = "%") +
  theme_ukb()

p_employ <- ggplot(ukb_descriptive, aes(y = emplyment, fill = sex)) + 
  geom_bar(aes(x = (..count..)*100/sum(..count..)), position = "dodge", na.rm = TRUE, width = 0.7) + 
  scale_fill_manual(values = c("grey35", "hotpink")) + 
  labs(y = "Employment", x = "%") +
  theme_ukb()

pdf(here::here("out/baseline_cohort_characteristics.pdf"), width = 12, height = 8)
  plot_grid(p_sex, p_ethnicity, p_age, p_employ, p_dep, p_income, p_entry, p_qual, ncol = 2)
dev.off()

# diagnoses ---------------------------------------------------------------
ukbdiagnoses_baseline <- ukb %>% 
  select(f.eid, contains("20002.0")) %>% 
  left_join(ukb_descriptive, by = "f.eid")

# make long 
ukbdiagnoses_baseline_long <- ukbdiagnoses_baseline %>% 
  pivot_longer(cols = contains("20002.0"), values_to = "illness_code", 
               names_to = c("instance", "array"),
               names_pattern = "f.20002.(.).(.*)")

# skin disease at baseline ------------------------------------------------
ecz_pso <- ukbdiagnoses_baseline_long %>% 
  mutate(eczema = as.numeric(illness_code == 1452),
         psoriasis = as.numeric(illness_code == 1453)) 

ecz_pso_tbl <- ecz_pso %>% 
  summarise(ecz = sum(eczema, na.rm = T), 
            pso = sum(psoriasis, na.rm = T))
ecz_pso_tbl
(ecz_pso_tbl/ukb_n)*100

ecz_pso_tbl_age <- ecz_pso %>% 
  group_by(age_cat) %>% 
  summarise(ecz = sum(eczema, na.rm = T), 
            pso = sum(psoriasis, na.rm = T))
ecz_pso_tbl_age
ecz_pso_tbl_age %>% 
  ungroup() %>% 
  left_join(ukb_n_age, by = "age_cat") %>% 
  mutate_at(c("ecz","pso"), ~(./n)*100)

# mental illnesses at baseline ---------------------------------------------
mental_ill <- ecz_pso %>% 
  mutate(anxiety = as.numeric(illness_code == 1287),
         depression = as.numeric(illness_code == 1286),
         smi = as.numeric(illness_code %in% c(1289,1291,1290))
         ) 
mental_ill_tbl <- mental_ill %>% 
  summarise_at(c("anxiety", "depression", "smi"), ~sum(., na.rm = T))
mental_ill_tbl
(mental_ill_tbl/ukb_n)*100

## collapse data
dt_mentalill <- setDT(mental_ill)
for (i in c("eczema", "psoriasis", "anxiety", "depression", "smi")) {
  dt_mentalill[is.na(get(i)), (i):=0]
}

dt_mentalill_1row <- dt_mentalill[, lapply(.SD, max), .SDcols = c("eczema", "psoriasis", "anxiety", "depression", "smi"), by = f.eid]
dt_mentalill_1row_demo <- dt_mentalill[, .SD[1], by = f.eid]
dt_mentalill_1row_demo <- dt_mentalill_1row_demo[, -c("eczema", "psoriasis", "anxiety", "depression", "smi", "instance", "array", "illness_code")]
dt_mentalill_1row_combined <- dt_mentalill_1row_demo[dt_mentalill_1row, on = .(f.eid)]

df_1row <- tibble(dt_mentalill_1row_combined) %>% 
  ungroup()

twoXtwo(df= df_1row, exp = "eczema", out = "anxiety")
twoXtwo(df= df_1row, exp = "eczema", out = "depression")
twoXtwo(df= df_1row, exp = "eczema", out = "smi")

twoXtwo(df= df_1row, exp = "psoriasis", out = "anxiety")
twoXtwo(df= df_1row, exp = "psoriasis", out = "depression")
twoXtwo(df= df_1row, exp = "psoriasis", out = "smi")

# eczema/psoriasis in linked records --------------------------------------
# 41270	Diagnoses - ICD10
# 41280	Date of first in-patient diagnosis - ICD10

ukb_hes <- ukb %>% 
  select(f.eid, starts_with("f.41270.0"), starts_with("f.41280.0")) %>% 
  left_join(ukb_descriptive, by = "f.eid")

# make long 
ukb_hes_long <- ukb_hes %>% 
  pivot_longer(cols = c(starts_with("f.41270.0"), starts_with("f.41280.0")), 
               names_to = c(".value", "array"),
               names_pattern = "f.412(.*)0.0.(.*)") %>% 
  rename("icd10" = `7`, "diag_date" = `8`)

ukb_hes_long$icd10[ukb_hes_long$diag_date > ukb_hes_long$study_entry] <- NA ## replace any diagnoses AFTER recruitment with NA

anxiety_codelist <- read.delim(here::here("codelist/eczema_psych_anxiety_HES.txt"), sep = ",")
depression_codelist <- read.delim(here::here("codelist/eczema_psych_depression_HES.txt"), sep = ",")
eczema_codelist <- read.delim(here::here("codelist/eczema_psych_eczemaDx_HES.txt"), sep = ",")
  eczema_codelist$ICD10codealternative <- str_remove_all(eczema_codelist$ICD10code, "\\.")
psoriasis_codelist <- read.delim(here::here("codelist/psoriasis_icd.txt"), sep = ",")
  psoriasis_codelist$ICD10codealternative <- str_to_upper(str_remove_all(psoriasis_codelist$icd, "\\."))
smi_codelist <- read.delim(here::here("codelist/ICD10codes-SMI.txt"), sep = ",")
  names(smi_codelist)[2] <- "ICD10codealternative"

hes_mental <- ukb_hes_long %>% 
  mutate(anxiety = as.numeric(icd10 %in% anxiety_codelist$ICD10codealternative),
         depression = as.numeric(icd10 %in% depression_codelist$ICD10codealternative),
         smi = as.numeric(icd10 %in% smi_codelist$ICD10codealternative),
         eczema = as.numeric(icd10 %in% eczema_codelist$ICD10codealternative),
         psoriasis = as.numeric(icd10 %in% psoriasis_codelist$ICD10codealternative)
         )

hes_mental_tbl <- hes_mental %>% 
  summarise(anxiety = sum(anxiety, na.rm = T), 
            depression = sum(depression, na.rm = T),
            smi = sum(smi, na.rm = T),
            psoriasis = sum(psoriasis, na.rm = T),
            eczema = sum(eczema, na.rm = T))
hes_mental_tbl
(hes_mental_tbl/ukb_n)*100

## 2x2 using HES pre-baseline only
dt_hes_mental <- setDT(hes_mental)
for (i in c("eczema", "psoriasis", "anxiety", "depression", "smi")) {
  dt_hes_mental[is.na(get(i)), (i):=0]
}

dt_hes_mental_1row <- dt_hes_mental[, lapply(.SD, max), .SDcols = c("eczema", "psoriasis", "anxiety", "depression", "smi"), by = f.eid]
dt_hes_mental_1row_demo <- setDT(ukb_descriptive)
dt_hes_mental_1row_combined <- dt_hes_mental_1row_demo[dt_hes_mental_1row, on = .(f.eid)]

df_hes_1row <- tibble(dt_hes_mental_1row_combined) %>% 
  ungroup()

twoXtwo(df= df_hes_1row, exp = "eczema", out = "anxiety")
twoXtwo(df= df_hes_1row, exp = "eczema", out = "depression")
twoXtwo(df= df_hes_1row, exp = "eczema", out = "smi")

twoXtwo(df= df_hes_1row, exp = "psoriasis", out = "anxiety")
twoXtwo(df= df_hes_1row, exp = "psoriasis", out = "depression")
twoXtwo(df= df_hes_1row, exp = "psoriasis", out = "smi")

## discordance - who has a mental illness code in HES but _not_ in assessment
diagnoses_list <- c("eczema", "psoriasis", "anxiety", "depression", "smi")
df_1row_combined <- df_1row %>% 
  rename_at(all_of(diagnoses_list), ~paste0(., "_assess")) %>% 
  left_join(df_hes_1row, by = c("f.eid", "age_at_recruit", "age_at_assess", "sex", "deprivation", "age_cat")) %>% 
  rename_at(all_of(diagnoses_list), ~paste0(., "_hes")) 

print("Kappa for assessment|hes")
kappa_df <- NULL
for(X in diagnoses_list){
  cont_table <- df_1row_combined %>% 
    select(contains(X)) %>% 
    group_by_all() %>% 
    tally() %>% 
    ungroup() %>% 
    spread(key = 2, value = n) %>% 
    select(2:3) %>% 
    as.matrix()
  a <- cont_table[1,1]
  b <- cont_table[1,2]
  c <- cont_table[2,1]
  d <- cont_table[2,2]
  n <- a+b+c+d
  p0 <- (a+d)/n
  pY <- ((a+b)/n)*((a+c)/n)
  pN <- ((c+d)/n)*((b+d)/n)
  pE <- pY + pN
  kappa <- ((p0-pE)/(1-pE))
  print(c(X, signif(kappa,3)))
  print.table(cont_table)
  results <- data.frame(variable = X, kappa = kappa)
  kappa_df <- kappa_df %>% 
    bind_rows(results)
}

