library(tidyverse)
library(data.table)
library(skimr)

datapath <- "/Volumes/EHR Group/GPRD_GOLD/Ali/2022_biobank/"

dir.create(here::here("data", "temp_store"), showWarnings = FALSE)
system(paste0("wget -nd https://biota.ndph.ox.ac.uk/tabserv.cgi?x=180721904008002d00985d6890ea037fWzQmaSN0PTE2NjU1NzgxMDUmcyNkPWdwX2NsaW5pY2FsJmkjYT03NDMxMSZpI3I9MTIyNTQzXQ== -P ", here::here("data", "temp_store", "gp_clinical.txt"), " -Ogp_clinical.txt"))

gp_clinical <- read_delim(here::here("temp_store/ukb.txt"), delim = "\t")
  
# look for eczema codes in tpp --------------------------------------------
gp_clinical_tpp <- gp_clinical %>% 
  filter(data_provider == 3)
gp_clinical_tpp %>% 
  filter(str_detect(read_3, "M111"))
eczema_tpp <- gp_clinical_tpp %>% 
  inner_join(eczema_mapped, by = c("read_3" = "readv3_code"))
eczema_tpp %>% View()
eczema_primarycare <- eczema_tpp$eid

# look for eczema codes in read v2 ----------------------------------------
gp_clinical_emisvision <- gp_clinical %>% 
  filter(data_provider != 3)
gp_clinical_emisvision %>% 
  filter(str_detect(read_2, "M111."))
eczema_mapped_readv2 <- eczema_mapped %>% 
  filter(match_mark == 1) %>% 
  group_by(readv2_code) %>% 
  slice(1) %>% 
  ungroup()
eczema_emisvision <- gp_clinical_emisvision %>% 
  inner_join(eczema_mapped_readv2, by = c("read_2" = "readv2_code"))
  

# combine TPP and EMIS/VISION ---------------------------------------------
eczema_primary_care <- eczema_tpp %>% 
  bind_rows(eczema_emisvision) %>% 
  group_by(eid) %>% 
  slice(1) %>% 
  ungroup() %>% 
  dplyr::select(f.eid = eid, data_provider, event_dt, read_2, read_3, termv3_desc) %>% 
  mutate(eczema_gp = 1)

# compare to UKB ----------------------------------------------------------
included_ids <- gp_clinical$eid %>% unique()
eczema_test <- eczema %>% 
  filter(f.eid %in% included_ids) %>% 
  full_join(eczema_primary_care, by = "f.eid")
mismatch <- ukb_descriptive %>% 
  filter(f.eid %in% included_ids) %>% 
  mutate(dob = as.Date(paste(year(study_entry)-age_at_recruit, "07", "01", sep = "-"))) %>% 
  left_join(eczema_test, by = "f.eid") %>% 
  filter(eczema == 1 | !is.na(eczema_gp)) %>% 
  mutate(age_eczema_gp = as.numeric((event_dt - dob)/365.25))

ggplot(mismatch, aes(x = age_at_recruit , y = age_eczema_gp)) +
  geom_point()

# gp_clinical <- read_table(paste0(datapath, "gp_clinical.txt"))
# saveRDS(gp_clinical, paste0(datapath, "gp_clinical.rds"))

gp_clinical <- readRDS(paste0(datapath, "gp_clinical.rds"))
gp_clinical_sample <- sample(gp_clinical$eid, size = 1e4, replace = F)

gp_clinical_sampleset <- gp_clinical %>% filter(eid %in% gp_clinical_sample)
#saveRDS(gp_clinical_sampleset, paste0(datapath, "gp_clinical_sample.rds"))
gp_clinical_sampleset <- readRDS(paste0(datapath, "gp_clinical_sample.rds"))

medcodes_eczema_dx <- read_csv(here::here("codelist/CPRDgold/medcodes-eczemadx.csv"))
medcodes_psoriasis_dx <- read_csv(here::here("codelist/CPRDgold/medcodes-psoriasis.csv"))
prodcodes_psoriasis_rx <- read_csv(here::here("codelist/CPRDgold/prodcodes-psoriasisrx.csv"))


medcodes_eczema_dx
gp_clinical %>% glimpse()
ad <- gp_clinical %>% 
  filter(read_2 == "M111.")
ad %>% filter(eid == 1957525)
