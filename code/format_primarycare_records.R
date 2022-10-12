library(tidyverse)
library(data.table)
library(skimr)

datapath <- "/Volumes/EHR Group/GPRD_GOLD/Ali/2022_biobank/"

dir.create(here::here("data", "temp_store"), showWarnings = FALSE)
system(paste0("wget -nd https://biota.ndph.ox.ac.uk/tabserv.cgi?x=180721904008002d00985d6890ea037fWzQmaSN0PTE2NjU1NzgxMDUmcyNkPWdwX2NsaW5pY2FsJmkjYT03NDMxMSZpI3I9MTIyNTQzXQ== -P ", here::here("data", "temp_store", "gp_clinical.txt"), " -Ogp_clinical.txt"))

clin_test <- read_delim(here::here("temp_store/gp_clinical_test.txt"), delim = "\t")
  
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
