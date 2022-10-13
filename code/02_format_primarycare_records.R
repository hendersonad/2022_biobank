  library(here)
library(tidyverse)
library(data.table)
library(skimr)
library(arrow)

datapath <- "/Volumes/EHR Group/GPRD_GOLD/Ali/2022_biobank/"

# load primary care records -----------------------------------------------
dir.create(paste0(datapath, "primarycare_data"), showWarnings = FALSE)
if(file.exists(paste0(datapath, "primarycare_data/gp_clinical.parquet"))==FALSE){
  print("Need to download the files from UK Biobank. Then convert to Parquet because the text files are BIG")
  #system(paste0("wget -nd -Ogp_clinical.txt https://biota.ndph.ox.ac.uk/tabserv.cgi?x=180721904008002d00985d6890ea037fWzQmaSN0PTE2NjU1NzgxMDUmcyNkPWdwX2NsaW5pY2FsJmkjYT03NDMxMSZpI3I9MTIyNTQzXQ=="))
  #system(paste0("wget -nd -Ogp_scripts.txt https://biota.ndph.ox.ac.uk/tabserv.cgi?x=dc7e693b0c59cb634ec97fa1c6ca99a7WzQmaSN0PTE2NjU1OTM3ODEmcyNkPWdwX3NjcmlwdHMmaSNhPTc0MzExJmkjcj0xMjI1NDNd"))
  gp_clinical <- arrow::read_delim_arrow(here("gp_clinical.txt"), delim = "\t")
  #gp_script <- arrow::read_delim(here("gp_scripts.txt"), delim = "\t")
  arrow::write_parquet(gp_clinical, paste0(datapath, "primarycare_data/gp_clinical.parquet"))
  #arrow::write_parquet(gp_script, paste0(datapath, "primarycare_data/gp_script.parquet"))
}else{
  gp_clinical <- arrow::read_parquet(file = paste0(datapath, "primarycare_data/gp_clinical.parquet"))
  #gp_script <- arrow::read_parquet(file = paste0(datapath, "primarycare_data/gp_script.parquet"))
}

setDT(gp_clinical)

# Save a list of unique IDs in gp_clinical so that I can subset UKBiobank data later to just those with linkage
gp_clinical <- arrow::read_parquet(file = paste0(datapath, "primarycare_data/gp_clinical.parquet"))
linked_ids <- unique(gp_clinical$eid)
readr::write_delim(data.frame(f.eid = linked_ids), file = paste0(datapath, "cohort_data/linkage_ids.txt"))
         
# load mapped codelists  --------------------------------------------------
eczema_mapped <- read_csv(here::here("codelist/eczema_mapped.csv"))
eczema_mapped %>% filter(readv3_code == "X505K")
fn_extract_medcodes <- function(data, read_2_list, read_3_list){
  #find medcodes (to get smaller DT) 
  gp_clinical_tpp <- data[read_3 %in% read_3_list]
  # select first obs by id 
  gp_clinical_tpp <- gp_clinical_tpp[, .SD[1], by = eid]
  
  # repeat for EMIS/Vision data (using READ v2)
  gp_clinical_emisvis <- data[read_2 %in% read_2_list]
  gp_clinical_emisvis <- gp_clinical_emisvis[, .SD[1], by = eid]
  
  return(gp_clinical_subset = rbind(gp_clinical_tpp, gp_clinical_emisvis))
}

eczema_extract <- fn_extract_medcodes(data = gp_clinical, read_2_list = unique(eczema_mapped$readv2_code), read_3_list = unique(eczema_mapped$readv3_code))

eczema_tpp <- eczema_extract %>% 
  filter(data_provider == 3) %>% 
  left_join(eczema_mapped, by = c("read_3" = "readv3_code")) 
eczema_emisvis <- eczema_extract %>% 
  filter(data_provider != 3) %>% 
  left_join(eczema_mapped, by = c("read_2" = "readv2_code"))  
eczema_full <- eczema_emisvis %>% 
  bind_rows(eczema_tpp)
eczema_full <- eczema_full[, .SD[1], by = eid]
eczema_full <- eczema_full %>% 
  mutate(desc = ifelse(data_provider == 3, termv3_desc, readv2_desc)) %>% 
  dplyr::select(f.eid = eid, data_provider, event_dt,read_2, read_3, medcode, desc) %>% 
  mutate(event_dt = as.Date(event_dt, format = "%d/%m/%Y")) %>% 
  mutate(eczema_gp = 1)
saveRDS(eczema_full, "eczema_test.rds")

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
