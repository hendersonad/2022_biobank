library(here)
library(tidyverse)
library(data.table)
library(skimr)
library(arrow)
source(here::here("file_paths.R"))


# load mapped codelists  --------------------------------------------------
eczema_mapped <- read_csv(here::here("codelist/ukb_mapped_primarycare_codelists/eczema_mapped.csv"))
psoriasis_mapped <- read_csv(here::here("codelist/ukb_mapped_primarycare_codelists/psoriasis_mapped.csv"))
anxiety_mapped <- read_csv(here::here("codelist/ukb_mapped_primarycare_codelists/anxiety_mapped.csv"))
depression_mapped <- read_csv(here::here("codelist/ukb_mapped_primarycare_codelists/depression_mapped.csv"))

eczema_medcodesRx_mapped <- read_csv(here::here("codelist/ukb_mapped_primarycare_codelists/eczema_medcodesRx.csv"))

## Insert prodcodes_psoriasisRx codelist attempts )term search, mapped) 
eczRx_mapped <- read_csv(here::here("codelist/ukb_mapped_primarycare_codelists/prodcodes_eczemaRx_mapped.csv"))
psoRx_mapped <- read_csv(here::here("codelist/ukb_mapped_primarycare_codelists/prodcodes_psoriasisRx_mapped.csv"))

# load primary care records -----------------------------------------------
dir.create(paste0(datapath, "primarycare_data"), showWarnings = FALSE)
if(file.exists(paste0(datapath, "primarycare_data/gp_clinical.parquet"))==FALSE){
  print("Need to download the files from UK Biobank. Then convert to Parquet because the text files are BIG")
  system(paste0("wget -nd -Ogp_clinical.txt https://biota.ndph.ox.ac.uk/tabserv.cgi?x=180721904008002d00985d6890ea037fWzQmaSN0PTE2NjU1NzgxMDUmcyNkPWdwX2NsaW5pY2FsJmkjYT03NDMxMSZpI3I9MTIyNTQzXQ=="))
  system(paste0("wget -nd -Ogp_scripts.txt https://biota.ndph.ox.ac.uk/tabserv.cgi?x=dc7e693b0c59cb634ec97fa1c6ca99a7WzQmaSN0PTE2NjU1OTM3ODEmcyNkPWdwX3NjcmlwdHMmaSNhPTc0MzExJmkjcj0xMjI1NDNd"))
  gp_clinical <- arrow::read_delim_arrow(here("gp_clinical.txt"), delim = "\t")
  gp_script <- arrow::read_delim_arrow(paste0(datapath, "gp_scripts.txt"), delim = "\t")
  arrow::write_parquet(gp_clinical, paste0(datapath, "primarycare_data/gp_clinical.parquet"))
  arrow::write_parquet(gp_script, paste0(datapath, "primarycare_data/gp_script.parquet"))
}else{
  gp_clinical <- arrow::read_parquet(file = paste0(datapath, "primarycare_data/gp_clinical.parquet"))
  gp_script <- arrow::read_parquet(file = paste0(datapath, "primarycare_data/gp_script.parquet"))
}

setDT(gp_clinical)
setDT(gp_script)

# Save a list of unique IDs in gp_clinical so that I can subset UKBiobank data later to just those with linkage
if(file.exists(paste0(datapath, "cohort_data/linkage_ids.txt"))==FALSE){
  gp_clinical <- arrow::read_parquet(file = paste0(datapath, "primarycare_data/gp_clinical.parquet"))
  linked_ids <- unique(gp_clinical$eid)
  readr::write_delim(data.frame(f.eid = linked_ids), file = paste0(datapath, "cohort_data/linkage_ids.txt"))
}else{
  f.eid <- readr::read_delim(paste0(datapath, "cohort_data/linkage_ids.txt"), delim = "\t")
}



# extract Eczema prodcodes from gp_script  ---------------------------------------
eczRx_mapped$cprd_name_search <- stringr::str_remove_all(eczRx_mapped$cprd_name, "[^[:alnum:]]")
data$drug_text <- stringr::str_to_lower(stringr::str_remove_all(data$drug_name, "[^[:alnum:]]"))

data_match <- data[drug_text %in% eczRx_mapped$cprd_name_search]

data_2rows <- data_match[, .SD[c(1, 2)], by = eid] #get first two rows per patid

data_out <- data_2rows %>% 
  dplyr::select(f.eid = eid, data_provider, issue_date, read_2, bnf_code, dmd_code, drug_name) %>% 
  filter(!is.na(issue_date)) %>% 
  mutate(issue_date = as.Date(issue_date, format = "%d/%m/%Y")) %>% 
  group_by(f.eid, issue_date) %>% 
  mutate(prescription_gp = 1:n()) %>%
  slice(1) %>% 
  ungroup()

data_out <- data_out %>% 
  group_by(f.eid) %>% 
  mutate(prescription_gp = 1:n()) %>%
  ungroup()
saveRDS(data_out, paste0(datapath, "primarycare_data/eczema_treatments_ukb.rds"))


# extract medcodes/READ diagnoses -----------------------------------------
fn_extract_medcodes <- function(data, codelist, outname = "null"){
  read_3_list <- codelist %>% dplyr::select(readv3_code) %>% pull() %>% unique()
  read_2_list <- codelist %>% dplyr::select(readv2_code) %>% pull() %>% unique()
  
  #find medcodes (to get smaller DT) 
  gp_clinical_tpp <- data[read_3 %in% read_3_list]
  # select first obs by id 
  gp_clinical_tpp <- gp_clinical_tpp[, .SD[1], by = eid]
  
  # repeat for EMIS/Vision data (using READ v2)
  gp_clinical_emisvis <- data[read_2 %in% read_2_list]
  gp_clinical_emisvis <- gp_clinical_emisvis[, .SD[1], by = eid]
  
  data_tpp <- gp_clinical_tpp %>% 
    left_join(codelist, by = c("read_3" = "readv3_code")) 
  data_emisvis <- gp_clinical_emisvis %>% 
    left_join(codelist, by = c("read_2" = "readv2_code")) 
  data_full <- data_emisvis %>% 
    bind_rows(data_tpp)
  data_full <- data_full[, .SD[1], by = eid]
  
  data_full <- data_full %>% 
    mutate(desc = ifelse(data_provider == 3, termv3_desc, readv2_desc)) %>% 
    dplyr::select(f.eid = eid, data_provider, event_dt,read_2, read_3, medcode, desc) %>% 
    mutate(event_dt = as.Date(event_dt, format = "%d/%m/%Y")) %>% 
    mutate(data_gp = 1)
  saveRDS(data_full, paste0(datapath, "primarycare_data/", outname, ".rds"))
  data_full
}

eczema_extract <- fn_extract_medcodes(data = gp_clinical, codelist = eczema_mapped, outname = "eczema_extract")
eczema_medRx_extract <- fn_extract_medcodes(data = gp_clinical, codelist = eczema_medcodesRx_mapped, outname = "eczema_medcodesRx_extract")
psoriasis_extract <- fn_extract_medcodes(data = gp_clinical, codelist = psoriasis_mapped, outname = "psoriasis_extract")
anxiety_extract <- fn_extract_medcodes(data = gp_clinical, codelist = anxiety_mapped, outname = "anxiety_extract")
depression_extract <- fn_extract_medcodes(data = gp_clinical, codelist = depression_mapped, outname = "depression_extract")

# table of code prevalence in data
read2_tab <- eczema_extract %>% 
  count(read_2, sort = T) %>% 
  left_join(eczema_mapped, by = c("read_2" = "readv2_code")) %>% 
  dplyr::select(read_2, n, readv2_desc) %>% 
  distinct(read_2, n, .keep_all = T) %>% 
  filter(!is.na(read_2)) %>% 
  mutate(pc = signif((n/sum(n))*100, 2))
read2_tab
read3_tab <- eczema_extract %>% 
  count(read_3, sort = T) %>% 
  left_join(eczema_mapped, by = c("read_3" = "readv3_code")) %>% 
  dplyr::select(read_3, n, termv3_desc) %>% 
  distinct(read_3, n, .keep_all = T) %>% 
  filter(!is.na(read_3)) %>% 
  mutate(pc = signif((n/sum(n))*100, 2))
read3_tab %>% 
  bind_rows(read2_tab) %>% 
  mutate(desc = ifelse(is.na(termv3_desc), readv2_desc, termv3_desc)) %>% 
  dplyr::select(read_3, read_2, desc, n, pc) %>% 
  write_csv(file = here::here("out/readCodes_eczema.csv"))





