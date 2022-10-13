#' Map CPRD GOLD codelists to Read v2 and Read CTV3 for UK Biobank
#' linked primary care data. 
#' 
#' Using the codelists from CPRD GOLD reasearch as the target
#' 

library(tidyverse)
library(here)
library(readxl)

# Get the mapping file from UK biobank 
dir.create(here::here("codelist/mappingfile"), showWarnings = FALSE)
if(file.exists(here::here("codelist/mappingfile/all_lkps_maps_v3.xlsx"))==FALSE){
  system(paste0("wget -nd  biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip -P ", here::here("codelist", "mappingfile")))
  unzip(here::here("codelist/mappingfile/primarycare_codings.zip"), exdir = here::here("codelist/mappingfile"))  
}

read_mapping <- readxl::read_excel(here::here("codelist/mappingfile/all_lkps_maps_v3.xlsx"), sheet = "read_v2_read_ctv3") %>% 
  janitor::clean_names()
read_mapping$readv2_amended <- paste0(read_mapping$readv2_code, read_mapping$termv2_order)

read_mapping_v3_to_v2 <- readxl::read_excel(here::here("codelist/mappingfile/all_lkps_maps_v3.xlsx"), sheet = "read_ctv3_read_v2") %>% 
  janitor::clean_names()
read_mapping_v3_to_v2$readv2_amended <- paste0(read_mapping_v3_to_v2$readv2_code, read_mapping_v3_to_v2$termv2_order)

# Read in the CPRD GOLD codelists 
medcodes_eczema_dx <- read_csv(here::here("codelist/CPRDgold/medcodes-eczemadx.csv"))
medcodes_psoriasis_dx <- read_csv(here::here("codelist/CPRDgold/medcodes-psoriasis.csv"))

## map readcodes to read CTV3 
eczema_mapped_v2tov3 <- medcodes_eczema_dx %>% 
  left_join(read_mapping, by = c("readcode" = "readv2_amended")) %>% 
  mutate(match_mark = 1)

# only keep the preferred code if there are multiple codes
eczema_mapped_v2tov3_unique <- eczema_mapped_v2tov3 %>% 
  arrange(readv3_code, termv3_type) %>% 
  group_by(readv3_code) %>% 
  slice(1) %>% 
  ungroup()
if(sum(duplicated(eczema_mapped_v2tov3_unique$readv3_code))>0){stop("Duplicate Read CTV3 codes")}


eczema_mapped_v3tov2 <- medcodes_eczema_dx %>% 
  left_join(read_mapping_v3_to_v2, by = c("readcode" = "readv2_amended")) %>% 
  mutate(match_mark = 2)

# only keep the preferred code if there are multiple codes
eczema_mapped_v3tov2_unique <- eczema_mapped_v3tov2 %>% 
  arrange(readv3_code, termv3_type) %>% 
  group_by(readv3_code) %>% 
  slice(1) %>% 
  ungroup()
if(sum(duplicated(eczema_mapped_v3tov2_unique$readv3_code))>0){stop("Duplicate Read CTV3 codes")}

eczema_mapped <- eczema_mapped_v2tov3_unique %>% 
  bind_rows(eczema_mapped_v3tov2_unique) %>% 
  distinct(readcode, readv3_code, .keep_all = TRUE) %>% 
  arrange(readcode) 
write_csv(eczema_mapped, here("eczema_mapped.csv"))

