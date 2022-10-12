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
system(paste0("wget -nd  biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip -P ", here::here("codelist", "mappingfile")))
unzip(here::here("codelist/mappingfile/primarycare_codings.zip"), exdir = here::here("codelist/mappingfile"))  
read_mapping <- readxl::read_excel(here::here("codelist/mappingfile/all_lkps_maps_v3.xlsx"), sheet = "read_v2_read_ctv3") %>% 
  janitor::clean_names()
read_mapping$readv2_amended <- paste0(read_mapping$readv2_code, read_mapping$termv2_order)
read_mapping %>% filter(readv2_code == "XaILU")

# Read in the CPRD GOLD codelist 
medcodes_eczema_dx <- read_csv(here::here("codelist/CPRDgold/medcodes-eczemadx.csv"))

## map readcodes to read CTV3 
medcodes_eczema_dx %>% 
  left_join(read_mapping, by = c("readcode" = "readv2_amended")) %>% 
  dplyr::select(readv2 = readcode, read_ctv3 = )

medcodes_psoriasis_dx <- read_csv(here::here("codelist/CPRDgold/medcodes-psoriasis.csv"))
prodcodes_psoriasis_rx <- read_csv(here::here("codelist/CPRDgold/prodcodes-psoriasisrx.csv"))

medcodes_eczema_dx %>% head(10)

gp_clinical_sampleset %>% filter(!is.na(read_3)) %>% glimpse()
gp_clinical_sampleset %>% filter(data_provider == 3)
