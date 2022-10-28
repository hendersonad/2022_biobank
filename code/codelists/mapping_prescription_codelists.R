#' Map CPRD GOLD prescription codelists to BNF to
#' linked primary care data. 
#' 
#' Using the codelists from CPRD GOLD reasearch as the target
#' 

library(tidyverse)
library(here)
library(readxl)
source(here::here("file_paths.R"))

# import original CPRD Gold codelists --------------------------------------
prodcodes_eczemaRx <- read_csv(here("codelist/bnf_codelists/prodcodes-eczemarx.csv"))
prodcodes_psoriasisRx <- read_csv(here("codelist/bnf_codelists/prodcodes-psoriasisrx.csv"))
prodcodes_sleepRx <- read_csv(here("codelist/bnf_codelists/prodcodes-sleep.csv"))


# import CPRD's product dictionary  ---------------------------------------
cprd_lkup <- read_delim("/Volumes/SHARED/EHR Share/3 Database guidelines and info/GPRD_Gold/Look up files/Lookups_2022_05/product.txt", delim = "\t") %>% 
  janitor::clean_names()


# import bnf - dmd mapping code from NHSBSA -------------------------------
temp <- tempfile()
download.file("https://www.nhsbsa.nhs.uk/sites/default/files/2022-10/BNF%20Snomed%20Mapping%20data%2020221017.zip",temp)
con <- unzip(temp, exdir = here("codelist/bnf_codelists"))
bnf_dmd_map <- read_xlsx(con)
unlink(temp)

mapping_prescription_codes <- function(cprd_filename, out_filename){
  cprd_codelist <- get(cprd_filename)
  
  # Map across to dmd code using the product.txt look up provided by CPRD --------
  cprd_lkup_subset <- cprd_lkup %>% 
    filter(prodcode %in% cprd_codelist$prodcode) %>% 
    dplyr::select(prodcode, dmdcode, productname, drugsubstance, strength, formulation, route) %>% 
    drop_na()
  
  ## Look at NA matches - why? ----- need to look at this in more detail
  cprd_lkup_na <- cprd_lkup %>% 
    filter(prodcode %in% cprd_codelist$prodcode) %>% 
    dplyr::select(prodcode, dmdcode, productname, drugsubstance, strength, formulation, route) %>% 
    filter(is.na(dmdcode))
  
  #cprd_lkup %>% filter(str_detect(productname, "Methotrexate 25mg/3ml solution for injection pre-filled")) %>% glimpse()
  #cprd_lkup_na %>% arrange(productname) %>% print(n= Inf)
  #cprd_lkup %>% filter(str_detect(productname, "Adalimumab 20mg/0.4ml solution")) %>% glimpse()
  
  # Map DMD across to BNF using the mapping file from NHS -------------------
  bnf_dmd_matches <- bnf_dmd_map %>% 
    janitor::clean_names() %>% 
    filter(snomed_code %in% cprd_lkup_subset$dmdcode) %>% 
    dplyr::select(snomed_code, dm_d_product_description, bnf_code, bnf_name) %>% 
    mutate(dmdcode = as.numeric(snomed_code))
  
  cat(paste("\n",
    "original CPRD prodcodes =", unique(cprd_codelist$prodcode) %>% length(), "\n",
    "original DMD codes =", unique(cprd_lkup_subset$dmdcode) %>% length(), "\n",
    "matched to BNF codes =", bnf_dmd_matches$snomed_code %>% length(), "\n")
  )
  
  # Combine and save --------------------------------------------------------
  prescription_codelist_matched <- cprd_codelist %>% 
    dplyr::select(prodcode, cprd_name = productname) %>% 
    left_join(cprd_lkup_subset, by = "prodcode") %>% 
    left_join(bnf_dmd_matches, by = "dmdcode") %>% 
    dplyr::select(prodcode, dmdcode, bnfcode = bnf_code, everything())
  
  write_csv(prescription_codelist_matched, here("codelist/ukb_mapped_primarycare_codelists", paste0(out_filename, ".csv")))
  prescription_codelist_matched
}
x <- mapping_prescription_codes("prodcodes_psoriasisRx", "prodcodes_psoriasisRx_mapped")
y <- mapping_prescription_codes("prodcodes_eczemaRx", "prodcodes_eczemaRx_mapped")
