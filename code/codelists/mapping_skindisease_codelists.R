#' Map CPRD GOLD codelists to Read v2 and Read CTV3 for UK Biobank
#' linked primary care data. 
#' 
#' Using the codelists from CPRD GOLD reasearch as the target
#' 

library(tidyverse)
library(here)
library(readxl)

# Read in the CPRD GOLD codelists  ----------------------------------------
medcodes_eczema_dx <- read_csv(here::here("codelist/CPRDgold/medcodes-eczemadx.csv"))
## terms to exclude in any codelist because they are not 'Atopic' dermatitis - then again what is aye? 
eczema_exclude <- c("seborrhoeic", "contact", "psoriaform", "superficial", "hyperkeratotic")

medcodes_eczema_rx <- read_csv(here::here("codelist/CPRDgold/medcodes-eczemarx.csv")) %>% 
  janitor::clean_names() 
medcodes_eczema_rx$desc <- medcodes_eczema_rx$readterm
eczemaRx_exclude <- c("perineum", "abdomen", "neck", "chest wall", "axilla", "breast","head","pelvis","iodine")
medcodes_psoriasis_dx <- read_csv(here::here("codelist/CPRDgold/medcodes-psoriasis.csv"))
medcodes_psoriasis_dx$desc <- medcodes_psoriasis_dx$readterm

medcodes_anxiety <- read_csv(here("codelist/CPRDgold/medcodes-anxiety-nohistory.csv")) %>% 
  janitor::clean_names() %>% 
  filter(definiteanxiety == 1) %>% ## keep ONLY definite codes
  dplyr::select(medcode, readcode, desc = readterm)

medcodes_depression <- read_csv(here("codelist/CPRDgold/medcodes-depression-nohistory.csv")) %>% 
  janitor::clean_names() %>% 
  filter(definitedepression == 1) %>% ## keep ONLY definite codes
  dplyr::select(medcode, readcode, desc = readterm)

# Get the mapping file from UK biobank  -----------------------------------
dir.create(here::here("codelist/mappingfile"), showWarnings = FALSE)
if(file.exists(here::here("codelist/mappingfile/all_lkps_maps_v3.xlsx"))==FALSE){
  system(paste0("wget -nd  biobank.ndph.ox.ac.uk/ukb/ukb/auxdata/primarycare_codings.zip -P ", here::here("codelist", "mappingfile")))
  unzip(here::here("codelist/mappingfile/primarycare_codings.zip"), exdir = here::here("codelist/mappingfile"))  
}

read_mapping_v2_to_v3 <- readxl::read_excel(here::here("codelist/mappingfile/all_lkps_maps_v3.xlsx"), sheet = "read_v2_read_ctv3") %>% 
  janitor::clean_names()
read_mapping_v2_to_v3$readv2_amended <- paste0(read_mapping_v2_to_v3$readv2_code, read_mapping_v2_to_v3$termv2_order)

read_mapping_v3_to_v2 <- readxl::read_excel(here::here("codelist/mappingfile/all_lkps_maps_v3.xlsx"), sheet = "read_ctv3_read_v2") %>% 
  janitor::clean_names()
read_mapping_v3_to_v2$readv2_amended <- paste0(read_mapping_v3_to_v2$readv2_code, read_mapping_v3_to_v2$termv2_order)


# map readcodes to read CTV3 ----------------------------------------------
map_readcodes <- function(original_list = medcodes_eczema_dx, filename = "eczema_mapped", exclude_words = ".*123"){
  ## check that medcode files have readcode as a variable (and it is called "readcode")
  if(!"readcode" %in% names(original_list)){stop("Original codelist does not have a readcode column (or it's not called readcode)")}
  if(!"desc" %in% names(original_list)){stop("No description column in original codelist (or it's not called 'desc')")}
  
  original_list <- original_list %>% 
    dplyr::select(medcode, readcode, desc)
  
  mapped_v2tov3 <- original_list %>% 
    left_join(read_mapping_v2_to_v3, by = c("readcode" = "readv2_amended")) 
  
  # only keep the preferred code if there are multiple codes
  mapped_v2tov3_unique <- mapped_v2tov3 %>% 
    group_by(readv2_code) %>% 
    arrange(readv2_code, termv2_order) %>% 
    slice(1) %>% 
    ungroup()
  if(sum(duplicated(mapped_v2tov3_unique$readv2_code))>0){stop("Duplicate Read v2 codes")}
  
  mapped_v3tov2 <- original_list %>% 
    left_join(read_mapping_v3_to_v2, by = c("readcode" = "readv2_amended"))
  
  # only keep the preferred code if there are multiple codes
  mapped_v3tov2_unique <- mapped_v3tov2 %>% 
    arrange(readv3_code, termv3_type) %>% 
    group_by(readv3_code) %>% 
    slice(1) %>% 
    ungroup()
  if(sum(duplicated(mapped_v3tov2_unique$readv3_code))>0){stop("Duplicate Read CTV3 codes")}
  
  mapped_codelist <- mapped_v2tov3_unique %>% 
    bind_rows(mapped_v3tov2_unique) %>% 
    distinct(readcode, readv3_code, .keep_all = TRUE) %>% 
    arrange(readcode) 
  
  ## Unsure how the UK Biobank mapping was made exactly but some CTV3 codes were too broad 
  all_words <- str_split(original_list$desc, pattern = boundary("word")) %>% unlist() 
  select_words <- all_words[str_detect(all_words, pattern = "of|and|X|NOS", negate = T)] %>% unique() %>% str_to_lower()
  
  mapped_codelist <- mapped_codelist %>% 
    mutate_at(c("termv2_desc", "termv3_desc"), ~str_to_lower(.)) %>% 
    filter(str_detect(termv3_desc, paste0(select_words, collapse = "|"))) %>% 
    filter(str_detect(termv3_desc, paste0(exclude_words, collapse = "|"), negate = TRUE)) %>% 
    filter(str_detect(termv2_desc, paste0(select_words, collapse = "|"))) 
  
  write_csv(mapped_codelist, here("codelist/ukb_mapped_primarycare_codelists", paste0(filename, ".csv")))
  
  cat("\n", 
      "Number of codes in original codelist: ", length(original_list$readcode), "\n",
      "Number of unique READ V2 codes after mapping: ", length(unique(mapped_codelist$readv2_code)), "\n",
      "Number of unique READ CTV3 codes after mapping: ", length(unique(mapped_codelist$readv3_code)))
}
map_readcodes(original_list = medcodes_eczema_dx, filename = "eczema_mapped", exclude_words = eczema_exclude)
map_readcodes(original_list = medcodes_eczema_rx, filename = "eczema_medcodesRx")
map_readcodes(original_list = medcodes_psoriasis_dx, filename = "psoriasis_mapped")
map_readcodes(original_list = medcodes_anxiety, filename = "anxiety_mapped")
map_readcodes(original_list = medcodes_depression, filename = "depression_mapped")
