eczemaRx_julian <- list.files("codelist/Eczema Rx from ALSPAC algorithms study", 
                              pattern = "codelist", full.names = TRUE) %>% 
  map_dfr(read_csv)


#Run 2_

data_match <- gp_script[drug_text %in% eczRx_mapped$cprd_name_search]
