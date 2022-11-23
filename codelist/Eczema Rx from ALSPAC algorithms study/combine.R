eczemaRx_julian <- list.files("codelist/Eczema Rx from ALSPAC algorithms study", 
                              pattern = "codelist", full.names = TRUE) %>% 
  map_dfr(read_csv) %>% 
  mutate(cprd_name_search=stringr::str_remove_all(description, "[^[:alnum:]]"))
setDT(eczemaRx_julian)



#Run 2_format_primary_care_records.R first
new_data_match <- gp_script[drug_name %in% eczemaRx_julian$description]
## Need to get rid of duplicates
data_out_new <- new_data_match %>% 
  dplyr::select(f.eid = eid, data_provider, issue_date, read_2, bnf_code, dmd_code, drug_name) %>% 
  filter(!is.na(issue_date)) %>% 
  mutate(issue_date = as.Date(issue_date, format = "%d/%m/%Y")) %>% 
  group_by(f.eid, issue_date) %>% 
  mutate(prescription_gp = 1:n()) %>%
  slice(1) %>% 
  ungroup()

## append the phototherapy medcodes data (considered as a treatment in the algorithm)
data_allRX_new <- data_out_new %>% 
  dplyr::select(f.eid, data_provider, event_dt = issue_date, desc = drug_name) %>% 
  bind_rows(
    dplyr::select(eczema_medRx_extract,
                  f.eid, data_provider, event_dt, desc)
  ) %>% 
  mutate(data_gp = 1)
(new_data_match)

#Compare exisiting matching with matching with codelists from ASLAPC study
nrow(data_allRX)
nrow(new_data_match)

