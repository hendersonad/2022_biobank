#' Create prescription codelists for various variables
#' 
#' Reads in the coding used for the Biobank self-reported medications.
#' Then searches for key terms/ingredients for various variables. 
#' Exports a list of codes to search for when trying to find individuals 
#'who took medication for X condition

library(tidyverse) 

ukb_drugcodes <- read_delim(here::here("codelist/ukb_drugcodes.txt"), delim = "\t")
generic_exclusion_terms <- "hydrochloride|tartrate"
prodcodes_sleep <- read_csv(here::here("codelist/bnf_codelists/prodcodes-sleep.csv"))

sleep_ingredients <- prodcodes_sleep %>% 
  janitor::clean_names() %>% 
  filter(definitedrugs == 1) %>% 
  select(ingredient) %>% 
  mutate(searchterm = str_remove_all(ingredient, generic_exclusion_terms)) %>% 
  mutate(searchterm = str_trim(searchterm, side = "right")) %>% 
  mutate(searchterm = str_replace_all(searchterm, " ", "|")) %>% 
  #mutate(searchterm = str_replace_all(searchterm, "||", "|")) %>% 
  drop_na() %>% 
  pull(searchterm) %>% 
  unique()

searchterm <- paste0(sleep_ingredients, collapse = "|")

ukb_drugcodes %>% 
  filter(str_detect(meaning, pattern = searchterm)) %>% 
  write_csv(here::here("codelist/ukb_insomnia.csv"))
  
