#' A small bit of code to convert the big flat csv exported from ukbconv
#' to a smaller parquet file format
#' 
#' ! This code will delete the original csv file which can take a 
#' long time to re-extract if needed. Instructions on how to do this
#' are available here - https://biobank.ctsu.ox.ac.uk/~bbdatan/Data_Access_Guide_v3.0.pdf
#' and need special passwords and access keys. 

library(tidyverse)
library(here)
library(arrow)

full_ukb <- read_csv("Z:/GPRD_GOLD/Ali/2022_biobank/ukb669156.csv")

arrow::write_parquet(full_ukb, "Z:/GPRD_GOLD/Ali/2022_biobank/full_ukbiobank.parquet")

unlink("Z:/GPRD_GOLD/Ali/2022_biobank/ukb669156.csv")

test <- arrow::read_parquet("Z:/GPRD_GOLD/Ali/2022_biobank/full_ukbiobank.parquet")
encoding <- read_table("Z:/GPRD_GOLD/Ali/2022_biobank/encoding.dat")

names(test)
