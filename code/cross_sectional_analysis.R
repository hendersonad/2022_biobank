library(tidyverse)
library(data.table)
library(ukbtools)
library(irr)
library(skimr)
library(summarytools)
library(cowplot)

dir.create(path = here::here("out"), showWarnings = F)
datapath <- "/Volumes/EHR Group/GPRD_GOLD/Ali/2022_biobank/"


# read in X-sectional cohort ----------------------------------------------


ukb <- read_rds(paste0(datapath, "ukb669156.rds"))
my_ukb_key <- ukb_df_field("ukb669156", path = datapath)


# logistic regressions - eczema/psoriasis with anxiety/depression/ --------
# 





