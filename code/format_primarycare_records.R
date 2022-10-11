library(tidyverse)
library(data.table)
library(skimr)

datapath <- "/Volumes/EHR Group/GPRD_GOLD/Ali/2022_biobank/"

gp_clinical <- read_table(paste0(datapath, "gp_clinical.txt"))
saveRDS(gp_clinical, paste0(datapath, "gp_clinical.rds"))