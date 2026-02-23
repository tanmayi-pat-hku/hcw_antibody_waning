# Healthcare worker cohort data cleaning script - template
# - Data compilation and cleaning
# Author: Wey Wen Lim
# Date: 2022-11-25
# Last updated: 2023-07-14

#####
# SETUP ----
# _Clear environment ----
# {
#     rm(list = ls())
# }
setwd("~/")

# _Load libraries ----
{
  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(readxl)) install.packages("readxl")
  if(!require(lubridate)) install.packages("lubridate")
  if(!require(janitor)) install.packages("janitor")
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(janitor)
}

# _Load raw datasets [NOTE: PLEASE USE LATEST FILE] ----
# {
#     hcw.raw <- hcw.raw
# }

# _Parameters ----
# Time points and survey instruments
{
  #today <- Sys.Date()
  today <- as.Date("2025-12-03")
  s.start <- as.Date("2020-06-01"); s.start
  s.dates <- seq(s.start, today, by = "month"); s.dates
  #s.moabb <- tolower(format(s.dates, format = "%b%y"))[1:(length(s.dates)-1)]; s.moabb # -1 if above 15 of the month
  s.moabb <- tolower(format(s.dates, format = "%b%y"))[1:(length(s.dates)-2)]; s.moabb # -2 if less than 15 of the month
  #
  latest <- c(round = 12, covdos = 8, fluyr = 5, infx = 4)
  #
  rds <- paste("r", 1:latest["round"], sep = "")
  vco <- paste("pv_", 1:latest["covdos"], sep = "")
  vfl <- paste("_f", 1:latest["fluyr"], sep = "")
  xco <- paste("_p", 1:latest["infx"], sep = "")
  baf <- paste(seq(6, 6*latest["round"]-1, by = 6), "m", sep = "")
  
  #Check
  print(today)
  print(s.start)
  print(s.dates)
  print(s.moabb)
  print(latest)
  print(rds)
  print(vco)
  print(vfl)
  print(xco)
  print(baf)
}

#####
# MERGE ----
# {
#    # 1. Merge main and July results and rename variables
#    source("filename.R")
# } # Run only once per analysis

# LOAD MERGED ----
hcw.all <- readRDS(file = "filename.rds") # update file every time
hcw.all <- as_tibble(hcw.all)
hcw.all

#####
# CLEANING ----
# Specify directory
clean.date <- "20251203"
dir <- paste("~/Data/1_cleaned/cleaned_", clean.date, "/", sep = "")

#
# Run:
# 1. Participation and follow-up (followup.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/001_hcw_clean_folo.R")

# 2. Sociodemographics (demographic.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/002_hcw_clean_demo.R")

# 3. Exposure (exposure.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/003_hcw_clean_expo.R")

# 4. Self-reported ARI symptoms (sari.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/004_hcw_clean_sari.R")

# 5. COVID-19 testing by PCR/RAT (testing.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/005_hcw_clean_test.R")

# 6. COVID-19 Vaccination - confirmed by staff (cvaccine.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/006_hcw_clean_covv.R")

# 6.1 COVID-19 Vaccination - self report (cvaccine_srep.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/006_hcw_clean_covvsr.R")

# 7. COVID-19 Post-infection sampling - confirmed by field staff (cinfect.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/007_hcw_clean_covi.R")

# 8. Self-reported health status (healthstat.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/008_hcw_clean_heal.R")

# 9. Monthly follow-up - response rate (month.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/009_hcw_clean_mthq.R")

# 10. Comorbidities
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/010_hcw_clean_comorb.R")

# 12. Lab results (postv1-5.csv/rds, posti1-2.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/012_hcw_clean_labs.R")

# 13. Flu vaccination (fvaccine.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/013_hcw_clean_fluv.R")

# 14. Flu infection (finfect.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/014_hcw_clean_flui.R")

# 15. Health-related behaviors (behave.csv/rds)
rstudioapi::navigateToFile("~/Programs/_General/data_cleaning/015_hcw_clean_behave.R")

# Script ends ----