## Class 4 - Feb 17, 2021

rm(list=ls())

#install.packages("tidyverse")
library(tidyverse)
library(readxl)

### Part 1: Merge Data

## Make a plan
# - schid
# - schname
# - gradeband
# - pew_swd
# - per_el
# - star_rating
# Observations: Traditional schools in Baltimore City schools

# Import, select, and rename data

md_sch_report_cards <- read_csv("Data/2019_Accountability_Schools.csv") %>%
  filter(`LSS Number` == 30) %>% # Baltimore City
  select(`School Number`, `School Name`, `Star Rating`) %>%
  
colnames(md_sch_report_cards) <- c("schid", "schname", "star_rating") 

bmore_sch_enrollment <- read_excel("Data/2018-19-CitySchoolsSchoolandDistrictLevelEnrollment.xlsx") %>%
  select(`School Number`,`School Name`, `Gradeband`, `School Type`, `% SWD`, `% EL`)

colnames(bmore_sch_enrollment) <- c("schid", "schname", "gradeband", "schtype", "per_swd", "per_el")

# Munge & clean data

bmore_sch_enrollment <- bmore_sch_enrollment %>%
  filter(
    schid != "A",
    schtype == "Traditional"
  ) %>%
  select (-schtype) %>%
  mutate(
    per_swd = per_swd*100,
    per_el = per_el*100
  )

## Merging

#left_join(bmore_sch_enrollment, md_sch_report_cards, by = "schid")

# Are any rows not complete cases?
# filter(analytic, !complete.cases(analytic))