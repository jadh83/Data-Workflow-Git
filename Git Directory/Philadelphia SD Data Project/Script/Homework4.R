### Homework 4 Project ###
# Author: Justin Hill
# Created: 02/20/2021
# Purposes:
# 1) Merge datasets
###

## Set up environment
rm(list=ls())

# Load readers
library(readr)
library(readxl)
library(tidyverse)

# Load school enrollment and performance data
penn_sch_enr_race <- read_excel("2018-2019 Enrollment by Race.xlsx")
penn_sch_enr_gender <- read_excel("2018-2019 Enrollment by Gender.xlsx")
penn_sch_per_keystone <- read_excel("2019 Keystone Exams School Level Data.xlsx")
penn_sch_per_pssa <- read_excel("2019 PSSA School Level Data.xlsx")

# Obtain enrollment data only for Philadelphia
phil_sch_enr_race <- penn_sch_enr_race[which(penn_sch_enr_race[,"County"] == "Philadelphia"),]
phil_sch_enr_gender <- penn_sch_enr_gender[which(penn_sch_enr_gender[,"County"] == "Philadelphia"),]
phil_sch_per_keystone <- penn_sch_per_keystone[which(penn_sch_per_keystone[,"County"] == "Philadelphia"),]
phil_sch_per_pssa <- penn_sch_per_pssa[which(penn_sch_per_pssa[,"County"] == "Philadelphia"),]

# Change column names to lower case and eliminate spaces in column names
colnames(phil_sch_enr_race) <- gsub(" ", ".", tolower(colnames(phil_sch_enr_race)))
colnames(phil_sch_enr_gender) <- gsub(" ", ".", tolower(colnames(phil_sch_enr_gender)))
colnames(phil_sch_per_keystone) <- gsub(" ", ".", tolower(colnames(phil_sch_per_keystone)))
colnames(phil_sch_per_pssa) <- gsub(" ", ".", tolower(colnames(phil_sch_per_pssa)))

# Drop variables
analytic_phil_gender <- select(phil_sch_enr_gender, aun, lea.name, lea.type, school.number, school.name) %>%
  distinct(school.number, .keep_all = TRUE)

# Filter Keystone results
analytic_phil_keystone <- filter(phil_sch_per_keystone, subject == "Algebra I", group == "All Students")

# Add zeros to match school numbers
analytic_phil_gender$school.number <- paste0("00000", analytic_phil_gender$school.number)

# Merge datasets
analytic <- left_join(analytic_phil_keystone, analytic_phil_gender, by = "school.number")

# Drop variables in merged dataset
analytic <- select(analytic, school.number, school.name.x, lea.type, number.scored, 
                   percent.advanced, percent.proficient, percent.basic, percent.below.basic)


# Plan for dataset
# 1) Decide which types of schools / grades will be analyzed
# 2) Condense data sets to only relevant data
# 3) Match school demographic data with school performance data
# 4) Compare outcomes for charter schools and public schools
# 5) Resolve warnings in data