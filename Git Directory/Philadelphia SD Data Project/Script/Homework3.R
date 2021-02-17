### Homework 3 Project ###
# Author: Justin Hill
# Created: 02/13/2021
# Purposes:
# 1) Add an R script that manages and cleans the dataset
# 2) Create common identifiers for rows
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
analytic_phil_race <- select(phil_sch_enr_race, -pka, -pkp, -pkf, -k4a, -k4p, -k4f, -k5a, -k5p, -k5f)
analytic_phil_gender <- select(phil_sch_enr_gender, -pka, -pkp, -pkf, -k4a, -k4p, -k4f, -k5a, -k5p, -k5f)

# Plan for dataset
# 1) Decide which types of schools / grades will be analyzed
# 2) Condense data sets to only relevant data
# 3) Match school demographic data with school performance data
# 4) Compare outcomes for charter schools and public schools
# 5) Resolve warnings in data