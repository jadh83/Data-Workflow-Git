### Homework 2 Project ###
# Author: Justin Hill
# Created: 02/06/2021
# Purposes:
# 1) Load school-level demographic data
# 2) Load school-level outcome data
# 3) Develop plan for analyzing the data set
###

## Set up environment
rm(list=ls())

# Load readers
library(readr)
library(readxl)

# Load school enrollment and performance data
penn_sch_enr_race <- read_excel("2018-2019 Enrollment by Race.xlsx")
penn_sch_enr_gender <- read_excel("2018-2019 Enrollment by Gender.xlsx")
penn_sch_per_keystone <- read_excel("2019 Keystone Exams School Level Data.xlsx")
penn_sch_per_PSSA <- read_excel("2019 PSSA School Level Data.xlsx")

# Obtain enrollment data only for Philadelphia
phil_sch_enr_race <- penn_sch_enr_race[which(penn_sch_enr_race[,"County"] == "Philadelphia"),]
phil_sch_enr_gender <- penn_sch_enr_gender[which(penn_sch_enr_gender[,"County"] == "Philadelphia"),]
phil_sch_per_keystone <- penn_sch_per_keystone[which(penn_sch_per_keystone[,"County"] == "Philadelphia"),]
phil_sch_per_PSSA <- penn_sch_per_PSSA[which(penn_sch_per_PSSA[,"County"] == "Philadelphia"),]

# Plan for dataset
# 1) Decide which types of schools / grades will be analyzed
# 2) Condense data sets to only relevant data
# 3) Match school demographic data with school performance data
# 4) Compare outcomes for charter schools and public schools
# 5) Resolve warnings in data