### Homework 5 Project ###
# Author: Justin Hill
# Created: 02/27/2021
# Purposes:
# 1) Compute & Merge statistics
###



## Set up environment and prepare data sets
rm(list=ls())

# Load readers
library(readr)
library(readxl)
library(tidyverse)

# Load school enrollment and performance data
penn_sch_enr_race <- read_excel("2018-2019 Enrollment by Race.xlsx")
penn_sch_per_keystone <- read_excel("2019 Keystone Exams School Level Data.xlsx")

# Obtain enrollment data only for Philadelphia
phil_sch_enr_race <- penn_sch_enr_race[which(penn_sch_enr_race[,"County"] == "Philadelphia"),]
phil_sch_per_keystone <- penn_sch_per_keystone[which(penn_sch_per_keystone[,"County"] == "Philadelphia"),]

# Change column names to lower case and eliminate spaces in column names
colnames(phil_sch_enr_race) <- gsub(" ", ".", tolower(colnames(phil_sch_enr_race)))
colnames(phil_sch_per_keystone) <- gsub(" ", ".", tolower(colnames(phil_sch_per_keystone)))



## Demographic Data Set

# Drop variables and convert student numbers to numeric values
analytic_phil_race <- select(phil_sch_enr_race, aun, lea.name, lea.type, race, '009', '010', '011', '012')
analytic_phil_race$'009' <- as.numeric(analytic_phil_race$'009') 
analytic_phil_race$'010' <- as.numeric(analytic_phil_race$'010') 
analytic_phil_race$'011' <- as.numeric(analytic_phil_race$'011') 
analytic_phil_race$'012' <- as.numeric(analytic_phil_race$'012') 

# Calculate total number of high school age students (grades 9-12) 
analytic_phil_race[is.na(analytic_phil_race)]=0
analytic_phil_race$total <- analytic_phil_race$'009' + analytic_phil_race$'010' + analytic_phil_race$'011' + analytic_phil_race$'012'
analytic_phil_race <- select(analytic_phil_race, aun, lea.name, lea.type, race, total)

# Change demographic info from rows to columns
analytic_phil_race_wide <- pivot_wider(analytic_phil_race, names_from = race, values_from = total)
colnames(analytic_phil_race_wide) <- gsub(" ",".", tolower(colnames(analytic_phil_race_wide)))

# Only keep charter schools with high school age students
analytic_phil_race_wide$total <- rowSums(analytic_phil_race_wide[,4:10])
analytic_phil_race_wide_filter <- filter(analytic_phil_race_wide, total != 0)
analytic_phil_race_final <- filter(analytic_phil_race_wide_filter, lea.type == 'CS')



## Keystone Data Set

# Drop variables and separate into data sets for each subject
analytic_phil_keystone <- select(phil_sch_per_keystone, -grade, -school.number, -county, -district.name, )
analytic_phil_keystone_filter <- filter(analytic_phil_keystone, group == "All Students")
analytic_phil_keystone_filter <- select(analytic_phil_keystone_filter, -group)
analytic_phil_keystone_algebra <- filter(analytic_phil_keystone_filter, subject == "Algebra I")
analytic_phil_keystone_biology <- filter(analytic_phil_keystone_filter, subject == "Biology")
analytic_phil_keystone_literature <- filter(analytic_phil_keystone_filter, subject == "Literature")
analytic_phil_keystone_algebra <- select(analytic_phil_keystone_algebra, -subject)
analytic_phil_keystone_biology <- select(analytic_phil_keystone_biology, -subject)
analytic_phil_keystone_literature <- select(analytic_phil_keystone_literature, -subject)



## Merge data sets

# Convert AUN to numeric values
analytic_phil_race_final$aun <- as.numeric(analytic_phil_race_final$aun)

# Join race and algebra data sets by AUN, eliminate schools with no Keystone data, and drop variables
analytic_algebra <- left_join(analytic_phil_race_final, analytic_phil_keystone_algebra, by = "aun") %>%
  filter(number.scored != "NA") %>% 
  select(-aun, -lea.type, -school.name)

# Join race and biology data sets by AUN, eliminate schools with no Keystone data, and drop variables
analytic_biology <- left_join(analytic_phil_race_final, analytic_phil_keystone_biology, by = "aun") %>%
  filter(number.scored != "NA") %>% 
  select(-aun, -lea.type, -school.name)

# Join race and literature data sets by AUN, eliminate schools with no Keystone data, and drop variables
analytic_literature <- left_join(analytic_phil_race_final, analytic_phil_keystone_literature, by = "aun") %>%
  filter(number.scored != "NA") %>% 
  select(-aun, -lea.type, -school.name)



## Working within merged data set

# Calculate number of students from each school in each scoring category for algebra
analytic_algebra$number.advanced <- with(analytic_algebra, round(number.scored * percent.advanced / 100))
analytic_algebra$number.proficient <- with(analytic_algebra, round(number.scored * percent.proficient / 100))
analytic_algebra$number.basic <- with(analytic_algebra, round(number.scored * percent.basic / 100))
analytic_algebra$number.below.basic <- with(analytic_algebra, round(number.scored * percent.below.basic / 100))

# Calculate number of students from each school in each scoring category for biology
analytic_biology$number.advanced <- with(analytic_biology, round(number.scored * percent.advanced / 100))
analytic_biology$number.proficient <- with(analytic_biology, round(number.scored * percent.proficient / 100))
analytic_biology$number.basic <- with(analytic_biology, round(number.scored * percent.basic / 100))
analytic_biology$number.below.basic <- with(analytic_biology, round(number.scored * percent.below.basic / 100))

# Calculate number of students from each school in each scoring category for literature
analytic_literature$number.advanced <- with(analytic_literature, round(number.scored * percent.advanced / 100))
analytic_literature$number.proficient <- with(analytic_literature, round(number.scored * percent.proficient / 100))
analytic_literature$number.basic <- with(analytic_literature, round(number.scored * percent.basic / 100))
analytic_literature$number.below.basic <- with(analytic_literature, round(number.scored * percent.below.basic / 100))

# Calculate overall percentages within each category for charter schools in Philadelphia
analytic_algebra$overall.percent.advanced <- sum(analytic_algebra$number.advanced) / sum(analytic_algebra$number.scored) * 100
analytic_algebra$overall.percent.proficient <- sum(analytic_algebra$number.proficient) / sum(analytic_algebra$number.scored) * 100
analytic_algebra$overall.percent.basic <- sum(analytic_algebra$number.basic) / sum(analytic_algebra$number.scored) * 100
analytic_algebra$overall.percent.below.basic <- sum(analytic_algebra$number.below.basic) / sum(analytic_algebra$number.scored) * 100
analytic_biology$overall.percent.advanced <- sum(analytic_biology$number.advanced) / sum(analytic_biology$number.scored) * 100
analytic_biology$overall.percent.proficient <- sum(analytic_biology$number.proficient) / sum(analytic_biology$number.scored) * 100
analytic_biology$overall.percent.basic <- sum(analytic_biology$number.basic) / sum(analytic_biology$number.scored) * 100
analytic_biology$overall.percent.below.basic <- sum(analytic_biology$number.below.basic) / sum(analytic_biology$number.scored) * 100
analytic_literature$overall.percent.advanced <- sum(analytic_literature$number.advanced) / sum(analytic_literature$number.scored) * 100
analytic_literature$overall.percent.proficient <- sum(analytic_literature$number.proficient) / sum(analytic_literature$number.scored) * 100
analytic_literature$overall.percent.basic <- sum(analytic_literature$number.basic) / sum(analytic_literature$number.scored) * 100
analytic_literature$overall.percent.below.basic <- sum(analytic_literature$number.below.basic) / sum(analytic_literature$number.scored) * 100

# Calculate difference between school percentages and city-wide percentages
analytic_algebra$diff.percent.advanced <- analytic_algebra$percent.advanced - analytic_algebra$overall.percent.advanced
analytic_algebra$diff.percent.proficient <- analytic_algebra$percent.proficient - analytic_algebra$overall.percent.proficient
analytic_algebra$diff.percent.basic <- analytic_algebra$percent.basic - analytic_algebra$overall.percent.basic
analytic_algebra$diff.percent.below.basic <- analytic_algebra$percent.below.basic - analytic_algebra$overall.percent.below.basic
analytic_biology$diff.percent.advanced <- analytic_biology$percent.advanced - analytic_biology$overall.percent.advanced
analytic_biology$diff.percent.proficient <- analytic_biology$percent.proficient - analytic_biology$overall.percent.proficient
analytic_biology$diff.percent.basic <- analytic_biology$percent.basic - analytic_biology$overall.percent.basic
analytic_biology$diff.percent.below.basic <- analytic_biology$percent.below.basic - analytic_biology$overall.percent.below.basic
analytic_literature$diff.percent.advanced <- analytic_literature$percent.advanced - analytic_literature$overall.percent.advanced
analytic_literature$diff.percent.proficient <- analytic_literature$percent.proficient - analytic_literature$overall.percent.proficient
analytic_literature$diff.percent.basic <- analytic_literature$percent.basic - analytic_literature$overall.percent.basic
analytic_literature$diff.percent.below.basic <- analytic_literature$percent.below.basic - analytic_literature$overall.percent.below.basic



## Plan for dataset

# 1) Resolve warnings in data set
# 2) Compare outcomes by demographic 