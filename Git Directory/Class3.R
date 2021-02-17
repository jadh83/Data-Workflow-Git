## Class3 3 - Feb 10, 2021

rm(list=ls())

#install.packages("tidyverse")
library(tidyverse)

md_scs <- read.csv("Data/2019_Accountability_Schools.csv")

## Base R

# Add variable
lss.numbers <- sprintf("%02d", md_scs$LSS.Number)
school.numbers <- sprintf("%04d", md_scs$School.Number)
md_scs$state.id <- paste0(lss.numbers, school.numbers)

# Manipulate variable
md_scs$LSS.Name <- tolower(md_scs$LSS.Name)

# Change variable names
colnames(md_scs) <- gsub(" ", ".", tolower(colnames(md_scs)))


## Dplyr

### RENAME

# analytic <- rename(md_scs, year = Year, lss.number = `LSS Number`)

### SELECT

# Keep variables
select(md_scs, school.name, star.rating, state.id)

# Drop variables
analytic <- select(md_scs, -year)

### MUTATE

# Add variables
analytic <- mutate(md_scs, 
  state.id = paste0(
    sprintf("%02d", lss.number),
    sprintf("%04d", school.number)
  )
)

mutate(md_scs, star.rating = star.rating * 3)

### FILTER

# Add or remove observations
analytic <- filter(md_scs, lss.number == 30, star.rating > 2)

#### Part 2
rm(list=ls())

#install.packages("tidyverse")
library(tidyverse)

analytic_raw <- read.csv("Data/2019_Accountability_Schools.csv")

## Make a plan

# Variables:
# - schid: school Number (integer)
# - schname: School Name (string)
# - rating: Star Rating (integer)
# - totalpts: Total Points Earned Percentage (integer)
# Observations: Baltimore City Schools, Baltimore County Schools

## Select the desired variables

analytic <- select(analytic_raw, `LSS.Number`, `School.Number`, `School.Name`, `Star.Rating`, `Total.Points.Earned.Percentage`)

## Rename variables

colnames(analytic) <- c("lssid", "schid", "schname", "rating", "totalpts")

## Select the desired observations

analytic <- filter(analytic, lssid == 30 | lssid == 3)
analytic <- select(analytic, -lssid)

## Recode variables
analytic <- mutate(analytic, totalpts = totalpts / 100)

## Save dataset
save(analytic, analytic_raw, file = "Data/analytic.Rdata")

#### Part 3

analytic_raw <- mutate_all(analytic_raw, function(x) { ifelse(x == "na", NA, x) })

analytic_raw %>% filter(., complete.cases(.))
