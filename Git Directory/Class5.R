## Class 5 - Feb 24, 2021

rm(list=ls())

#install.packages("tidyverse")
library(tidyverse)
library(readxl)

# load data
load("Data/analytic.Rdata.Rdata")

### Part 2: Aggregating Data ###

## Simple aggregates
mean(analytic$star_rating, na.rm=TRUE)
median(analytic$star_rating)
quantile(analytic$star_rating, probs=c(0.1, 0.25, 0.5, 0.75, 0.9))
sd(analytic$star_rating)

first(analytic$star_rating)
max(analytic$star_rating, na.rm=TRUE)
min(analytic$star_rating, na.rm=TRUE)

paste0(analytic$star_rating, collapse=";")

# example

analytic$avg_star_rating <- mean(analytic$star_rating)
analytic$star_rating_centered <- analytic$star_rating - analytic$avg_star_rating

## Complex aggregates

analytic_gradeband_summary <- analytic %>%
  group_by(gradeband) %>%
  summarize(
    avg_gradeband_star_rating = mean(star_rating)
  ) 

analytic <- left_join(analytic, analytic_gradeband_summary, by="gradeband")

analytic %>%
  group_by(star_rating) %>%
  summarize(
    avg_per_swd_by_star_rating = mean(per_swd),
    avg_per_el_by_star_rating = mean(per_el),
    n()
  ) 


# Setup environment
rm(list=ls())

#install.packages("tidyverse")
library(tidyverse)
library(readxl)

### Part 3: Revisiting Merging ###

# Generate random numbers between 10 and 20
set.seed(032788)
runif(100)*10+10

M <- 100
N <- 10000

school <- data.frame(
  schid = 1:M,
  title1 = as.integer(runif(M) > 0.5)
)

students <- data.frame(
  stid = 1:N,
  schid = ceiling(runif(N)*100),
  #schid = rep(1:(N/M), each=M),
  female = as.integer(runif(N) > 0.48)
)

school <- filter(school, runif(M) < 0.9)
students <- filter(students, runif(N) < 0.9)

## Merge

analytic1 <- left_join(students, school, by="schid")
analytic2 <- left_join(school, students, by="schid")
analytic3 <- full_join(school, students, by="schid")
analytic4 <- anti_join(students, school, by="schid")

