
library(tidyverse)
library(lubridate)
library(magrittr)
library(psych)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exercise 1: part A

load("vashmds.RData")

# Determine record number in data set 

nrow(vashmds)

# Create yearsep variable and determine number of separations by year

vashmds <- mutate(vashmds, yearsep = year(sepdate))
sum(table(vashmds$yearsep))

# Order the rows and create a morbseq variable

vashmds <- vashmds %>% 
           group_by(rootlpno) %>%
           arrange(admdate) %>%
           mutate(morbseq = row_number()) %>%
           ungroup()

# Use morbseq to determine number of patients in data set and number of patients
# with single or multiple hospital records

num_patients <- vashmds %>% 
                filter(morbseq == 1) %>% 
                nrow()
print(num_patients)

num_patients_multi <- vashmds %>% 
                      filter(morbseq == 2) %>% 
                      nrow()
print(num_patients_multi)

# Determine mean patient age at first admission

patient_age_adm1 <- vashmds %>% 
                    filter(morbseq == 1) %>% 
                    summarise(mean = mean(age))
print(patient_age_adm1)

    # or describe(with(vashmds, age[morbseq == 1]))

# Use aggregate command to determine the mean, median and maximum number of 
# hospital records per patient

num_records <- vashmds %>% 
               select(rootlpno) %>% 
               group_by(rootlpno) %>% 
               summarise(count=n())
describe(num_records$count)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exercise 1: part B

load("vascancer.RData")

# Create morbseq variable for vascancer file

vascancer <- vascancer %>%
             group_by(rootlpno) %>%
             arrange(candate) %>%
             mutate(morbseq = row_number()) %>%
             ungroup()

# Assess distribution of morbseq

table(vascancer$morbseq)

# Reconstruct the file as one record per individual

vascancer_tab1 <- vascancer %>% 
                  filter(morbseq == 1) %>% 
                  select(-morbseq) %>%
                  rename(cansite1 = cansite, 
                         cantis1 = cantis, 
                         candate1 = candate)
vascancer_tab2 <- vascancer %>% 
                  filter(morbseq == 2) %>%
                  select(-morbseq) %>%
                  rename(cansite2 = cansite,
                         cantis2 = cantis,
                         candate2 = candate)
vascancer2 <- merge(vascancer_tab1, vascancer_tab2, 
                    x.by = "rootlpno", y.by = "rootlpno",
                    all = TRUE)

# Clearing up environment

rm(vascancer_tab1, vascancer_tab2)

# Saving new data frame

save(vascancer2, file="vascancer2.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~