## PMIM402J 
## Table 1 demographic values
## Student no. 2005070
## Date: 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)

load('cleandata.Rdata')
summary(cleandata)
load('analysisdata.RData')

# Number of individuals in the two intention groups
table(cleandata$intention_to_feed)

# Ethnicity (white)
cleandata %>% filter(intention_to_feed == 1 & ethnicity == 3) %>% nrow()
cleandata %>% filter(intention_to_feed == 0 & ethnicity == 3) %>% nrow()

# Nationality (Welsh/British)
cleandata %>% filter(intention_to_feed == 1 & nationality == 1 |
                       intention_to_feed == 1 & nationality == 2) %>% nrow()
cleandata %>% filter(intention_to_feed == 0 & nationality == 1 |
                       intention_to_feed == 0 & nationality == 2) %>% nrow()

# Smoking history
analysisdata %>% filter(intention_to_feed == 1 & smoking_history == 1) %>% nrow()
analysisdata %>% filter(intention_to_feed == 0 & smoking_history == 1) %>% nrow()

# Previous children
analysisdata %>% filter(intention_to_feed == 1 & other_children == 1) %>% nrow()
analysisdata %>% filter(intention_to_feed == 0 & other_children == 1) %>% nrow()

# BMI
temp1 <- analysisdata %>% filter(intention_to_feed == 1)
temp0 <- analysisdata %>% filter(intention_to_feed == 0) 
mean(temp1$bmi)
sd(temp1$bmi)
mean(temp0$bmi)
sd(temp0$bmi)
mean(analysisdata$bmi)
sd(analysisdata$bmi)

# Relationship status
# Single
analysisdata %>% filter(intention_to_feed == 1 & rltnship_status == 0) %>% nrow()
analysisdata %>% filter(intention_to_feed == 0 & rltnship_status == 0) %>% nrow()
# Dating or engaged
analysisdata %>% filter(intention_to_feed == 1 & rltnship_status == 1) %>% nrow()
analysisdata %>% filter(intention_to_feed == 0 & rltnship_status == 1) %>% nrow()
# Married or living with partner
analysisdata %>% filter(intention_to_feed == 1 & rltnship_status == 2) %>% nrow()
analysisdata %>% filter(intention_to_feed == 0 & rltnship_status == 2) %>% nrow()

# PHQ9 depression severity
summary(temp1$depression_severity)
summary(temp0$depression_severity)

# GAD7 anxiety severity
summary(temp1$anxiety_severity)
summary(temp0$anxiety_severity)

# Level of education
summary(temp1$education)
summary(temp0$education)

# Household income
summary(temp1$household_income)
summary(temp0$household_income)

rm(temp1, temp0)
