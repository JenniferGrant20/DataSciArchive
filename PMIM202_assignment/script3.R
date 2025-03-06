## PMIM402J Script 3
## Question 2: Do specific factors affect the duration of breastfeeding? 
## Survival analyses
## Student no. 2005070 
## Date: 18/06/24

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(ggplot2)
library(survival) # For survival analysis
library(survminer) # For survival analysis
library(gtsummary) # For tbl_regression()

## Loading the data
load('analysisdata.RData')
followup <- read.csv('Born in Wales Follow-up Data June 2024.csv')

# There are two individuals with the same ID '396' but have different anxiety
# and depression scores (and referring back to the full dataset, they also had
# different expected delivery dates and therefore are not duplicates)
# This is a problem only for this analysis as the ID is necessary for merging 
# the two data frames.
# As it is unclear which of the follow-up '396' IDs belongs to which '396' in 
# the main data frame, they will be removed from the survival analyses

followup <- followup %>% # renaming the followup variables
  rename(id = STUDY_ID, 
         followup = Follow.Up.Time, 
         stopped_bf = Stopped.Breastfeeding)

survivaldata <- analysisdata %>%
  filter(id != "396") %>% # removing the problematic rows
  left_join(followup, by = "id") %>% # merging the data frames
  select(-id, -rltnship_status, -anxiety_severity, -education) 
  # removed the unnecessary variables

## Recoding the low mood / depression variable into two levels
## (0 = no low mood/depression, 1 = low mood/depressive symptoms)

table(survivaldata$depression_severity)
survivaldata$depression_severity <- ifelse(survivaldata$depression_severity 
                                           == "None", 0, 1)
survivaldata$depression_severity <- factor(survivaldata$depression_severity,
                                           levels = c(0, 1),
                                           labels = c("No", "Yes"))
table(survivaldata$depression_severity) # Checking the numbers add up


## Recoding the household income variable into two levels with low income 
## categorised as up to £20,000 for the purpose of this analysis
## (0 = not low income, 1 = low income)

table(survivaldata$household_income)
survivaldata$household_income <- ifelse(survivaldata$household_income ==
                                        "Less than £10,000" |
                                        survivaldata$household_income ==
                                        "Between £10k and £20k", 1, 0)
survivaldata$household_income <- factor(survivaldata$household_income,
                                           levels = c(0, 1),
                                           labels = c("No", "Yes"))
table(survivaldata$household_income) # Checking the numbers add up

## Renaming the two recoded variables above

survivaldata <- survivaldata %>%
  rename(low_mood = depression_severity,
         low_income = household_income)

## Recoding BMI from a continuous numeric variable into a categorical factor
## (categorised as underweight, healthy, overweight, or obese according to
## NHS guidelines: https://www.nhsinform.scot/healthy-living/food-and-nutrition/healthy-eating-and-weight-loss/body-mass-index-bmi/)

survivaldata$bmi <- cut(
  survivaldata$bmi,
  breaks = c(0, 18.5, 24.9, 29.9, Inf),
  labels = c("Underweight", "Healthy", "Overweight", "Obese"),
  right  = TRUE
)

## Overview of the number of women who stopped breastfeeding during the followup

status_table <- table(survivaldata$stopped_bf)
row.names(status_table) <- c("stopped breastfeeding = 0",
                             "stopped breastfeeding = 1")
status_table
(status_table[1] / (status_table[1] + status_table[2])) * 100
## only 9.16% did not stop breastfeeding during the followup

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Survival analysis with all variables as an initial overview

fit1 <- survfit(Surv(followup, stopped_bf) ~ 1, data = survivaldata)
print(fit1)
summary(fit1)

ggsurvplot(fit1, # Plotting the curve
           conf.int = TRUE,
           risk.table = TRUE, 
           surv.median.line = "hv", 
           ggtheme = theme_minimal())

## Multivariable cox regression

cox1 <- coxph(Surv(followup, stopped_bf) ~ low_mood + smoking_history + 
                other_children + low_income + bmi, data = survivaldata)
summary(cox1)

zph <- cox.zph(cox1) 
print(zph) # Checking the proportional hazards assumption
ggcoxzph(zph) # Looking at the plotted Schoenfeld residuals

tbl_regression(cox1, exponentiate = TRUE) # Results table

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Low mood survival analysis

fit2 <- survfit(Surv(followup, stopped_bf) ~ low_mood, data = survivaldata)
print(fit2)
summary(fit2)

ggsurvplot(fit2, # Survival curve
           # pval = TRUE, 
           conf.int = TRUE,
           xlab = "Days",
           risk.table = "abs_pct", 
           risk.table.col = "strata", 
           linetype = "strata", 
           # surv.median.line = "hv", 
           ggtheme = theme_minimal(),
           legend.labs = c("No low mood", 
                           "Low mood"), 
           palette = c("cornflowerblue", "springgreen4"))

## Checking the log rank is significant
survdiff(Surv(followup, stopped_bf) ~ low_mood, data = survivaldata)

## Cox regression
cox2 <- coxph(Surv(followup, stopped_bf) ~ low_mood, data = survivaldata)
summary(cox2)
ggcoxzph(cox.zph(cox2)) # Schoenfeld residuals

tbl_regression(cox2, exponentiate = TRUE) # Results table

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## BMI survival analysis

fit3 <- survfit(Surv(followup, stopped_bf) ~ bmi, data = survivaldata)
print(fit3)
summary(fit3)

ggsurvplot(fit3, # Survival curve
           # pval = TRUE, 
           conf.int = TRUE,
           xlab = "Days",
           risk.table = "abs_pct",
           risk.table.col = "strata",
           linetype = "strata", 
           # surv.median.line = "hv", 
           ggtheme = theme_minimal(),
           legend.labs = c("Underweight", "Healthy weight", "Overweight",
                           "Obese"), 
           palette = c("cornflowerblue", "palegreen3", "springgreen4", 
                       "darkgreen"))

## Log rank
survdiff(Surv(followup, stopped_bf) ~ bmi, data = survivaldata)

## Cox regression
cox3 <- coxph(Surv(followup, stopped_bf) ~ bmi, data = survivaldata)
summary(cox3)
ggcoxzph(cox.zph(cox3)) # Schoenfeld residuals

tbl_regression(cox3, exponentiate = TRUE) # Results table

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Smoking status survival analysis

fit4 <- survfit(Surv(followup, stopped_bf) ~ smoking_history, 
                data = survivaldata)
print(fit4)
summary(fit4)

ggsurvplot(fit4, # Survival curve
           # pval = TRUE, 
           conf.int = TRUE,
           xlab = "Days",
           risk.table = "abs_pct",
           risk.table.col = "strata",
           linetype = "strata", 
           # surv.median.line = "hv", 
           ggtheme = theme_minimal(),
           legend.labs = c("No history of smoking", "Has history of smoking"), 
           palette = c("cornflowerblue", "springgreen4"))

## Log rank
survdiff(Surv(followup, stopped_bf) ~ smoking_history, data = survivaldata)

## Cox regression
cox4 <- coxph(Surv(followup, stopped_bf) ~ smoking_history, 
              data = survivaldata)
summary(cox4)
ggcoxzph(cox.zph(cox4)) # Schoenfeld residuals

tbl_regression(cox4, exponentiate = TRUE) # Results table

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Low income survival analysis

fit5 <- survfit(Surv(followup, stopped_bf) ~ low_income, data = survivaldata)
print(fit5)
summary(fit5)

ggsurvplot(fit5, # Survival curve
           # pval = TRUE, 
           conf.int = TRUE,
           xlab = "Days",
           # risk.table = "abs_pct", 
           # risk.table.col = "strata", 
           linetype = "strata", 
           # surv.median.line = "hv", 
           ggtheme = theme_minimal(),
           legend.labs = c("Household income £20k+",
                           "Household income below £20k"), 
           palette = c("cornflowerblue", "springgreen4"))

## Log rank
survdiff(Surv(followup, stopped_bf) ~ low_income, data = survivaldata)

## Cox regression 
cox5 <- coxph(Surv(followup, stopped_bf) ~ low_income, data = survivaldata)
summary(cox5)
ggcoxzph(cox.zph(cox5)) # Schoenfeld residuals

tbl_regression(cox5, exponentiate = TRUE) # Results table

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Parity survival analysis

fit6 <- survfit(Surv(followup, stopped_bf) ~ other_children, 
                data = survivaldata)
print(fit6)
summary(fit6)

ggsurvplot(fit6, # Survival curve
           # pval = TRUE, 
           conf.int = TRUE,
           xlab = "Days",
           risk.table = "abs_pct",
           risk.table.col = "strata",
           linetype = "strata", 
           # surv.median.line = "hv", 
           ggtheme = theme_minimal(),
           legend.labs = c("No previous child", "Has previous child"), 
           palette = c("cornflowerblue", "springgreen4"))

## Log rank
survdiff(Surv(followup, stopped_bf) ~ other_children, data = survivaldata)

## Cox regression
cox6 <- coxph(Surv(followup, stopped_bf) ~ other_children, data = survivaldata)
summary(cox6)
ggcoxzph(cox.zph(cox6)) # Schoenfeld residuals

tbl_regression(cox6, exponentiate = TRUE) # Results table
