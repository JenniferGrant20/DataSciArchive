library(tidyverse)

# Opening the files

load('diabetes8.RData')
load('PBSdata3.RData')
load('diabsupp.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ascertainment of Statin Exposure 
# 
# Creating anystatin variable to tag PBSdata3 records with a statin PBS item no.

statins <- c(1224, 1453, 1687, 1942, 2011, 2012, 2013, 2831, 2833, 2834, 2892,
             2967, 2978, 8023, 8024, 8173, 8197, 8213, 8214, 8215, 8303, 8304,
             8313, 8419, 8521, 8721, 8722, 8757)

PBSstatin <- PBSdata3 %>%
  mutate(anystatin = ifelse(pbsitem %in% statins, 1, 0))

PBSstatin %>% filter(anystatin == 1) %>% nrow()

# Create a dispensing sequence variable, statseq, which numbers the statin scripts. 
# Select the first statin script for each patient (statseq=1) and delete the 
# remainder of records. Rename dispdate as statdate and delete all variables in
# the file except rootlpno and statdate. Now select the 3,624 first-time statin 
# scripts and save them as PBSstatin after deleting the remaining records. 

PBSstatin <- PBSstatin %>%
  filter(anystatin == 1) %>%
  arrange(rootlpno, dispdate) %>%
  group_by(rootlpno) %>%
  mutate(statseq = row_number()) %>%
  ungroup() %>%
  filter(statseq == 1) %>%
  rename(statdate = dispdate) %>%
  select(rootlpno, statdate)

save(PBSstatin, file = 'PBSstatin.RData')

# Merge diabetes8 and PBSstatin by rootlpno. Create a new variable, statin, and 
# set a default value of ‘0’. Apply a series of tests to the number of days 
# between statdate and date, such that when statdate precedes date, statin is 
# assigned the value ‘9’; and when statdate falls on or within the next 364 days 
# after date, statin is assigned the value ‘1’.  Delete all the records 
# from your file where statin = 9, leaving 9,713 records of diabetic patients 
# not known to have already been prescribed a statin at the time of their 
# inaugural index date.  The reason for this exclusion is to restrict the study
# domain to diabetic patients prescribed a statin for the first time. 

diabetes9 <- diabetes8 %>%
  merge(PBSstatin, by = 'rootlpno', all.x = TRUE) %>%   
  mutate(statin = 0) %>%
  mutate(statin=ifelse(!is.na(statdate) & (statdate<date), 9, statin)) %>%
  mutate(statin=ifelse((statin!=9) & 
                       (!is.na(statdate)) & 
                       (statdate-date<=364), 1, statin)) %>%
  filter(statin!=9)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Preparation of Additional Covariates 

# Create a year variable by drawing the year off the inaugural index date variable.  
# Now select the 8,263 records where year ≥ 1991 and delete the remaining records 
# from 1990. The reasons for this exclusion are (i) to ensure that all patients 
# in the analysis have at least one full year of hospital morbidity information 
# prior to their diabetes diagnosis on which to base the assessment of 
# comorbidity; and (ii) to strengthen the assumption that all patients prescribed 
# statins prior to their diabetes diagnosis have been excluded. Save as diabetes9.

diabetes9 <- diabetes9 %>%
  mutate(year = year(date)) %>%
  filter(year >= 1991)

save(diabetes9, file = 'diabetes9.RData')

# Load the variables, seifagp, ariagp and macss onto the end of your platform 
# file, using diabsupp from your CD as an external file keyed by rootlpno.

diabetes10 <- diabetes9 %>%
  merge(diabsupp, by = 'rootlpno', all.x = TRUE)

save(diabetes10, file = 'diabetes10.RData')

# Create a new stage variable that is assigned the value ‘1’, ‘2’ or ‘3’ 
# according to whether at the inaugural index date the patient was in clinical 
# stage 1 (no diabetic pharmacotherapy), 2 (oral hypoglycaemic agent, but not 
# insulin) or 3 (insulin). Hint: You can use the stgpt1, stgpt2 and stgpt3 
# variables that you created in exercise 2; eg, if stgpt1 = 0 and stgpt2 =0 and 
# stgpt3 >0, then the patient must have commenced in clinical stage 3. You should
# have 5,489 stage1 patients, 2,291 stage 2 patients and 483 stage 3 patients.

diabetes10 <- diabetes10 %>%
  mutate(stage = ifelse(stgpt1==0 & stgpt2==0 & stgpt3>0, 3,
                        ifelse(stgpt1==0 & stgpt2>0, 2, 1)))
table(diabetes10$stage)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Survival variables

# The analysis will be restricted to a maximum of three years of follow-up to 
# prevent the calendar year in which follow-up commences being correlated with 
# length of follow-up.  
# Create surv3, to become survival time censored at three years, initially by 
# assigning to it the difference in days between exit and date. 
# Also create dead3, to become vital status at three years, initially by giving 
# it the same value as dead.  Two adjustments are then made such that if
# surv3 > 1,095, its value is trimmed back to 1,095 and dead3 is set to ‘0’.  
# Select the 6,795 records with year < 1998 to ensure a minimum of two years of 
# follow-up is available and delete the remaining records.  Limiting the available 
# follow-up to 2.5 years, on average, for new patients in 1997, is considered to
# be an acceptable compromise. 
# Delete all variables except rootlpno, sex, age, statin, year, seifagp, ariagp, 
# macss, stage, surv3 and dead3 and save the file with the name diabstat. 
# Your file in now ready for analysis.

diabstat <- diabetes10 %>%
  mutate(surv3=exit-date, dead3=dead) %>%
  mutate(dead3=ifelse(surv3>1095, 0, dead)) %>%
  mutate(surv3=as.numeric(ifelse(surv3>1095, 1095, surv3))) %>%
  filter(year<1998) %>%
  select(rootlpno, sex, age, statin, year, seifagp, ariagp, macss, stage,
         surv3, dead3)

save(diabstat, file = 'diabstat.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bivariate regression model

# Using Cox regression, run a simple bivariate model that estimates the mortality 
# rate ratio over three years in diabetics newly prescribed a statin in the first
# year after diagnosis compared with other diabetics.  Your results should 
# resemble the following with MRR = 0.61, suggesting a crude protective effect 
# of statins on premature mortality. 

library(survival)

cox5.8 <- coxph(Surv(surv3, dead3)~statin, data=diabstat)
summary(cox5.8)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optimising Covariates for Multivariate Regression Model 

# Before running a multivariate model to adjust the crude effect, some 
# transformations are necessary to optimise the fit of the covariates.  
# This starts with imputing means for missing values for sex (90 instances) and 
# age (9 instances).  Recode these to 0.5 for sex (after also recoding ‘2’ to ‘0’ 
# for females) and 59.2 for age.  

diabstat2 <- diabstat %>%
  mutate(sex=ifelse(sex==2, 0, sex)) %>%
  mutate(sex=ifelse(is.na(sex), 0.5, sex)) %>%
  mutate(age=ifelse(is.na(age), 59.2, age))

# The following information will save you time:
# (i) optimal adjustment for age is thought to be achieved with a (-2, ½) 
# fractional polynomial so replace a plain age covariate with two covariates, 
# one of form 1/X2 (create as variable agem2) and the other √X (create as 
# variable agesqrt); (ii) optimal adjustment for comorbidity is achieved by a 
# binary 0-1 indicator for any comorbidity, anycom, and by the natural log of 
# the MACSS score after adding the constant 0.01 (create as variable macssln – 
# but why did we add the 0.01?); and (iii) create a 0-1 binary indicator, stage2, 
# for patient diagnosed in stage 2 diabetes and another, stage3, for those 
# diagnosed in stage 3.  In addition, (iii) equate a new variable seifa25 to      
# seifagp and recode seifa25 so that the value ‘1’ becomes ‘0’ and values ‘2-5’ 
# become ‘1’; and (iv) equate a new variable aria5 to ariagp and recode aria5 so 
# that values ‘1-4’ become ‘0’ and value ‘5’ becomes ‘1’.  These two
# transformations will simplify your output given that most of the effect of 
# socioeconomic status is expressed as protection in the least disadvantaged 
# group and most of the effect of remoteness is expressed as a higher mortality
# in the most remote group. Save your files with these updates.  

diabstat2 <- diabstat2 %>%
  mutate(agem2=(1/age^2), agesqrt=(sqrt(age))) %>%
  mutate(anycom=ifelse(macss>0, 1, 0))  %>%
  mutate(macssln=(log(macss+0.01))) %>%
  mutate(stage2=ifelse(stage==2, 1, 0), stage3=ifelse(stage==3, 1, 0)) %>%
  mutate(seifa25=ifelse(seifagp==1, 0,
                        ifelse(seifagp %in% 2:5, 1, seifagp))) %>%
  mutate(aria5=ifelse(ariagp %in% 1:4, 0,
                      ifelse(ariagp==5, 1, ariagp)))

save(diabstat2, file = 'diabstat2.RData')

# Run a multivariate model of the effect of statin on three-year mortality with 
# sex, agem2, agesqrt, stage2, stage3, anycom, macssln, seifa25 and aria5 as 
# covariates.  
# Your results should be like the following with the adjusted MRR = 0.73.

cox5.9 <- coxph(Surv(surv3,dead3)~statin+sex+agem2+agesqrt+stage2+stage3+anycom+
                  macssln+seifa25+aria5, 
                data=diabstat2)
summary(cox5.9)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Using Time as an Instrumental Variable 

# Although our results so far are consistent empirically (we ignore statistical 
# precision for the purpose of this exercise) with a protective effect of early
# prescription of a statin on three year mortality in diabetics, further 
# exploratory analysis is warranted. The literature suggests that there has been 
# a large increase in use of statins in Australia since the early 1990s and this 
# can be confirmed in our data by crosstabulating statin by year.  
# The proportion of diabetics prescribed a statin in the first year increased 
# from 2.0% in 1991 to 12.8% in 1997.  Note how the marginal take-up was greater 
# in the second half of the period than in the first half.

xtabs(~year+statin, data=diabstat2)

# Run a bivariate Cox regression model of the effect of year on three-year 
# mortality.  Use the facility available in your statistical package to enter 
# year as a categorical variable represented by a series of binary indicators,
# with the first year of 1991 as the reference category (R users: you may wish 
# to create the binary indicator variables in your data file before fitting the 
# model. Why is there no imperative to include all of the covariates in this 
# regression model?  Check that your results are similar to those below.  
# Where is most of the marginal improvement in mortality concentrated: in the 
# first half of the period or in the second half of the period?  Is this 
# observation consistent or inconsistent with the greatest marginal increase in 
# uptake of statins being in the first half of the period?  What are the 
# limitations of using calendar year as an instrumental variable in these 
# circumstances?

cox5.11 <- coxph(Surv(surv3, dead3)~as.factor(year), data=diabstat2)
summary(cox5.11)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Derivation of Propensity Ranks 

# Given our suspicion from the instrumental variable analysis that the effect of
# early statin prescription may be modified as its use extends to patients with 
# more marginal indications, we will now construct a propensity rank model to 
# stratify patients into those with strong and more marginal indications for use 
# of statins.  To do this, run a logistic regression model with statin as the 
# dependent (binary outcome) variable and sex, age, stage2, stage3, macsss and 
# anycom as the independent variables.  We exclude seifa2-5 and aria5 because 
# the concern here is with clinical decision-making and not with structural 
# barriers to access to treatment.  Note that we do not use the transformations 
# of age and comorbidity from before, as they were tailored to a Cox regression 
# on mortality. 

logistic <- glm(statin~sex+age+stage2+stage3+macss+anycom,
                data=diabstat2, family="binomial")
summary(logistic)

# Use the logistic regression coefficients to construct a propensity rank, 
# propen, for each individual in the file.  This takes the form: 
# propen = β1•sex + β2•age + β3•stage2 + β4•stage3 + β5•macss + β6•anycom 
# Assign these ranks and check that your mean score is 0.365979 with a 
# standard deviation of 0.3338472.  Run a frequency analysis on propen that 
# includes the identification of cut points for quintiles.  Now create a 
# propensity score group variable, propengp, which takes the values ‘1’ to ‘5’ 
# with ‘1’ representing patients in the lowest quintile of propensity to be 
# prescribed a statin.  Run a crosstabulation of statin by propengp to confirm 
# the results shown above.  Not surprisingly, the proportion of patients 
# prescribed a statin early in their illness rises steadily across the 
# propensity score quintiles.  In a traditional propensity score analysis, this
# is the table from which random sampling would occur to achieve a balanced 
# distribution of exposed and non-exposed subjects across each propensity score 
# stratum.  However, in this exercise we will use the propensity score to 
# demonstrate effect modification.

# The coefficients from the model are found in coef(logistic[n]).
diabstat2 <- diabstat2 %>%
  mutate(propen = coef(logistic)[2]*diabstat2$sex +
           coef(logistic)[3]*diabstat2$age +
           coef(logistic)[4]*diabstat2$stage2 +
           coef(logistic)[5]*diabstat2$stage3 +
           coef(logistic)[6]*diabstat2$macss +
           coef(logistic)[7]*diabstat2$anycom)

library(psych)
describe(diabstat2$propen)

quantile(diabstat2$propen, prob=c(.2,.4,.6,.8,1))

diabstat2$propengp <- cut(diabstat2$propen, 
                          quantile(diabstat2$propen, prob=c(0,.2,.4,.6,.8,1)),
                          include.lowest=TRUE,
                          labels = 1:5)

xtabs(~propengp+statin, data=diabstat2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Effect Modification by Propensity Scores 

# To keep things simple we will compare only two collapsed strata of the 
# propensity score; the lowest 40% (patients with lower propensity for statin 
# treatment) with the remainder of patients (average to high propensity).  
# Create a new variable, lopropen, by equating it to propengp. Recode lopropen 
# so that values ‘3-5’ become ‘0’ and values ‘1-2’ become ‘1’ thus making it a 
# binary indicator. Also create an interaction term, intstlo, equal to the 
# product of statin and lopropen. R users: As for Stata, the interaction term 
# can be added directly into the model using the ‘*’ syntax, or created 
# separately first. Rerun your multivariate Cox regression model from step 9, 
# but this time include lopropen and intstlo as covariates. Notice the 
# relatively low p-value for the interaction term (p=0.084). It is uncommon for 
# interactions in Cox and logistic regression models to be strong enough to yield 
# small p-values, even with much larger sample sizes, due to the multiplicative
# rather than additive scaling of these models. This tells us something is up!


diabstat2 <- diabstat2 %>%
  mutate(lopropen=as.numeric(propengp)) %>%
  mutate(lopropen=ifelse(propengp %in% 3:5, 0,
                         ifelse(propengp %in% 1:2, 1, propengp))) %>%
  mutate(intstlo=statin*lopropen)

cox5.13 <- coxph(Surv(surv3,dead3)~statin+lopropen+intstlo+sex+agem2+agesqrt+
                   stage2+stage3+anycom+macssln+seifa25+aria5, data=diabstat2)
summary(cox5.13)

# To better characterise the interaction for the purpose of scientific 
# generalisation, we now remove the lopropen and intstlo covariates from the 
# model and run two separate Cox regressions: one restricted to patients in the 
# lowest 40% of propensity scores (lopropen=1) and the other restricted to 
# patients in the higher 60% (lopropen=0).  Confirm that the effect of statin in 
# the low propensity patients is MRR = 1.54, whereas in the high propensity 
# patients MRR = 0.63.  These compare with our previous adjusted result of 0.73.

cox5.14a <- coxph(Surv(surv3,dead3)~statin+sex+agem2+agesqrt+stage2+stage3+
                    anycom+macssln+seifa25+aria5,
                  data=diabstat2 %>% filter(lopropen==1))
summary(cox5.14a)

cox5.14b <- coxph(Surv(surv3,dead3)~statin+sex+agem2+agesqrt+stage2+stage3+
                    anycom+macssln+seifa25+aria5,
                  data=diabstat2 %>% filter(lopropen==0))
summary(cox5.14b)

# You should now consider the following questions: 
# A. On the basis of these fictitious results alone, what is our best estimate
# of the effect of early use of a statin in the average diabetic with appropriate 
# clinical indications? 
# B. How has the internal validity of this estimate been optimised in the analysis? 
# C. Is it externally valid to generalise the estimate to all diabetics? 
# D. How has the external validity of the estimate of effect been optimised in 
# your analysis? 
