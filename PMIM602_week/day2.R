#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)

# loading the files

load('Dthdata2.RData')
load('MBSdata2.RData')
load('PBSdata2.RData')
load('diabetes.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create ohgaseq using the first script for an oral hypoglycaemic agent 
# and inspseq using the first script for an insulin preparation 

PBSdata3 <- PBSdata2 %>%
  arrange(rootlpno, morbseq) %>%
  group_by(rootlpno) %>%
  mutate(index=ifelse(diabmed==1, morbseq, NA)) %>%
  mutate(index=min(index, na.rm=TRUE)) %>%
  mutate(ohgaseq=ifelse(morbseq>=index, row_number()-index+1, 0)) %>%
  mutate(index=ifelse(diabmed==2, morbseq, NA)) %>%
  mutate(index=min(index, na.rm=TRUE)) %>%
  mutate(inspseq=ifelse(morbseq>=index, row_number()-index+1, 0)) %>%
  select(-index) %>%
  ungroup()

save(PBSdata3, file = 'PBSdata3.RData')

# Now cut down the file to contain only the rootlpno and dispdate for records 
# where (ohgaseq =  1 and inspseq = 0) 

PBSohga <- PBSdata3 %>% 
  filter(ohgaseq == 1 & inspseq == 0) %>%
  select(rootlpno, dispdate)

save(PBSohga, file = 'PBSohga.RData')

# Merge diabetes platform and PBSohga by rootlpno and rename dispdate to stage2

diabetes2 <- diabetes %>% 
  merge(PBSohga, all.x = TRUE) %>%
  rename(stage2 = dispdate)

save(diabetes2, file = 'diabetes2.RData')

# Cut down PBSdata3 to rootlpno and dispdate for records where inspseq = 1

PBSinsp <- PBSdata3 %>% 
  filter(inspseq == 1) %>%
  select(rootlpno, dispdate)

save(PBSinsp, file = 'PBSinsp.RData')

# Merge diabetes2 platform and PBSinsp by rootlpno and rename dispdate to stage3

diabetes3 <- diabetes2 %>% 
  merge(PBSinsp, all.x = TRUE) %>%
  rename(stage3 = dispdate)

# Merge diabetes 3 and Dthdata2 by rootlpno and rename dthdate to exit 
# Create a new binary indicator variable, dead (1 = dead, 0 = alive)
# For records where dead is set to ‘0’, or for those whose death occurred after 
# 31 December 1999, change exit to 31 December 1999

# diabetes4 <- diabetes3 %>%
#   merge(Dthdata2, all.x = TRUE) %>%
#   rename(exit = dthdate) %>%
#   mutate(dead = ifelse(!is.na(exit), 1, 0))
# 
# for(i in 1:nrow(diabetes4)){
#   if(diabetes4$dead[i] == 0 | diabetes4$exit[i] > as.Date("1999-12-31")){
#     diabetes4$exit[i] <- as.Date("1999-12-31")
#   }  
# }

diabetes4 <- diabetes3 %>% 
  merge((Dthdata2 %>% select(rootlpno, dthdate)), all.x=TRUE) %>%
  rename(exit=dthdate) %>%
  mutate(dead=ifelse(!is.na(exit), 1, 0)) %>%
  mutate(exit=as.Date(ifelse(dead==0 | exit >= as.Date("1999-12-31"),
                             "1999-12-31", as.character(exit))))

save(diabetes4, file = 'diabetes4.RData')

# Calculate the lengths of person-time that each patient spent in each of the 
# three stages of diabetes, as well as the total person-time that each patient 
# contributed to the clinical population during all stages combined.  

diabetes5 <- diabetes4 %>% 
  mutate(stgpt3 = as.numeric(ifelse(!is.na(stage3),exit - stage3 + 1,0))) %>%
  mutate(stgpt2 = as.numeric(ifelse(!is.na(stage2),exit - stage2 + 1 - stgpt3,0))) %>%
  mutate(totpt = as.numeric(exit - date + 1)) %>%
  mutate(stgpt1 = totpt - (stgpt2 + stgpt3)) %>%
  relocate(stgpt3, .after = stgpt2) %>%
  relocate(stgpt1, .before = stgpt2)

# Check for negative person-time values

diabetes5 %>% filter(stgpt1 < 0) %>% nrow()
diabetes5 %>% filter(stgpt2 < 0) %>% nrow()
diabetes5 %>% filter(stgpt3 < 0) %>% nrow()
diabetes5 %>% filter(totpt < 0) %>% nrow()

# Fix the negative person-times and re-run person-time syntax

diabetes5 <- diabetes5 %>%
  mutate(dead=ifelse(totpt<0 | stgpt1<0 | stgpt2<0 | stgpt3<0, 0, dead)) %>%
  mutate(exit=as.Date(
    ifelse(totpt<0 | stgpt1<0 | stgpt2<0 | stgpt3<0, 
           as.Date("1999-12-31", origin="1970-01-01"), exit),
    origin="1970-01-01"))

diabetes5 <- diabetes5 %>% 
  mutate(stgpt3 = as.numeric(ifelse(!is.na(stage3),exit - stage3 + 1,0))) %>%
  mutate(stgpt2 = as.numeric(ifelse(!is.na(stage2),exit - stage2 + 1 - stgpt3,0))) %>%
  mutate(totpt = as.numeric(exit - date + 1)) %>%
  mutate(stgpt1 = totpt - (stgpt2 + stgpt3)) %>%
  relocate(stgpt3, .after = stgpt2) %>%
  relocate(stgpt1, .before = stgpt2)

save(diabetes5, file = 'diabetes5.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check the descriptive statistics

library(psych)
describe(diabetes5 %>% select(stgpt1, stgpt2, stgpt3, totpt))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Estimate point prevalence

date_prev94 <- '1994-06-30'
diabetes5_94 <- diabetes5 %>%
  mutate(prev94=ifelse(date<=date_prev94 & exit>=date_prev94, 1, 0)) %>%
  mutate(prev94=ifelse(!is.na(stage3) & stage3<=date_prev94 & 
                         exit>=date_prev94, 3, prev94)) %>%
  mutate(prev94=ifelse(prev94!=3 & !is.na(stage2) & stage2<=date_prev94 & 
                         exit>=date_prev94, 2, prev94))

date_prev99 <- '1999-06-30'
diabetes5_99 <- diabetes5 %>%
  mutate(prev99=ifelse(date<=date_prev99 & exit>=date_prev99, 1, 0)) %>%
  mutate(prev99=ifelse(!is.na(stage3) & stage3<=date_prev99 & 
                         exit>=date_prev99, 3, prev99)) %>%
  mutate(prev99=ifelse(prev99!=3 & !is.na(stage2) & stage2<=date_prev99 & 
                         exit>=date_prev99, 2, prev99))

table(diabetes5_94$prev94)
table(diabetes5_99$prev99)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Utilisation Rate of Primary Medical Care by Disease Severity 

# Create gpvisit variable which tags MBSdata2 records with an MBS item 
# representing a gp visit

gp_codes <- c(seq(1, 51), seq(160, 164), seq(170, 173), seq(193, 199),
              seq(601, 602), seq(700, 712), seq(720, 779), seq(900, 903),
              seq(2501, 2509), seq(2517, 2526), seq(2546, 2559),
              seq(2574, 2578), seq(2721, 2727), seq(5000, 5267))

MBSdata3 <- MBSdata2 %>%
  ungroup() %>%
  mutate(gpvisit=ifelse(mbsitem %in% gp_codes, 1, 0))

# Merge MBSdata3 to date, stage2, stage3 and exit from the diabetes platform 

# MBSdata3 <- diabetes5 %>%
#   select(rootlpno, date, stage2, stage3, exit) %>%
#   merge(MBSdata3, by='rootlpno', all.x=TRUE)

MBSdata3 <- MBSdata3 %>%
  merge((diabetes5 %>% select(rootlpno, date, stage2, stage3, exit)), 
        by='rootlpno', all.x=TRUE)

save(MBSdata3, file = 'MBSdata3.RData')

# Create  a new variable, stgpvis, which should take on the values of 
# ‘0’ (not diabetic at that time), ‘1’ (visit in stage 1), ‘2’ (visit in stage 2) 
# or ‘3’ (visit in stage 3). 

MBSdata4 <- MBSdata3 %>%
  mutate(stgpvis=ifelse(gpvisit==1, 0, NA)) %>%
  mutate(stgpvis=ifelse(gpvisit==1 & !is.na(servdate) & !is.na(date) & !is.na(exit) & 
                          date<=servdate & exit>=servdate, 1, stgpvis)) %>%
  mutate(stgpvis=ifelse(gpvisit==1 & !is.na(servdate) & !is.na(exit) & !is.na(stage3) &
                          stage3<=servdate & exit>=servdate, 3, stgpvis)) %>%
  mutate(stgpvis=ifelse(gpvisit==1 & !is.na(servdate) & !is.na(exit) & stgpvis!=3 & 
                          !is.na(stage2) & stage2<=servdate & exit>=servdate, 2, stgpvis))

table(MBSdata4$stgpvis)

save(MBSdata4, file = 'MBSdata4.RData')

#  Create three new variables, gpvisit1, gpvisit2 and gpvisit3 and internally 
# load the sums of the GP visits in the three disease stages into the appropriate 
# variable on the first record in each block.

MBSgpst <- MBSdata4 %>%
  select(rootlpno, stgpvis) %>%
  arrange(rootlpno, stgpvis) %>%
  group_by(rootlpno, stgpvis) %>%
  # Get the counts of the GP visits at each stage into the first row of each
  # of the rootlpno/stgpvis groups.
  mutate(gpvisit1=ifelse(stgpvis==1 & row_number()==1, n(), 0)) %>%
  mutate(gpvisit2=ifelse(stgpvis==2 & row_number()==1, n(), 0)) %>%
  mutate(gpvisit3=ifelse(stgpvis==3 & row_number()==1, n(), 0)) %>%
  # Keep just the first row with the counts.
  filter(row_number()==1) %>%
  ungroup()

MBSgpst <- MBSgpst %>%
  group_by(rootlpno) %>%
  # Collapse the maximum values from the stgpvis records to the first record
  # and preventing the NA's being included in the maximum if no value exists
  # i.e. set to zero.
  mutate(gpvisit1=ifelse(is.na(gpvisit1), 0, max(gpvisit1, na.rm=TRUE))) %>%
  mutate(gpvisit2=ifelse(is.na(gpvisit2), 0, max(gpvisit2, na.rm=TRUE))) %>%
  mutate(gpvisit3=ifelse(is.na(gpvisit3), 0, max(gpvisit3, na.rm=TRUE))) %>%
  filter(row_number()==1) %>%
  select(-stgpvis) %>%
  ungroup() %>%
  distinct()

save(MBSgpst, file = 'MBSgpst.RData')

# Merge diabetes5 and MBSgpst by rootlpno.  There are 47 cases with missing 
# values for gpvisit1, gpvisit2 and gpvisit3; these should be recoded to zero. 

diabetes6 <- diabetes5 %>% 
  merge(MBSgpst, by = 'rootlpno', all.x = TRUE)

sum(is.na(diabetes6$gpvisit1)&
    is.na(diabetes6$gpvisit2)&
    is.na(diabetes6$gpvisit3))

diabetes6 <- diabetes6 %>%
  mutate(gpvisit1=ifelse(is.na(gpvisit1), 0, gpvisit1),
         gpvisit2=ifelse(is.na(gpvisit2), 0, gpvisit2),
         gpvisit3=ifelse(is.na(gpvisit3), 0, gpvisit3))

save(diabetes6, file = 'diabetes6.RData')

# Sum the stage-specific GP visits requesting also the means, minima and maxima.  
# Then divide each stage-specific numerator by its corresponding stage-specific 
# person-time denominator to measure the utilisation rate of primary medical 
# care by severity of diabetes.  What are the rates per person year? 

describe(diabetes6 %>% select(gpvisit1, gpvisit2, gpvisit3))
