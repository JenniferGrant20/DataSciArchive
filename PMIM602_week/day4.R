library(tidyverse)

# Opening the files

load('diabetes8.RData')
load('HMDSdata3.RData')
load('PBSdata3.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Identification of Index Subjects Hospitalised for Upper Gastrointestinal 
# Complications 

# Tag HMDSdata3 records that have a UGC diagnosis code as the principal condition 
# treated during hospital stay (ie, diag1), using a new variable, ugc. 

UGC <- c(format(seq(531.00, 535.99, 0.01), digits=6), 
         format(seq(578.00, 578.99, 0.01), digits=6),
         paste("K", format(seq(25.00, 29.99, 0.01), digits=5), sep=""),
         paste("K", format(seq(92.00, 92.29, 0.01), digits=5), sep="")
         )

index <- HMDSdata3 %>% 
  mutate(ugc = ifelse(diag1 %in% UGC, 1, 0))

# Create ugcseq variable that numbers tagged ugc records, filter for
# first-tagged records admitted on or after 31 January 1991, and cut down file

index <- index %>% arrange(rootlpno, fileseq) %>%
  filter(ugc==1 & admdate>='1991-01-31') %>%
  group_by(rootlpno) %>%
  mutate(ugcseq=row_number()) %>%
  ungroup() %>%
  filter(ugcseq==1) %>%
  select(rootlpno, sex, age, admdate)

save(index, file = 'index.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ascertainment of Indomethacin Exposure 

# Tag all PBSdata3 records with an indomethacin PBS item number using a new 
# variable, indometh. Filter for indometh = 1

indomethacin <- c(2454, 2459, 2757, 5126, 5127, 5128)

indexposure <- PBSdata3 %>%
  mutate(indometh = ifelse(pbsitem %in% indomethacin, 1, 0)) %>%
  filter(indometh == 1)

save(indexposure, file = 'indexposure.RData')

# Merge indexposure and index. Delete all the PBS records where admdate remains
# blank as these records do not relate to an index subject with UGC. 

indexposure2 <- indexposure %>%
  merge(index, by = 'rootlpno', all.x = TRUE) %>%
  filter(!is.na(admdate))

save(indexposure2, file = 'indexposure2.RData')

# Create a new variable, exposure. Set it to ‘0’ if dispdate falls neither in 
# the case nor control window; ‘1’ if dispdate falls within the case window of 
# 31 days up to and including admdate; and ‘2’ if it falls within the control 
# window from 395 to 365 days, inclusive, prior to admdate.  
# Filter for records with exposure equal to ‘1’ or ‘2’.   
# HINT: Initially assign the difference in days between admdate and dispdate to 
# exposure; then recode exposure into the three categories using the correct 
# ranges of the difference.

indexposure3 <- indexposure2 %>%
  mutate(exposure_time=admdate-dispdate) %>%
  mutate(exposure=ifelse(exposure_time>0 & exposure_time<=30, 1,
                         ifelse(exposure_time>=365 & exposure_time<=395, 2, 0)))

table(indexposure3$exposure)

indexposure3 <- indexposure3 %>%
  filter(exposure == 1 | exposure == 2)

save(indexposure3, file = 'indexposure3.RData')

# Create casex and conex sequences and cut down file

indexposure4 <- indexposure3 %>%
  arrange(-fileseq) %>%
  group_by(rootlpno, exposure) %>%
  mutate(casexseq=ifelse(exposure==1, row_number(), 0),
         conexseq=ifelse(exposure==2, row_number(), 0)) %>%
  ungroup() %>%
  filter(casexseq==1 | conexseq==1) %>%
  arrange(fileseq) %>%
  select(rootlpno, dispdate, exposure)

save(indexposure4, file = 'indexposure4.RData')

# Separate case and control records

indcasexp <- indexposure4 %>% filter(exposure == 1)
indconexp <- indexposure4 %>% filter(exposure == 2)

save(indcasexp, file = 'indcasexp.RData')
save(indconexp, file = 'indconexp.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analysis of the Case-Crossover Design 

# Merge index with dispdate and exposure variables using the indcasexp file as a 
# look-up table keyed by rootlpno.  
# Rename dispdate to casexdat and rename exposure to casexp, also recoding the 
# values of the latter so that ‘0’ = no case window exposure and ‘1’ = presence 
# of case window exposure.  
# Do the same using the indconexp file and rename dispdate to conexdat and 
# exposure to conexp, also recoding the values of the latter so that ‘0’ = no 
# control window exposure and ‘1’ = presence of control window exposure.  

index1 <- index %>% merge(indcasexp, all.x=TRUE) %>%
  rename(casexdat=dispdate, casexp=exposure) %>%
  mutate(casexp=ifelse(is.na(casexp), 0, 1))

index2 <- index1 %>% merge(indconexp, all.x=TRUE) %>%
  rename(conexdat=dispdate, conexp=exposure) %>%
  mutate(conexp=ifelse(is.na(conexp), 0, 1))

save(index2, file = 'index2.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform a crosstabulation of casexp by conexp.  

xtabs(~casexp+conexp, data=index2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation of a File of Reference Subjects

# Create a random list of admdates

set.seed(2500000)

indexadm <- index2 %>%
  mutate(randsort=runif(nrow(index2), min=0, max=1)) %>%
  arrange(randsort) %>%
  mutate(randseq=row_number()) %>%
  select(randseq, admdate)

save(indexadm, file = 'indexadm.RData')

# Create a random index in the platform data frame

set.seed(3500000)
index_size <- nrow(indexadm)
platform_size <- nrow(diabetes8)
diabetes8 <- diabetes8 %>%
  mutate(randsort=runif(platform_size, min=0, max=1)) %>%
  arrange(randsort) %>%
  mutate(randseq=rep(seq(1:index_size), 
                     ceiling(platform_size/index_size))[1:platform_size])

# Create a random list of admdates for the reference group

reference <- diabetes8 %>% 
  merge(indexadm, all.x=TRUE) %>%
  arrange(rootlpno) %>%
  filter(exit>=admdate) %>%
  select(rootlpno, admdate)

nrow(reference)

save(reference, file = 'reference.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Ascertainment of Indomethacin Exposure in Reference Subjects 

# Open your PBSdata3 file and again tag all records with an indomethacin PBS 
# item number using indometh. Select all of the tagged PBS records and delete 
# the remaining records. 

refexposure <- PBSdata3 %>%
  mutate(indometh = ifelse(pbsitem %in% indomethacin, 1, 0)) %>%
  filter(indometh == 1)

save(refexposure, file = 'refexposure.RData')

# Load the index admission dates from the reference file as a look-up table 
# keyed by rootlpno.  Delete all PBS records where admdate remains blank.

refexposure2 <- refexposure %>%
  merge(reference, all.x=TRUE) %>%
  filter(!is.na(admdate))

# Create a new variable, exposure, and set it to ‘0’, ‘1’ or ‘2’ as before based 
# on the difference between dispdate and admdate.  Select all records with 
# exposure equal to ‘1’ or ‘2’ and delete the remaining records.

refexposure3 <- refexposure2 %>%
  mutate(exposure_time=admdate-dispdate) %>%
  mutate(exposure=ifelse(exposure_time>=0 & exposure_time<=30, 1,
                         ifelse(exposure_time>=365 & exposure_time<=395, 2, 0)))

refexposure3 <- refexposure3 %>% 
  filter(exposure == 1 | exposure == 2)

save(refexposure3, file = 'refexposure3.RData')

# Sequence and cut down file

refexposure4 <- refexposure3 %>%
  arrange(-fileseq) %>%
  group_by(rootlpno, exposure) %>%
  mutate(casexseq=ifelse(exposure==1, row_number(), 0),
         conexseq=ifelse(exposure==2, row_number(), 0)) %>%
  ungroup() %>%
  filter(casexseq==1 | conexseq==1) %>%
  arrange(fileseq) %>%
  select(rootlpno, dispdate, exposure)

nrow(refexposure4)

save(refexposure4, file = 'refexposure4.RData')

# Separate the case and control records

refcasexp <- refexposure4 %>% filter(exposure == 1)
refconexp <- refexposure4 %>% filter(exposure == 2)

save(refcasexp, file = 'refcasexp.RData')
save(refconexp, file = 'refconexp.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analysis of the Case-Time-Control Design 

# Create a file suitable for tabular analysis of cross-over pairs

reference1 <- reference %>%
  merge(refcasexp, all.x = TRUE) %>%
  rename(casexdat=dispdate, casexp=exposure) %>%
  mutate(casexp=ifelse(is.na(casexp), 0, 1))

reference2 <- reference1 %>%
  merge(refconexp, all.x=TRUE) %>%
  rename(conexdat=dispdate, conexp=exposure) %>%
  mutate(conexp=ifelse(is.na(conexp), 0, 1))

save(reference2, file = 'reference2.RData')

# Cross-tabulation

xtabs(~casexp+conexp, data=reference2)
