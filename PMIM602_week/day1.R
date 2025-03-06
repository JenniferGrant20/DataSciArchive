library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# opening MBS file

load('MBSdata.RData')
View(MBSdata)

# creating glyhb variable to tag records with diabetes-related MBS item

mbscodes <- c('66551', '66554', '66557', '73815', '73840', '66319', '66322')

MBSdata2 <- MBSdata %>% mutate(glyhb=ifelse(mbsitem %in% mbscodes, 1, 0))

# creating fileseq

MBSdata2 <- MBSdata2 %>% 
  arrange(rootlpno, servdate, mbsitem) %>%
  mutate(fileseq=row_number())

# creating morbseq

MBSdata2 <- MBSdata2 %>% 
  group_by(rootlpno) %>%
  arrange(servdate, mbsitem) %>%
  mutate(morbseq = row_number()) %>%
  ungroup()

# creating servseq

MBSdata2 <- MBSdata2 %>%
  group_by(rootlpno,glyhb) %>%
  mutate(servseq = ifelse(glyhb == 1, seq_along(rootlpno), NA)) %>% 
  ungroup()

save(MBSdata2, file = 'MBSdata2.RData')

# cutting down the file

MBSdata2 <- rename(MBSdata2, date = servdate)
MBSdata2 <- rename(MBSdata2, sequence = servseq)
MBSdata2$type <- 1
MBSdata2 <- MBSdata2 %>% filter(sequence >= 1)
MBSdata2 %>% filter(sequence == 1) %>% nrow()
MBSdata2 <- MBSdata2 %>% 
  select(rootlpno, date, sex, age, sequence, type)

MBScutdown <- MBSdata2
save(MBScutdown, file = 'MBScutdown.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# opening PBS file

load('PBSdata.RData')
View(PBSdata)

# creating diabmed variable to tag records with diabetes-related PBS item

oralhyp <- c('1202', '1801', '2178', '2430', '2440', '2449', '2607', '2720', 
             '2939', '2940', '8188', '8189', '8391', '8392', '8450', '8451', 
             '8452', '8533', '8535', '8607', '8687', '8688', '8689', '8690', 
             '8691', '8692', '8693', '8694', '8695', '8696', '8810', '8811')
insulin <- c('1425', '1426', '1429', '1430', '1431', '1461', '1462', '1531',
             '1532', '1533', '1534', '1535', '1537', '1591', '1592', '1710', 
             '1711', '1713', '1715', '1716', '1718', '1721', '1722', '1761', 
             '1762', '1763', '2061', '2062', '8006', '8084', '8085', '8212', 
             '8390', '8435', '8571', '8609')

PBSdata2 <- PBSdata %>% 
  mutate(diabmed=ifelse(pbsitem %in% oralhyp, 1, 
                                              ifelse(pbsitem %in% insulin, 2, 0)))

# creating fileseq

PBSdata2 <- PBSdata2 %>% 
  arrange(rootlpno, dispdate, pbsitem) %>%
  mutate(fileseq=row_number())

# creating morbseq

PBSdata2 <- PBSdata2 %>% 
  arrange(rootlpno, dispdate, pbsitem) %>%
  group_by(rootlpno) %>%
  mutate(morbseq = row_number()) %>%
  ungroup()

# creating dispseq

# PBSdata2 <- PBSdata2 %>%
#   group_by(rootlpno,diabmed) %>%
#   mutate(dispseq = ifelse(diabmed == 1 | diabmed == 2, seq_along(rootlpno), NA)) %>%
#   ungroup()

PBSdata2 <- PBSdata2 %>%
  arrange(rootlpno, dispdate, pbsitem) %>%
  group_by(rootlpno) %>%
  # Arrange by rootlpno, then diabmed>0 (- puts TRUE first), then date then
  # item.
  # This works because (diabmed>0) is TRUE or FALSE which have values 1 or 0.
  arrange(rootlpno, -(diabmed>0), dispdate, pbsitem) %>%
  mutate(dispseq=ifelse(diabmed>0, row_number(), NA)) %>%
  ungroup() %>%
  arrange(rootlpno, dispdate, pbsitem)

save(PBSdata2, file = 'PBSdata2.RData')

# cutting down the file

PBSdata2 <- rename(PBSdata2, date = dispdate)
PBSdata2 <- rename(PBSdata2, sequence = dispseq)
PBSdata2$type <- 2
PBSdata2 <- PBSdata2 %>% filter(sequence >= 1)
PBSdata2 <- PBSdata2 %>% 
  select(rootlpno, date, sex, age, sequence, type)
PBSdata2 %>% filter(sequence == 1) %>% nrow()

PBScutdown <- PBSdata2
save(PBScutdown, file = 'PBScutdown.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# opening HMDS file

load('HMDSdata.RData')
View(HMDSdata)

# creating diabetes variable to tag records with diabetes-related code

diabetescodes <- c(format(seq(250.00,250.99, 0.01),nsmall=2),
                   'V77.1',
                   paste("E", format(seq(10.00, 14.99, 0.01), width=4), sep=""))

HMDSdata2 <- HMDSdata %>% mutate(diabetes = 0)

for(i in 1:21){
  HMDSdata2 <- HMDSdata2 %>%
    mutate(diabetes=ifelse(!!sym(paste('diag', i, sep='')) %in% diabetescodes, 1, diabetes))
}

# creating fileseq 

HMDSdata2 <- HMDSdata2 %>% 
  arrange(rootlpno, admdate, sepdate) %>%
  mutate(fileseq=row_number())

# creating morbseq

HMDSdata2 <- HMDSdata2 %>%
  arrange(rootlpno, admdate, sepdate) %>%
  group_by(rootlpno) %>%
  mutate(morbseq = row_number()) %>%
  ungroup()

# creating condseq

# HMDSdata2 <- HMDSdata2 %>%
#   group_by(rootlpno,admdate, sepdate) %>%
#   mutate(
#     condseq = ifelse(diabetes == 1,seq_along(rootlpno),NA)) %>%
#   ungroup()

HMDSdata2 <- HMDSdata2 %>%
  arrange(rootlpno, admdate, sepdate) %>%
  group_by(rootlpno) %>%
  arrange(rootlpno, -(diabetes>0), admdate, sepdate) %>%
  mutate(condseq = ifelse(diabetes>0, row_number(), NA)) %>%
  ungroup() %>%
  arrange(rootlpno, admdate, sepdate)

save(HMDSdata2, file = 'HMDSdata2.RData')

# cutting down the file

HMDSdata2 <- HMDSdata2 %>% rename(date = admdate, sequence = condseq)
HMDSdata2$type <- 3
HMDSdata2 <- HMDSdata2 %>% filter(sequence >= 1) %>%
  select(rootlpno, date, sex, age, sequence, type)
HMDSdata2 %>% filter(sequence == 1) %>% nrow()

HMDScutdown <- HMDSdata2
save(HMDScutdown, file = 'HMDScutdown.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Opening Dthdata file

load('Dthdata.RData')
View(Dthdata)

# creating diabetes variable to tag records with diabetes-related code

diabetescodes <- c(seq(2500, 2509), "V771", paste("E", seq(100, 149), sep=""))

Dthdata2 <- Dthdata %>% mutate(diabetes=ifelse(codcode %in% diabetescodes, 1, 0))

save(Dthdata2, file = 'Dthdata2.RData')

# cutting down the file

Dthdata2 <- Dthdata2 %>% rename(date = dthdate)
Dthdata2$sequence <- 1
Dthdata2$type <- 4
Dthdata2 <- Dthdata2 %>% filter(diabetes == 1) %>% 
  select(rootlpno, date, sex, age, sequence, type)

Dthcutdown <- Dthdata2
save(Dthcutdown, file = 'Dthcutdown.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# creating the antepenultimate platform file

load('MBScutdown.RData')
load('PBScutdown.RData')
load('HMDScutdown.RData')
load('Dthcutdown.RData')

diabantepenult <- MBScutdown %>% 
  rbind(PBScutdown, HMDScutdown, Dthcutdown)

diabantepenult <- diabantepenult %>% 
  arrange(rootlpno, date) %>%
  mutate(fileseq=row_number())

diabantepenult <- diabantepenult %>%
  arrange(rootlpno, date) %>%
  group_by(rootlpno) %>%
  mutate(morbseq = row_number()) %>%
  ungroup()

save(diabantepenult, file = "diabantepenult.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# creating the penultimate platform file

diabpenult <- diabantepenult %>%
  filter(sequence==1) %>%
  select(-sequence)

diabpenult <- diabpenult %>%
  arrange(rootlpno, date, type) %>%
  mutate(fileseq=row_number())

diabpenult <- diabpenult %>%
  arrange(rootlpno, date, type) %>%
  group_by(rootlpno) %>%
  mutate(morbseq = row_number()) %>%
  ungroup()

save(diabpenult, file = 'diabpenult.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# creating the ultimate platform file

diabetes <- diabpenult %>%
  arrange(rootlpno, date) %>%
  filter(morbseq==1) %>%
  select(-morbseq)  %>%
  mutate(fileseq=row_number())

save(diabetes, file = 'diabetes.RData')

# getting the frequencies of each type

table(diabetes$type)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# backcasting correction for incidence estimation

# creating yearinc variable 

library(lubridate)

diabantepenult <- diabantepenult %>%
  mutate(yearinc=year(date))

table((diabantepenult %>% filter(morbseq==1))$yearinc)

# survival analysis

diabantepenult <- diabantepenult %>%
  mutate(previous=ifelse(morbseq>1, 1, 0))

diabantepenult <- diabantepenult %>%
  mutate(revsurti=ifelse(previous==0, interval(as.Date('1989-12-31'), date) %/% days(1), NA))

diabantepenult <- diabantepenult %>%
  mutate(revsurti=ifelse(previous==0, revsurti, interval(lag(date), date) %/% days(1)))

library(survival)

surv1.21 <- survfit(Surv(ceiling(revsurti / 90), previous) ~ 1, conf.int=0,
                    data=diabantepenult)
plot(surv1.21, ylim=c(0,1), xscale=1/90, mark.time=FALSE,
     main="Year of Incidence - Diabetes",
     xlab="Days", ylab="Cumulative Survival")

summary(surv1.21)

library(survminer)

ggsurvplot(surv1.21, conf.int=FALSE, ylim=c(0, 1), xscale=1/90,
           title='Year of Incidence - Diabetes',
           xlab='Days', ylab='Cumulative Survival',
           legend='none'
           )

# applying the correction factors

diabantepenult <- diabantepenult %>%
  mutate(cx=ifelse(revsurti>=2160, 1,
                   0.04 / (1.017 * (revsurti ^ -0.438))))
diabantepenult <- diabantepenult %>%
  mutate(cx=ifelse(cx>=1, 1, ifelse(cx<=0, 0, cx)))

xtabs(~yearinc, data=(diabantepenult %>% filter(morbseq==1))) %>% round(digits=0)

xtabs(cx~yearinc, data=(diabantepenult %>% filter(morbseq==1))) %>% round(digits=0)

# capture-recapture correction for period prevalence estimation

diabpenult <- diabpenult %>%
  arrange(fileseq)

mbs <- diabpenult %>%
  group_by(rootlpno) %>%
  mutate(mbs=ifelse(any(type==1), 1, 0)) %>%
  ungroup() %>%
  distinct(rootlpno, mbs)

pbs <- diabpenult %>%
  group_by(rootlpno) %>%
  mutate(pbs=ifelse(any(type==2), 1, 0)) %>%
  ungroup() %>%
  distinct(rootlpno, pbs)

hmds <- diabpenult %>%
  group_by(rootlpno) %>%
  mutate(hmds=ifelse(any(type==3), 1, 0)) %>%
  ungroup() %>%
  distinct(rootlpno, hmds)

dth <- diabpenult %>%
  group_by(rootlpno) %>%
  mutate(dth=ifelse(any(type==4), 1, 0)) %>%
  ungroup() %>%
  distinct(rootlpno, dth)

diabpenult <- diabpenult %>%
  merge(mbs) %>% merge(pbs) %>% merge(hmds) %>% merge(dth)

diabpenult <- diabpenult %>%
  mutate(cth=ifelse(mbs==1 | pbs==1, 1, 0)) %>%
  mutate(state=ifelse(hmds==1 | dth==1, 1, 0))

# performing the cross tabulation

xtabs(~cth+state, data=(diabpenult %>% filter(morbseq==1)))

ftable(xtabs(~mbs+pbs+hmds+dth, data=(diabpenult %>% filter(morbseq==1))))
