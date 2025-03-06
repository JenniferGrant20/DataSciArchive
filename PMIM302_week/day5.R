
library(tidyverse)
library(survival)
library(survminer)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exercise 5

load('bphsurgery.RData')

# Tag records with TURP or OP

bphsurgery <- mutate(bphsurgery, yearsep = year(sepdate))

bphsurgery2 <- bphsurgery %>%
  mutate(bphsurg=NA) %>%
  mutate(bphsurg=ifelse(!is.na(proc1) & (yearsep < 1988) & 
                        (proc1 %in% 56.01), 1, 
                      ifelse(!is.na(proc1) & (yearsep < 1988) & 
                               (proc1 %in% seq(56.02, 56.05, 0.01)),
                             0, bphsurg))) %>%
  mutate(bphsurg=ifelse(!is.na(proc2) & (yearsep < 1988) & 
                        (proc2 %in% 56.01), 1,
                      ifelse(!is.na(proc2) & (yearsep < 1988) & 
                               (proc2 %in% seq(56.02, 56.05, 0.01)), 
                             0, bphsurg))) %>%
  mutate(bphsurg=ifelse(!is.na(proc3) & (yearsep < 1988) & 
                        (proc3 %in% 56.01), 1,
                      ifelse(!is.na(proc3) & (yearsep < 1988) & 
                               (proc3 %in% seq(56.02, 56.05, 0.01)),
                             0, bphsurg))) %>%
  mutate(bphsurg=ifelse(!is.na(proc1) & (yearsep > 1987) & 
                        (proc1 >= 60.20 & proc1 <= 60.29), 1,
                      ifelse(!is.na(proc1) & (yearsep > 1987) & 
                               (proc1 >= 60.30 & proc1 <= 60.69), 
                             0, bphsurg))) %>%
  mutate(bphsurg=ifelse(!is.na(proc2) & (yearsep > 1987) & 
                        (proc2 >= 60.20 & proc2 <= 60.29), 1,
                      ifelse(!is.na(proc2) & (yearsep > 1987) & 
                               (proc2 >= 60.30 & proc2 <= 60.69), 
                             0, bphsurg))) %>%
  mutate(bphsurg=ifelse(!is.na(proc3) & (yearsep > 1987) & 
                        (proc3 >= 60.20 & proc3 <= 60.29), 1,
                      ifelse(!is.na(proc3) & (yearsep > 1987) & 
                               (proc3 >= 60.30 & proc3 <= 60.69), 
                             0, bphsurg)))


# Create sequence variables (fileseq, morbseq, and indexseq)

bphsurgery2 <- bphsurgery2 %>% arrange(rootlpno, sepdate)
bphsurgery2 <- bphsurgery2 %>% mutate(fileseq = row_number())

bphsurgery2 <- bphsurgery2 %>% 
  group_by(rootlpno) %>%
  arrange(sepdate) %>%
  mutate(morbseq = row_number()) %>%
  ungroup()

temp <- bphsurgery2 %>% 
  filter(bphsurg == 1 | bphsurg == 0) %>% 
  select(rootlpno, sepdate, bphsurg) %>% 
  group_by(rootlpno, sepdate, bphsurg) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  group_by(rootlpno) %>% 
  arrange(sepdate) %>%
  mutate(indexseq = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

temp <- bphsurgery2 %>% full_join(temp, by = c('rootlpno', 'sepdate', 'bphsurg')) %>%
  mutate(indexseq = ifelse(is.na(indexseq), 0, indexseq)) %>%
  arrange(rootlpno, sepdate)

for (i in 2:nrow(bphsurgery2)) {
  if ((temp$rootlpno[i] == temp$rootlpno[i-1]) & (temp$indexseq[i-1] != 0)){
    temp$indexseq[i] <- temp$indexseq[i-1] + 1
  }
}

bphsurgery2 <- temp

# Create status and time variables to death and censoring variable 

bphsurgery2$dead <- 0
bphsurgery2$dead[!is.na(bphsurgery2$dthdate)] <- 1

bphsurgery2$followup <- as.Date('1996-12-31')

for (i in 1:nrow(bphsurgery2)){
  if(bphsurgery2$dead[i] == 1 & 
     bphsurgery2$dthdate[i] < bphsurgery2$followup[i]){
    bphsurgery2$followup[i] <- bphsurgery2$dthdate[i]
  }
}

bphsurgery2 <- bphsurgery2 %>%
  mutate(survival=as.integer(followup - admdate))

save(bphsurgery2, file = "bphsurgery2.RData")

# Actuarial survival and unadjusted Cox regression analysis comparing TURP to OP

temp <- bphsurgery2 %>% filter(indexseq == 1)
surv <- survfit(Surv(ceiling(survival / 365), dead) ~ bphsurg, data = temp)
ggsurvplot(surv, conf.int=TRUE, ylim=c(0.5, 1), xscale=1/365,
           title='Survival Time', ylab='Cumulative Survival', xlab='Days',
           legend='top', legend.title='Procedure',
           legend.labs=c('OP', 'TURP'))

cox <- coxph(Surv(survival, dead)~ bphsurg, data=temp)
summary(cox)

rm(temp, surv, cox)

# Examine confounding by age

summarise(bphsurgery2, mean(age))

summarise(filter(bphsurgery2, bphsurg == 0), mean(age))

summarise(filter(bphsurgery2, bphsurg == 1), mean(age))

summarise(bphsurgery2, mean(yearsep))

summarise(filter(bphsurgery2, bphsurg == 0), mean(yearsep))

summarise(filter(bphsurgery2, bphsurg == 1), mean(yearsep))

# Risk adjust for age (transformed) and calendar time

bphsurgery3 <- bphsurgery2 %>%
  mutate(age2=age^2) %>%
  mutate(age2ln=age2 * log(age))

temp <- bphsurgery3 %>% filter(indexseq == 1)
cox_age <- coxph(Surv(survival, dead)~ bphsurg + yearsep + age2 + age2ln, 
                 data=temp)
summary(cox_age)


# Risk adjust for age, calendar time and comorbidity (transformed) 

bphsurgery3 <- bphsurgery3 %>%
  mutate(chmsqr=1/((0.01 + charlson)^0.5)) %>%
  mutate(chln=log(charlson + 0.01))

bphsurgery3 <- bphsurgery3 %>%
  mutate(anycom=ifelse(charlson > 0, 1, 0))

temp <- bphsurgery3 %>% filter(indexseq == 1)
cox_multi <- coxph(Surv(survival, dead)~ bphsurg + yearsep + age2 + age2ln + 
                     chmsqr + chln + anycom, data=temp)
summary(cox_multi)
