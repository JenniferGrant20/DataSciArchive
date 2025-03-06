
library(tidyverse)
library(survival)
library(survminer)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exercise 4

load("vaslinks5.RData")

# Create status and time for vasectomy

vaslinks5$followup <- as.Date('1996-12-31')

for (i in 1:nrow(vaslinks5)){
  if(vaslinks5$dead[i] == 1 & vaslinks5$dthdate[i] < vaslinks5$followup[i]){
    vaslinks5$followup[i] <- vaslinks5$dthdate[i]
  }
}

vasosub <- vaslinks5[vaslinks5$treat == 2, ]
vasodates <- aggregate(vasosub$admdate, list("rootlpno" = vasosub$rootlpno), min)

vaslinks5 <- merge(vaslinks5, vasodates, by = "rootlpno", all.x = TRUE)
names(vaslinks5)[which(names(vaslinks5) == "vasodate.x")] <- "vasodate"

rm(vasodates, vasosub)

vaslinks5$reverse <- 0

for (i in 1:nrow(vaslinks5)){
  if (!is.na(vaslinks5$vasodate[i]) == TRUE & 
      vaslinks5$admdate[i] <= vaslinks5$vasodate[i] & 
      vaslinks5$vasodate[i] <= vaslinks5$followup[i]){
        vaslinks5$reverse[i] <- 1
  }
}

vaslinks5$timerev <- as.integer(0)

for (i in 1:nrow(vaslinks5)){
  if (vaslinks5$revseq[i] == 1){
    vaslinks5$timerev[i] <- as.integer(vaslinks5$vasodate[i] - 
                                         vaslinks5$admdate[i])
  } else if (vaslinks5$revseq[i] > 1){
    vaslinks5$timerev[i] <- as.integer(vaslinks5$followup[i] - 
                                         vaslinks5$admdate[i] + 1)
  } else if (is.na(vaslinks5$vasodate[i])){
    vaslinks5$timerev[i] <- as.integer(vaslinks5$followup[i] - 
                                         vaslinks5$admdate[i])
  } else if (!is.na(vaslinks5$vasodate[i])){
    vaslinks5$timerev[i] <- as.integer(vaslinks5$vasodate[i] - 
                                         vaslinks5$admdate[i])
  }
}

vaslinks5 <- vaslinks5[ , -which(names(vaslinks5) %in% "vasodate")]

# Actuarial survival analysis

temp <- vaslinks5 %>% filter(vasseq == 1) %>% 
  select(rootlpno, fileseq, timerev, reverse, age30)
surv <- survfit(Surv(ceiling(timerev / 365), reverse) ~ 1, data = temp)
ggsurvplot(surv, conf.int = TRUE, ylim = c(0.975, 1), xscale = 1/365,
           title = 'Survival Time to Vasectomy',
           ylab = 'Cumulative Survival', xlab = 'Days', 
           legend = 'none')

rm(temp, surv)

# Creating age30 binary indicator

vaslinks5$age30 <- 0 
for (i in 1:nrow(vaslinks5)){
  if (vaslinks5$age[i] < 30){
    vaslinks5$age30[i] <- 1
  }
}

# Actuarial survival analysis including age30

temp <- vaslinks5 %>% filter(vasseq == 1) %>% 
  select(rootlpno, fileseq, timerev, reverse, age30)
surv <- survfit(Surv(ceiling(timerev / 365), reverse) ~ age30, data = temp)
ggsurvplot(surv, conf.int = TRUE, ylim = c(0.96, 1), xscale = 1/365,
           title = 'Survival Time to Vasectomy',
           ylab = 'Cumulative Survival', xlab = 'Days', 
           legend = 'top',
           legend.title = 'Age Group',
           legend.labs = c('Age >= 30 years', 'Age < 30 years'))

# Cox analysis  

cox <- coxph(Surv(timerev,reverse) ~ age30, data = temp)
summary(cox)

rm(surv, temp, cox)

# Paternity

vaslinks5$patern <- 0
for (i in 1:nrow(vaslinks5)){
  if (!is.na(vaslinks5$bthdate[i]) == TRUE & 
      vaslinks5$bthdate[i] < vaslinks5$followup[i]){
    vaslinks5$patern[i] <- 1
  }
}

vaslinks5$timepat <- as.integer(0)
for (i in 1:nrow(vaslinks5)){
  if (vaslinks5$patern[i] == 1){
    vaslinks5$timepat[i] <- as.integer(vaslinks5$bthdate[i] - 
                                         vaslinks5$admdate[i])
  } else {
    vaslinks5$timepat[i] <- as.integer(vaslinks5$followup[i] - 
                                         vaslinks5$admdate[i])
  }
}

# Actuarial analysis including paternity

temp <- vaslinks5 %>%
  filter(revseq==1) %>%
  select(rootlpno, fileseq, timepat, patern)
surv <- survfit(Surv(ceiling(timepat / 365), patern) ~ 1, data=temp)
ggsurvplot(surv, conf.int=TRUE, ylim=c(0.4, 1), xscale=1/365,
           title='Survival Time to Paternity on Birth Certificate',
           ylab='Cumulative Survival', xlab='Days',
           legend='none')

# Multivariable Cox regression for paternity by age and time

vaslinks5$year1990 <- 0
for (i in 1:nrow(vaslinks5)){
  if (vaslinks5$yearsep[i] >= 1990){
    vaslinks5$year1990[i] <- 1
  }
}

vaslinks5$elapse6 <- 0

vasdates <- vaslinks5 %>% filter(vasseq==1) %>%
  select(rootlpno, vasdate=admdate)

vaslinks5 <- merge(vaslinks5, vasdates, by = 'rootlpno', all.x = TRUE) 
vaslinks5 <- vaslinks5 %>% arrange(fileseq)

vaslinks5 <- vaslinks5 %>%  
  mutate(diff=vasodate - vasdate) %>%
  mutate(elapse6=ifelse(diff >= 2190, 0, 1)) %>%
  select(-diff)

num <- (vaslinks5 %>% filter(revseq==1 & vaslinks5$vasseq >= 1) %>% count())[[1]]
num

temp <- vaslinks5 %>%
  filter(revseq==1 & vasseq >= 1) %>%
  select(rootlpno, timepat, patern, age30, year1990, elapse6)
cox <- coxph(Surv(timepat, patern)~ age30 + year1990 + elapse6, data=temp)
summary(cox)

vaslinks6 <- vaslinks5

save(vaslinks6, file = 'vaslinks6.RData')
