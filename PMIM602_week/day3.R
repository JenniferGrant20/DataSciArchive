library(tidyverse)

# Opening the files

load('diabetes6.RData')
load('diabcomp.RData')
load('HMDSdata2.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merging diabetes6 and diabcomp by rootlpno

diabetes7 <- diabetes6 %>% 
  merge(diabcomp, by = 'rootlpno', all.x = TRUE) %>%
  mutate(complic = ifelse(is.na(compdate), 0, 1))

table(diabetes7$complic)

save(diabetes7, file = 'diabetes7.RData')

# Merging HMDSdata2 and diabetes 7 by rootlpno

HMDSdata3 <- HMDSdata2 %>%
  merge(diabetes7 %>% select(rootlpno, date), by='rootlpno', all.x=TRUE) %>%
  arrange(rootlpno, fileseq)

# Creating pregnant variable

pregnancy_codes <- c(
  format(seq(630.00, 677.99, 0.01), digits=6),
  paste("O", sprintf("%05.2f", seq(00.0, 99.99, 0.01)), sep=""))

HMDSdata3 <- HMDSdata3 %>% mutate(pregnant=0)
for(i in 1:21){
  HMDSdata3 <- HMDSdata3 %>%
    mutate(pregnant=ifelse(!!sym(paste('diag', i, sep='')) %in% pregnancy_codes, 1, pregnant))
}

table(HMDSdata3$pregnant)

# Creating pregseq variable

HMDSdata3 <- HMDSdata3 %>%
  mutate(admdate_after_date=ifelse(admdate>=date, 1, 0)) %>%
  arrange(rootlpno, -admdate_after_date, -pregnant) %>%
  group_by(rootlpno) %>%
  mutate(pregseq=ifelse(admdate>=date & pregnant==1, row_number(), NA)) %>%
  ungroup() %>%
  select(-admdate_after_date) %>%
  arrange(rootlpno, morbseq, fileseq)

head(HMDSdata3[477:491,], 15)

save(HMDSdata3, file = 'HMDSdata3.RData')

# Cutting down the file

HMDSpreg <- HMDSdata3 %>%
  rename(pregdate = admdate) %>%
  filter(pregseq == 1) %>%
  select(rootlpno, pregdate)

save(HMDSpreg, file = 'HMDSpreg.RData')

# Merging diabetes7 and HMDSpreg by rootlpno

diabetes8 <- diabetes7 %>%
  merge(HMDSpreg, by = 'rootlpno', all.x = TRUE) %>%
  mutate(pregexp = ifelse(is.na(pregdate), 0, 1))

save(diabetes8, file = 'diabetes8.RData')

# Cutting down the file for analysis

rpfdiab <- diabetes8 %>%
  filter(age >= 15 & age <= 39 & sex == 2)

save(rpfdiab, file = 'rpfdiab.RData')

# Create file of women w/ a pregnancy, randomly sort, number and cut down

set.seed(2000000)
rpfexp <- rpfdiab %>%
  filter(pregexp==1) %>% 
  mutate(randsort=runif(n(), min=0, max=1)) %>%
  arrange(randsort) %>% 
  mutate(randseq=row_number()) %>%
  mutate(timepreg=pregdate-date)

head(rpfexp[1:15,], 15)

rpfexp <- rpfexp %>%
  select(rootlpno, randseq, timepreg)

save(rpfexp, file = 'rpfexp.RData')

# Create file of women w/o a pregnancy, randomly sort, number and cut down

set.seed(3000000)
rpfnon <- rpfdiab %>%
  filter(pregexp==0) %>% 
  mutate(randsort=runif(n(), min=0, max=1)) %>%
  arrange(randsort) 

rpfnon <- rpfnon %>%
  mutate(randseq=rep(1:nrow(rpfexp), ceiling(nrow(rpfnon)/nrow(rpfexp)))[1:nrow(rpfnon)])

head(rpfnon[1:15,], 15)

# Merging rpfnon and rfpexp by randseq

rpfnon <- rpfnon %>%
  merge(rpfexp %>% select(randseq, timepreg), by='randseq', all.x=TRUE) %>%
  arrange(rootlpno) %>% 
  select(rootlpno, timepreg)

save(rpfnon, file = 'rpfnon.RData')

# Merging rpfdiab and rpfnon by rootlpno, and excluding non-exposed women with
# impossible virtual pregnancies

rpfdiab2 <- rpfdiab %>% merge(rpfnon, all.x=TRUE)

rpfdiab2 <- rpfdiab2 %>%
  mutate(timepreg=ifelse(pregexp==1, pregdate-date, timepreg))

## Compare the time to pregnancy in the two groups (as defined by the values in
## pregexp). We could use individual calls to functions or we could create our
## own function which aggregates the results we want.
overview <- function(vector){
  cat('Length', names(summary(vector)), 'SD', '\n', sep='\t')
  cat(length(vector), summary(vector, digits=4), sd(vector), '\n', sep='\t')
}
with(rpfdiab2, tapply(timepreg, pregexp, overview))

rpfdiab2 <- rpfdiab2 %>%
  mutate(flag=ifelse(exit-date < timepreg, 1, 0))

xtabs(~flag+pregexp, data=rpfdiab2)

## 56 non-exposed women are flagged

rpfdiab2 <- rpfdiab2 %>% filter(flag==0) %>% select(-flag)

save(rpfdiab2, file = 'rpfdiab2.RData')

# Exclude women who developed complications prior to their pregnancy date

rpfdiab3 <- rpfdiab2 %>%
  # Set the column to zero to simplify what happens with NA's.
  mutate(flag=0) %>%
  mutate(flag=ifelse(pregexp==1,
                     ifelse(pregdate>compdate, 1, 0),
                     ifelse(timepreg>compdate-date, 1, 0)))

xtabs(~flag+pregexp, data=rpfdiab3)

rpfdiab3 <- rpfdiab3 %>% filter(is.na(flag) | flag==0) %>% select(-flag)

save(rpfdiab3, file = 'rpfdiab3.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Survival time to complication
# Create a compsurv time variable which is initially set to the days from date 
# to compdate for those with complications and from date to exit for those 
# without complications. Time-zero is then moved forward to the real or virtual 
# date of pregnancy admission by reducing compsurv by timepreg.

rpfdiab4 <- rpfdiab3 %>% arrange(rootlpno) %>%
  mutate(compsurv=ifelse(complic==0, exit-date, compdate-date)) %>%
  mutate(compsurv=as.numeric(compsurv-timepreg))

head(rpfdiab4, 15)

save(rpfdiab4, file = 'rpfdiab4.RData')

# Survival plot

library(survival)
library(survminer)

surv3.16 <- survfit(Surv(ceiling(compsurv/365), complic)~pregexp,
                    conf.int=0, data=rpfdiab4)
x_scale <- 1/365
y_limits <- c(0.7, 1)
colours <- c('blue', 'red')
labels <- c('pregexp=0', 'pregexp=1')
plot(surv3.16, xscale=x_scale, ylim=y_limits, mark.time=F,
     main="Survival time to complications \nin days from pregnancy admission",
     xlab="Days", ylab="Cumulative survival", col=colours
)
legend("topright", labels, lty=c(1,1), col=colours)

ggsurvplot(surv3.16, conf.int=FALSE, xscale=x_scale, ylim=y_limits,
           title='Survival time to complications \nin days from pregnancy admission',
           xlab='Days', ylab='Cumulative Survival',
           legend='top', legend.title='',
           legend.labs=labels
)

# Check for confounding of age and disease severity

rpfdiab5 <- rpfdiab4 %>%
  mutate(stagpreg=ifelse(timepreg<=stgpt1, 1, 0)) %>%
  mutate(stagpreg=ifelse(timepreg>=stgpt1 & timepreg<=(stgpt1+stgpt2), 2, stagpreg)) %>%
  mutate(stagpreg=ifelse(timepreg>=stgpt1+stgpt2, 3, stagpreg))

table(rpfdiab5$stagpreg)

save(rpfdiab5, file = 'rpfdiab5.RData')

# Cox regressions

cox3.1 <- coxph(Surv(compsurv,complic)~pregexp, data=rpfdiab5)
summary(cox3.1)

cox3.2 <- coxph(Surv(compsurv,complic)~pregexp+age, data=rpfdiab5)
summary(cox3.2)

cox3.3 <- coxph(Surv(compsurv,complic)~pregexp+age+stagpreg, data=rpfdiab5)
summary(cox3.3)

# Repeat with subsets with timepreg > or <= 270 days

sub1 <- rpfdiab5 %>% filter(timepreg > 270)
sub2 <- rpfdiab5 %>% filter(timepreg <= 270)

cox3.4 <- coxph(Surv(compsurv,complic)~pregexp+age+stagpreg, data=sub1)
summary(cox3.4)
