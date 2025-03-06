
library(tidyverse)
library(magrittr)
library(lubridate)
library(psych)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exercise 2: part A

# Deleting duplicate death dates and merging vashmds and vasdeath

load("vashmds.RData")
load("vasdeath.RData")
vashmds %>% filter(rootlpno == '14171943')   # hospital admission in 1984 #
vasdeath_fixed <- vasdeath[-33,]

vaslinks <- merge(vashmds, vasdeath_fixed, by = "rootlpno", all = TRUE)
save(vaslinks, file="vaslinks.RData")

# Merging with vascancer2

load("vascancer2.RData")
vaslinks2 <- merge(vaslinks, vascancer2, by = "rootlpno", all = TRUE)
save(vaslinks2, file="vaslinks2.RData")

# Merging with vasbirth

load("vasbirth.RData")
vaslinks3 <- merge(vaslinks2, vasbirth, by = "rootlpno", all = TRUE)
save(vaslinks3, file="vaslinks3.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exercise 2: part B

# Adding yearsep variable

vaslinks3 <- mutate(vaslinks3, yearsep = year(sepdate))

# Adding treat variable (tag)

vaslinks4 <- for (i in 1:nrow(vaslinks3)){
  
  if (vaslinks3$yearsep[i] < 1988){
    if (any(vaslinks3[i,14:16] %in% c(56.36,59.81))) {
      vaslinks3$treat[i] <- 1}
    if (any(vaslinks3[i,14:16] %in% c(56.34,56.37))) {
      vaslinks3$treat[i] <- 2}
  }
  
  if (vaslinks3$yearsep[i] >= 1988) {
    if (any(vaslinks3[i,14:16] %in% seq(63.70,63.79,0.01))){
      vaslinks3$treat[i] <- 1}
    if (any(vaslinks3[i,14:16] %in% seq(63.80,63.89,0.01))) {
      vaslinks3$treat[i] <- 2}  
  }
}

   ## or

vaslinks4 <- vaslinks3 %>%
  mutate(treat=0) %>%
  mutate(treat=ifelse(!is.na(proc1) & (yearsep < 1988) & 
                        (proc1 %in% c(56.36,59.81)), 1, 
               ifelse(!is.na(proc1) & (yearsep < 1988) & 
                        (proc1 %in% c(56.34,56.37)), 2,treat))) %>%
  mutate(treat=ifelse(!is.na(proc2) & (yearsep < 1988) & 
                        (proc2 %in% c(56.36,59.81)), 1,
               ifelse(!is.na(proc2) & (yearsep < 1988) & 
                        (proc2 %in% c(56.34,56.37)), 2, treat))) %>%
  mutate(treat=ifelse(!is.na(proc3) & (yearsep < 1988) & 
                        (proc3 %in% c(56.36,59.81)), 1,
               ifelse(!is.na(proc3) & (yearsep < 1988) & 
                        (proc3 %in% c(56.34,56.37)), 2, treat))) %>%
  mutate(treat=ifelse(!is.na(proc1) & (yearsep > 1987) & 
                        (proc1 >= 63.70 & proc1 <= 63.79), 1,
               ifelse(!is.na(proc1) & (yearsep > 1987) & 
                        (proc1 >= 63.80 & proc1 <= 63.89), 2, treat))) %>%
  mutate(treat=ifelse(!is.na(proc2) & (yearsep > 1987) & 
                        (proc2 >= 63.70 & proc2 <= 63.79), 1,
               ifelse(!is.na(proc2) & (yearsep > 1987) & 
                        (proc2 >= 63.80 & proc2 <= 63.89), 2, treat))) %>%
  mutate(treat=ifelse(!is.na(proc3) & (yearsep > 1987) & 
                        (proc3 >= 63.70 & proc3 <= 63.79), 1,
               ifelse(!is.na(proc3) & (yearsep > 1987) & 
                        (proc3 >= 63.80 & proc3 <= 63.89), 2, treat)))

table(vaslinks4$treat)

# Creating fileseq variable

vaslinks4 <- vaslinks4 %>% arrange(rootlpno, sepdate)
vaslinks4 <- vaslinks4 %>% mutate(fileseq = row_number())

# Creating morbseq variable at the end of each variable

vaslinks4 <- vaslinks4 %>% 
               group_by(rootlpno) %>%
               arrange(sepdate) %>%
               mutate(morbseq = row_number()) %>%
               ungroup()

# Creating a vasseq variable for each individual 

temp <- vaslinks4 %>% 
          filter(treat == 1) %>% 
          select(rootlpno, sepdate, treat) %>% 
          group_by(rootlpno, sepdate, treat) %>%
          filter(row_number() == 1) %>% 
          ungroup() %>% 
          group_by(rootlpno) %>% 
          arrange(sepdate) %>%
          mutate(vasseq = ifelse(row_number() == 1, 1, 0)) %>%
          ungroup()

temp <- vaslinks4 %>% full_join(temp, by = c('rootlpno', 'sepdate', 'treat')) %>%
  mutate(vasseq = ifelse(is.na(vasseq), 0, vasseq)) %>%
  arrange(rootlpno, sepdate)

for (i in 2:nrow(vaslinks4)) {
  if ((temp$rootlpno[i] == temp$rootlpno[i-1]) & (temp$vasseq[i-1] != 0)){
        temp$vasseq[i] <- temp$vasseq[i-1] + 1
  }
}

vaslinks4 <- temp

# Creating revseq variable for each individual

temp <- vaslinks4 %>% 
  filter(treat == 2) %>% 
  select(rootlpno, sepdate, treat) %>% 
  group_by(rootlpno, sepdate, treat) %>%
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  group_by(rootlpno) %>% 
  arrange(sepdate) %>%
  mutate(revseq = ifelse(row_number() == 1, 1, 0)) %>%
  ungroup()

temp <- vaslinks4 %>% full_join(temp, by = c('rootlpno', 'sepdate', 'treat')) %>%
  mutate(revseq = ifelse(is.na(revseq), 0, revseq)) %>%
  arrange(rootlpno, sepdate)

for (i in 2:nrow(vaslinks4)) {
  if ((temp$rootlpno[i] == temp$rootlpno[i-1]) & (temp$revseq[i-1] != 0)){
    temp$revseq[i] <- temp$revseq[i-1] + 1
  }
}

vaslinks4 <- temp

# Creating dead variable

vaslinks4$dead <- 0
vaslinks4$dead[!is.na(vaslinks4$dthdate)] <- 1

save(vaslinks4, file="vaslinks4.RData")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Filling in the table

denominator <- vaslinks4 %>% distinct(rootlpno) %>% count()
denominator <- denominator[[1]]
    ## 2933

# Number of men who underwent vasectomy (2806)

vaslinks4 %>% filter(vasseq == 1) %>% distinct(rootlpno) %>% count()

# Number of men who underwent reversal (1181)

vaslinks4 %>% filter(vasseq == 2) %>% distinct(rootlpno) %>% count()

# Mean age in years +- SD 
# for vasectomies (mean = 34.5, SD = 6.5)
# for reversals (mean = 38.5, SD = 7.8)

vasectomy <- vaslinks4 %>% filter(vasseq == 1) 
describe(vasectomy$age)

reversal <- vaslinks4 %>% filter(vasseq == 2)
describe(reversal$age)

# Number dead (11.78%)

table(vasectomy$dead)
table(vaslinks4$dead)
(33 / 2773) * 100


# Number with a first cancer registration
