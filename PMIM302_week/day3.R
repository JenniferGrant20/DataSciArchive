
library(tidyverse)
library(psych)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exercise 3

# Open vaslinks4 file

load("vaslinks4.RData")

# Determine apparent first time admission post-1985

temp <- vaslinks4 %>% filter(yearsep >= 1985)
temp <- temp %>% 
  group_by(rootlpno) %>%
  arrange(sepdate) %>%
  mutate(morbseq = row_number()) %>%
  ungroup()
temp <- temp %>% filter(morbseq == 1)

   ## Generate frequency distribution of these records by yearsep

yearsep <- temp$yearsep
freq_table <- table(yearsep)
print(freq_table)

# Apply clearance period to correct prevalence pool effect 

vaslinks4_clearance <- vaslinks4 %>% filter(yearsep >= 1985 & morbseq == 1)
table(vaslinks4_clearance$yearsep)

# Create transseq variable to identify inter-hospital transfers

vaslinks4 <- vaslinks4 %>% 
  group_by(rootlpno) %>% 
  arrange(rootlpno, sepdate) %>%
  ungroup()

vaslinks4$transseq <- 0
vaslinks4$transseq <- ifelse(vaslinks4$morbseq >= 2 & 
                                lag(vaslinks4$sepdate) > vaslinks4$admdate, 
                                1, vaslinks4$transseq)
vaslinks4$transseq <- ifelse(vaslinks4$morbseq >= 2 & 
                               lag(vaslinks4$septype) == 2 &
                               lag(vaslinks4$sepdate) == vaslinks4$admdate, 
                               1, vaslinks4$transseq)

attach(vaslinks4)
for (i in 1:nrow(vaslinks4)){
  if (morbseq[i] >= 2 && transseq[i] == 1 && transseq[i-1] >= 1)
    vaslinks4$transseq[i] <- vaslinks4$transseq[i-1] + 1 
}
detach(vaslinks4)

# Backflow final separation date to index record

vaslinks4 <- vaslinks4[order(-vaslinks4$fileseq),]

vaslinks4$findate <- vaslinks4$sepdate
for (i in 2:nrow(vaslinks4)){
  if (vaslinks4$transseq[i-1] > 0){
    vaslinks4$findate[i] <- vaslinks4$findate[i-1]
  }
}

vaslinks4 <- vaslinks4[order(vaslinks4$fileseq),]

# Calculate total length of stay to include transfers

vaslinks4 <- vaslinks4 %>% mutate(los = sepdate - admdate)
for (i in 1:nrow(vaslinks4)){
  if (vaslinks4$los[i] == 0){
    vaslinks4$los[i] <- 1
  }
}

vaslinks4 <- vaslinks4 %>% mutate(totlos = findate - admdate)
for (i in 1:nrow(vaslinks4)){
  if (vaslinks4$totlos[i] == 0){
    vaslinks4$totlos[i] <- 1
  }
}

vaslinks4$los <- as.numeric(vaslinks4$los)
vaslinks4$totlos <- as.numeric(vaslinks4$totlos)
describe(vaslinks4 %>% filter(transseq == 0) %>% select(los, totlos))

vaslinks5 <- vaslinks4
save(vaslinks5, file="vaslinks5.RData")
