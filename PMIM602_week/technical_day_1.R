# ------------------------------------------------------------------------------
# PMIM602 Advanced Analysis of Linked Health Data
# Technical Session 1
# Pete Arnold
# 9 April 2021
# ------------------------------------------------------------------------------

# Libraries.
library(dplyr)
library(magrittr)
library(lubridate)
library(data.table)

# ==============================================================================
# PART 1
# ------------------------------------------------------------------------------
# Create a fileseq variable.
# ------------------------------------------------------------------------------
load('./data/MBSdata.RData')

N <- nrow(MBSdata)

# Sort by rootlpno + servdate, then group by the rootlpno, then number the rows.
start <- Sys.time()

temp1_tv <- MBSdata %>%
    arrange(rootlpno, servdate) %>%
    mutate(fileseq=row_number())

end <- Sys.time()
cat('\nTidyverse took ', end-start, ' seconds to complete ', N, ' rows.\n')

# ------------------------------------------------------------------------------
# Create a morbseq variable.
# ------------------------------------------------------------------------------

# Sort by rootlpno + servdate, then group by the rootlpno, then number the rows.
start <- Sys.time()

temp1_tv <- MBSdata %>%
    arrange(rootlpno, servdate) %>%
    group_by(rootlpno) %>%
    mutate(morbseq=row_number()) %>%
    ungroup()

end <- Sys.time()
cat('\nTidyverse took ', end-start, ' seconds to complete ', N, ' rows.\n')

# In base-R we can use the data.table library.
start <- Sys.time()

#temp1_dt <- MBSdata[order(MBSdata$rootlpno, MBSdata$servdate),]
temp1_dt <- data.table(temp1_dt)
temp1_dt <- temp1_dt[, morbseq := 1:.N, by=rootlpno]

end <- Sys.time()
cat('\nData.table took ', end-start, ' seconds to complete ', N, ' rows.\n')

# Use the base-R group averages over a level.
start <- Sys.time()

temp1_av <- MBSdata[order(MBSdata$rootlpno, MBSdata$servdate),]
temp1_av$morbseq <- ave(rep(1, nrow(temp1_av)), temp1_av$rootlpno, FUN=seq_along)

end <- Sys.time()
cat('\nAve took ', end-start, ' seconds to complete ', N, ' rows.\n')

# Or step-by-step.
start <- Sys.time()

temp1_br <- MBSdata[order(MBSdata$rootlpno, MBSdata$servdate),]
# Set everything to 1, which is correct for the first row for each rootlpno.
temp1_br$morbseq <- 1
# Then add one if the rootlpno we are on is the same as the previous one.
N <- 10000
for (i in 2:N) { #nrow(MBSdata)){
    if (temp1_br[i, 'rootlpno'] == temp1_br[i - 1, 'rootlpno']) {
        temp1_br[i, 'morbseq'] <- temp1_br[i - 1, 'morbseq'] + 1
    }
}

end <- Sys.time()
cat('\nFor loop took ', end-start, ' seconds to complete ', N, ' rows.\n')

# ==============================================================================
# PART 2
# ------------------------------------------------------------------------------
# Create a condition sequence variable.
# ------------------------------------------------------------------------------
# Restore the value of N.
N <- nrow(MBSdata)

# Recode `flag` to 1 for any entry matching one of the following.
random_mbs_items <- c(23, 116, 65063, 44, 73912, 53)

# In base-R with the data.table library.
start <- Sys.time()
# Sort the data.
temp2_dt <- MBSdata[order(MBSdata$rootlpno, MBSdata$servdate),]
# Create a flag for any of the above values.
temp2_dt$flag <- 0
temp2_dt$flag[temp2_dt$mbsitem %in% random_mbs_items] <- 1
# Where `flag` is not 0 then count the number of rows for matching rootlpno's.
temp2_dt <- data.table(temp2_dt)
temp2_dt <- temp2_dt[flag != 0, condseq := 1:.N, by=rootlpno]
temp2_dt <- as.data.frame(temp2_dt)
end <- Sys.time()
cat('\nCondseq took ', end-start, ' seconds to complete.\n')

# And with the Tidyverse.
start <- Sys.time()
temp2_tv <- MBSdata %>% 
    mutate(flag=ifelse(mbsitem %in% random_mbs_items, 1, 0))
temp2_tv <- temp2_tv %>%
    arrange(rootlpno, servdate, mbsitem) %>%
    group_by(rootlpno) %>%
    # Now we sort in reverse order of the tag (i.e. 1's first) so that they
    # are located in rows 1...
    arrange(-flag) %>%
    mutate(servseq = ifelse(flag==1, row_number(), NA)) %>%
    ungroup() %>%
    # Just to make sure, re-sort in the expected order.
    arrange(rootlpno, servdate, mbsitem)
end <- Sys.time()
cat('\nTidyverse took ', end-start, ' seconds to complete ', N, ' rows.\n')

# ==============================================================================
# PART 3
# ------------------------------------------------------------------------------
# A vertical merge.
# ------------------------------------------------------------------------------
start <- Sys.time()

huge_data_frame <- temp1_av %>% 
    rbind(temp1_br, temp1_tv)

end <- Sys.time()
cat('\nTriple rbind took ', end-start, ' seconds to complete.\n')

rm(huge_data_frame)

# ==============================================================================
# PART 4
# ------------------------------------------------------------------------------
# Horizontal extension.
# ------------------------------------------------------------------------------
# 1. Add a column:
temp3_tv <- MBSdata %>% 
    mutate(flag=ifelse(mbsitem %in% random_mbs_items, 1, 0))

# 2. Merge/join two tables:
load('data/Dthdata.RData')
temp3_tv <- MBSdata %>%
    merge(Dthdata, by='rootlpno', all.x=TRUE)

temp3_tv <- MBSdata %>%
    left_join(Dthdata, by='rootlpno')

temp3_tv <- MBSdata %>%
    merge(Dthdata %>% select(-sex, -age), by='rootlpno', all.x=TRUE)

# 3. In base-R.
temp3_tv <- merge(MBSdata, Dthdata, by='rootlpno', all.x=TRUE)

temp3_tv <- left_join(MBSdata, Dthdata, by='rootlpno')

temp3_tv <- merge(MBSdata, Dthdata[, !(colnames(Dthdata) %in% c('sex', 'age'))],
    by='rootlpno', all.x=TRUE)

# ==============================================================================
# MISCELLANEOUS
# ------------------------------------------------------------------------------
# Backcasting correction.
# ------------------------------------------------------------------------------

# Start with the antepenult file which has all the sequence values.
# Get the time from the start of the study (1.1.90) and plot survival with
# previous indicator being the event (i.e. mark each second or later event).

# Survival = 1.017 * reverse-survival-time ^ -0.438

# If reverse-survival-time > time where survival = 0, Cx (correction) = 1
# If                       <                        , Cx = 0.04/(1.017*rst^-0.438)

# Cx can be extreme, so constrain it to 0 to 1.

load('data/diabantepenult_cx.RData')
temp <- diabantepenult %>% filter(morbseq==1)

temp %>% filter(yearinc==1990) %>% summarise(n())
temp %>% filter(yearinc==1990) %>% summarise(sum(cx))

# xtabs(formula -> cx is the vector of counts; yearinc is the classifying var.)
xtabs(~yearinc, data=temp) %>% round(digits=0)
xtabs(cx~yearinc, data=temp) %>% round(digits=0)

# ------------------------------------------------------------------------------
# Capture-recapture.
# ------------------------------------------------------------------------------

# Look at the total population in the commonwealth records (mbs/pbs) and the
# total number in the state data (hmds/dth). Then, with the cross-tabulation,
# we can find the number in neither, either and both and from that calculate
# the estimated total number of cases.

# For this, we need to access the entries in the table:
table <- xtabs(~cth+state ...)

# Using table[1, 2] etc.

# ------------------------------------------------------------------------------
# Tidying the tables in R.
# ------------------------------------------------------------------------------
library(Hmisc)
load('data/MBSdata.RData')
# Create a list of the labels you want to change.
labels <- c(servdate="")
# Convert this to a list of labels for the target object containing only those
# labels you wish to change, but with empty entries for each of the others.
label(MBSdata) <- as.list(labels[match(names(MBSdata), names(labels))])




