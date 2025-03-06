#####################################################
# IALHD DAY 1
# R Syntax solution
# 
# J. Demmler
# March 2018
#
#####################################################

# Q1. ####
# Set working directory to the path where your R Data files are, e.g.
setwd("D:/IALHD R Data/")
# Check files in this directory
list.files()
load("vashmds.RData")
# Open file in Pane 1
View(vashmds)

# Q2. ####
# return the total number records
nrow(vashmds)
# You can find this information as well in Pane 3/ Environment, e.g.
# vashmds - 9406 obs. of 16 variables
# if you click on the table symbol to the right of this text it will execute `View(vashmds)` for you
# `dim(vashmds)` will show you the dimension of the table, i.e. 9406 rows and 16 columns.


# Q3. ####
# create temporary table to explore
vashmds.tmp <-  vashmds
# we will use the library chron to extract the years from the dates
# install the package first (you only need to do this once)
install.packages("chron")
# load the package (you need to do this in each new R session)
library(chron)
vashmds.tmp$yearsep <- years(vashmds.tmp$sepdate)
  
# frequency of yearsep
table(vashmds.tmp$yearsep)
# sum if wanted
# sum( table(vashmds.tmp$yearsep))

# Q4. ####
# we will use the plyr package
# install the package first (you only need to do this once)
install.packages("plyr")
# load the package (you need to do this in each new R session)
library(plyr)
# make sure the data is sorted, in this case it is
# create morbseq using `ddply`
# for each `rootlpno` in  `vashmds.tmp` create a new function (`mutate`) which counts through
# each record for this `rootlpno` and append the result as a new column `morbseq` to the data
vashmds.tmp <- ddply(vashmds.tmp,"rootlpno", mutate, morbseq=1:length(rootlpno))

# Q5. ####
nrow(vashmds.tmp[vashmds.tmp$morbseq == 1,])
# 2933
nrow(vashmds.tmp[vashmds.tmp$morbseq == 2,])
# 1772

# Q6. ####
# simple solution, but doesn't include std. dev.
# summary(vashmds.tmp$age[vashmds.tmp$morbseq == 1])
# install the package first (you only need to do this once)
install.packages("psych")
# load the package (you need to do this in each new R session)
library(psych)
describe(vashmds.tmp$age[vashmds.tmp$morbseq == 1])

# Q7. ####
# create a new column `totrec`, which summarises the total number of `morseq`
# for each `rootlpno`
vashmds.tmp <- ddply(vashmds.tmp,"rootlpno", mutate, totrec=max(morbseq))
describe(vashmds.tmp$totrec[vashmds.tmp$morbseq == 1])

# Q8. ####
load("vascancer.RData")
View(vascancer)

# Q9. ####
# order by `rootlpno`, should already be ordered
# vascancer <- vascancer[order(vascancer$rootlpno),]

# create`morbseq` in `vascancer`
vascancer <- ddply(vascancer,"rootlpno", mutate, morbseq=1:length(rootlpno))
# just show rows 20-33, which are the ones shown in the workbook
View(vascancer[20:33,])

# Q10. ####
table(vascancer$morbseq)

# Q11. ####
# create new empty columns
vascancer$cansite1 <- NA
vascancer$cantis1 <- NA
vascancer$candate1 <- as.Date(NA)
vascancer$cansite2 <- NA
vascancer$cantis2 <- NA
vascancer$candate2 <- as.Date(NA)

# if `morbseq` = 1 copy into `cansite1`,`cantis1` and `candate1`
c1 <- which(vascancer$morbseq == 1)
vascancer$cansite1[c1] <- vascancer$cansite[c1]
vascancer$cantis1[c1] <- vascancer$cantis[c1]
vascancer$candate1[c1] <- vascancer$candate[c1]

# if `morbseq` = 2 copy into `cansite2`,`cantis2` and `candate2`
c2 <- which(vascancer$morbseq == 2)
vascancer$cansite2[c2-1] <- vascancer$cansite[c2]
vascancer$cantis2[c2-1] <- vascancer$cantis[c2]
vascancer$candate2[c2-1] <- vascancer$candate[c2]

# only keep rows, where `cansite1` is not NULL (ignoring the duplicate rows) and
# save as `vascancer2`, we also ignore the now redundant columns 2-5
c3 <- which(is.na(vascancer$cansite1) == FALSE)
vascancer2 <- vascancer[c3,c(1,6:11)]

# show rows 19-28 as displayed in the workbook
View(vascancer2[19:28,])

# remove temporary objects `c1`, `c2` and `c3`
rm(c1,c2,c3)

# save `vascancer2` as a new R Data file
save(vascancer2, file="vascancer2.RData")
