## PMIM402J Script 3
## Analysis 2: decision tree code
## Student no. 2005070 
## Date:

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(rpart) # For decisions tree
library(rpart.plot) # For decision tree

load('analysisdata.RData')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

treedata <- analysisdata %>% select(-id, -bmi)

treemodel <- rpart(intention_to_feed~., data=treedata)
rpart.plot(treemodel)

# the decision tree provides an explanation for 11 individuals who did not 
# intend to breastfeed
(11 / 134) * 100 # which is only 8.21% of those who did not intend to breastfeed

table(analysisdata$intention_to_feed)
(134 / 744) * 100 # 18.01% of the dataset did not intend to breastfeed
