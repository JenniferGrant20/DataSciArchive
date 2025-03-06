## PMIM402J Script 2
## Analysis 1: logistic regression code
## Student no. 2005070 
## Date:

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(car)

load('analysisdata.RData')
logregdata <- analysisdata %>% select(-id, -bmi)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rltnship_plot <- ggplot(logregdata, aes(x = rltnship_status, 
                                        y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal() +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial'))

anxiety_plot <- ggplot(logregdata, aes(x = anxiety_severity, 
                                       y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal()

child_plot <- ggplot(logregdata, aes(x = other_children, 
                                     y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal()

education_plot <- ggplot(logregdata, aes(x = education, 
                                         y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal()

ggarrange(rltnship_plot, anxiety_plot, child_plot, education_plot, 
  ncol = 2, nrow = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Logistic regression models

modelfull <- glm(intention_to_feed~., 
             data = logregdata, family = binomial)
summary(modelfull)

model1 <- glm(intention_to_feed ~ rltnship_status + depression_severity +
                anxiety_severity + smoking_history + other_children +
                household_income, 
              data = logregdata, family = binomial)
summary(model1)

model2 <- glm(intention_to_feed ~ rltnship_status + depression_severity + 
                anxiety_severity + smoking_history + other_children, 
              data = logregdata, family = binomial)
summary(model2)

model3 <- glm(intention_to_feed ~ rltnship_status + depression_severity +
                anxiety_severity + other_children, 
              data = logregdata, family = binomial)
summary(model3)

model4 <- glm(intention_to_feed ~ rltnship_status + anxiety_severity + 
                other_children, 
              data = logregdata, family = binomial)
summary(model4)

exp(coef(model4)["rltnship_status2"])
# the coefficient for 'married/living with partner' is 1.24 and is significant
# which means that being married or living in a partnership is associated with
# an increased odds of intending to breastfeed by a factor of 3.47
exp(coef(model4)["anxiety_severity1"])
# the coefficient for 'anxiety severity = 1' is 0.69 and is significant which
# means that having minimal anxiety is associated with an increased
# likelihood (2.00 times) of intending to breastfeed
exp(coef(model4)["anxiety_severity2"])
# the coefficient for 'anxiety severity = 2' is 0.68 and is significant which
# means that having mild anxiety is associated with an increased likelihood 
# (1.97 times) of intending to breastfeed
exp(coef(model4)["other_children1"])
# the coefficient for having a previous child is -0.45 and is significant
# which means that having previous children is associated with a decreased 
# likelihood (0.64 times) of intending to breastfeed

# stepwisemodel <- step(modelfull, direction = 'both')
# summary(stepwisemodel)
# plot(stepwisemodel)

# model1.1 <- glm(intention_to_feed~rltnship_status,
#               data=logregdata, family=binomial)
# summary(model1.1)

# the coefficient for 'dating/engaged' is 0.8591 but non-significant
# the coefficient for 'married/living with partner' is 1.2938 and is significant
# which means that being married or living in a partnership is associated with
# an increased likelihood of intending to breastfeed one's baby in some capacity
# (by exp(1.29) or 3.63 times)

# model1.2 <- glm(intention_to_feed~anxiety_severity,
#                 data=logregdata, family=binomial)
# summary(model1.2)

# the intercept coefficient (no anxiety) is 1.1144 and is significant which
# means that having no anxiety is associated with an increased likelihood
# (3.03 times) of intending to breastfeed
# the coefficient for 'anxiety severity = 1' is 0.6092 and is significant
# which means that having minimal anxiety is associated with an increased
# likelihood (1.84 times) of intending to breastfeed

# model1.3 <- glm(intention_to_feed~other_children,
#                 data=logregdata, family=binomial)
# summary(model1.3)

# the intercept coefficient (no children) is 1.7445 and is significant which 
# means that having no previous children is associated with an increased 
# likelihood (5.70 times) of intending to breastfeed
# the coefficient for having a previous child is -0.5063 and is significant
# which means that having previous children is associated with a decreased 
# likelihood (0.60 times) of intending to breastfeed

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assumption checks

# Checking the independent variables are not highly correlated
library(Hmisc)
library(PerformanceAnalytics)
ivdata <- logregdata %>% select(-intention_to_feed)
cormatrix <- rcorr(as.matrix(ivdata))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(cormatrix$r, cormatrix$P)

vif(model4)
vif(modelfull)

# Checking for influential outliers using Cook's distance
# plot(stepwisemodel, which = 4, id.n = 7)
# plot(model4, which = 4, id.n = 6)
# par(mfrow = c(2,2))
# plot(stepwisemodel)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(sjPlot)

c <- plot_model(model4, type = "pred", terms = "other_children") +
  labs(y="Prob(intention to breastfeed)", 
       x="Had a previous child (0 = no, 1 = yes)",
       title = "")

b <- plot_model(model4, type = "pred", terms = "anxiety_severity") +
  labs(y="Prob(intention to breastfeed)", 
       x="Anxiety severity (0 = none, 1 = minimal, 2 = mild, 3 = moderate, 4 = severe)",
       title = "")

a <- plot_model(model4, type = "pred", terms = "rltnship_status") +
  labs(y="Prob(intention to breastfeed)", 
       x="Relationship status (0 = single, 1 = dating/engaged, 2 = married/partnered)")

ggarrange(a, b, c, ncol = 1, nrow = 3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newdata <- logregdata %>% 
  select(rltnship_status, anxiety_severity, other_children)

pred <- predict(model4,
                newdata,
                type = "response")
pred 

pred_outcome <- ifelse(pred < 0.5, 0, 1)
pred_outcome <- factor(pred_outcome,
                        levels = c(0, 1),
                        labels = c("0", 
                                   "1"))

tab <- table(logregdata$intention_to_feed, pred_outcome,
             dnn = c("observed", "predicted"))
tab

# correctly predicted the absence of intention to breastfeed for 7 women
# incorrectly predicted the presence of intention to breastfeed for 127 women
# incorrectly predicted the absence of intention to breastfeed for 3 women
# correctly predicted the presence of intention to breastfeed for 607 women

accuracy <- sum(diag(tab)) / sum(tab)
accuracy
# 82.53%

sensitivity <- tab[2, 2] / (tab[2, 2] + tab[2, 1])
sensitivity
# 99.51%

specificity <- tab[1, 1] / (tab[1, 1] + tab[1, 2])
specificity
# 5.22%

library(pROC)

res <- roc(intention_to_feed ~ fitted(model4),
           data = logregdata)

ggroc(res, legacy.axes = TRUE)
res$auc
# area under the curve: 0.629

ggroc(res, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res$auc, 2)))

library(gtsummary)
tbl_regression(model4, exponentiate = TRUE)
