## PMIM402J Script 2
## Question 1: What key factors are predictive of intention to breastfeed?
## Logistic regression and decision tree analyses
## Student no. 2005070 
## Date: 18/06/24

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(ggplot2) # For plots
library(ggpubr) # For ggarrange()
library(gtsummary) # For tbl_regression()
library(car) # For vif()
library(sjPlot) # For plot_model()
library(rpart) # For decision tree
library(rpart.plot) # For decision tree

## Loading the data and removing unnecessary variables
load('analysisdata.RData')
data2 <- analysisdata %>% select(-id, -bmi)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Looking at the distribution of data through jittered scatter plots

rltnship_plot <- ggplot(data2, aes(x = rltnship_status, 
                                        y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal() 

anxiety_plot <- ggplot(data2, aes(x = anxiety_severity, 
                                       y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal()

child_plot <- ggplot(data2, aes(x = other_children, 
                                     y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal()

education_plot <- ggplot(data2, aes(x = education, 
                                         y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal()

depression_plot <- ggplot(data2, aes(x = depression_severity, 
                                         y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal()

smoking_plot <- ggplot(data2, aes(x = smoking_history,
                                       y = intention_to_feed)) +
  geom_jitter(height = .05, alpha = .1) + theme_minimal()

ggarrange(rltnship_plot, anxiety_plot, child_plot, education_plot, 
          depression_plot, smoking_plot,
          ncol = 3, nrow = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Logistic regression models

modelfull <- glm(intention_to_feed~., 
                 data = data2, family = binomial)
summary(modelfull)

## Starting backward elimination
model1 <- glm(intention_to_feed ~ rltnship_status + depression_severity +
                anxiety_severity + smoking_history + other_children +
                household_income, 
              data = data2, family = binomial)
summary(model1)

model2 <- glm(intention_to_feed ~ rltnship_status + depression_severity + 
                anxiety_severity + smoking_history + other_children, 
              data = data2, family = binomial)
summary(model2)

## Final model
model3 <- glm(intention_to_feed ~ rltnship_status + depression_severity +
                anxiety_severity + other_children, 
              data = data2, family = binomial)
summary(model3)

# Creating a table of results
tbl_regression(model3, exponentiate = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Assumption of multicollinearity of independent variables
vif(model3)
vif(modelfull)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Visualising the results of the model

a <- plot_model(model3, type = "pred", terms = "rltnship_status") +
  labs(y="Prob(intention to breastfeed)", 
       x="Relationship status",
       title = "Predicted probabilities of intention to breastfeed")

b <- plot_model(model3, type = "pred", terms = "anxiety_severity") +
  labs(y="Prob(intention to breastfeed)", 
       x="Anxiety severity",
       title = "")

c <- plot_model(model3, type = "pred", terms = "other_children") +
  labs(y="Prob(intention to breastfeed)", 
       x="Had a previous child",
       title = "")

d <- plot_model(model3, type = "pred", terms = "depression_severity") +
  labs(y="Prob(intention to breastfeed)", 
       x="Depression severity",
       title = "")

ggarrange(a, b, c, d, ncol = 2, nrow = 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Decision tree

treemodel <- rpart(intention_to_feed~., data = data2)
rpart.plot(treemodel)
summary(treemodel)
