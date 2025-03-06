## PMIM402J Script 2
## Classification
## Student no. 2005070
## Date: 02/07/24

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(mice)
library(pROC) # For roc() and ggroc()
# library(tidymodels)
library(caret)
library(randomForest)
library(DataExplorer)
library(gtsummary) # For tbl_regression()
seed <- 50
set.seed(seed)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Loading the data and looking at it
data <- read.csv('heart_disease_modified.csv')
summary(data)

# Removing unnecessary variables
data <- data %>% select(-X, -pace_maker, -Patient_ID)

# Recoding character variables into numeric
data$drug <- recode(data$drug, 
                    "None" = 0, "Aspirin" = 1, "Clopidogrel" = 1, "Both" = 2)

data$fam_hist <- recode(data$fam_hist, "no" = 0, "yes" = 1)

# Looking at the data using histograms
data %>% gather(attributes, value, 1:ncol(data)) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill="lightblue", colour = "black") +
  facet_wrap(~attributes, scales = "free")

# Showed lots of zeroes for certain variables which we will convert to NAs
data <- data %>% mutate(chol = ifelse(chol == 0, NA, chol),
                        thalach = ifelse(thalach == 0, NA, thalach),
                        trestbps = ifelse(trestbps == 0, NA, trestbps))

# Percentage missing for each of the variables
data %>% filter(is.na(chol)) %>% nrow() / 920 * 100
data %>% filter(is.na(thalach)) %>% nrow() / 920 * 100
data %>% filter(is.na(trestbps)) %>% nrow() / 920 * 100

## Imputation
set.seed(seed)
md.pattern(data) # Looking at the missing data
imputedata <- data
imputedata <- mice(imputedata, m=5, method='pmm')
imputedata <- complete(imputedata)
summary(imputedata) # Checking the data looks okay after imputation
md.pattern(imputedata)

# Removing cholesterol variable as there were too many NAs prior to imputation
imputedata <- imputedata %>% select(-chol) 

# Checking the data after imputation using histograms
imputedata %>% gather(attributes, value, 1:ncol(imputedata)) %>%
  ggplot(aes(x=value)) +
  geom_histogram(fill="lightblue", colour = "black") +
  facet_wrap(~attributes, scales = "free")

# Formatting factor variables
imputedata$sex <- as.factor(imputedata$sex)
imputedata$cp <- as.factor(imputedata$cp)
imputedata$fbs <- as.factor(imputedata$fbs)
imputedata$restecg <- as.factor(imputedata$restecg)
imputedata$exang <- as.factor(imputedata$exang)
imputedata$slope <- as.factor(imputedata$slope)
imputedata$thal <- as.factor(imputedata$thal)
imputedata$smoker <- as.factor(imputedata$smoker)
imputedata$drug <- as.factor(imputedata$drug)
imputedata$fam_hist <- as.factor(imputedata$fam_hist)

## Splitting into train and test data
imputedata$class <- as.factor(imputedata$class)
set.seed(seed)
bound <- (nrow(imputedata)/5)*3
imputedata <- imputedata[(sample(nrow(imputedata))),] 
df.train <- imputedata[1:bound,]
df.test <- imputedata[(bound+1):nrow(imputedata),]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Logistic regression

# Training the regression model using training data
lr <- glm(class ~ ., family = "binomial", data=df.train) 
lr
tbl_regression(lr, exponentiate = TRUE) # Results table

# Applying the model to the test data
preds.lr <- predict(lr, df.test, type="response")

# Creating a confusion matrix
cm.lr <- table(predicted = as.numeric(preds.lr>0.5), truth = df.test$class)
cm.lr

# Checking accuracy
round(((cm.lr[1,1] + cm.lr[2,2]) / nrow(df.test)) * 100, 2) # accuracy
round((cm.lr[2, 2] / (cm.lr[2, 2] + cm.lr[2, 1]) * 100), 2) # sensitivity
round((cm.lr[1, 1] / (cm.lr[1, 1] + cm.lr[1, 2]) * 100), 2) # specificity

# ROC analysis
res.lr <- roc(class ~ preds.lr, data = df.test)
res.lr$auc
ggroc(res.lr, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res.lr$auc, 2)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Random forest

# Training the model
set.seed(seed)
rf <- randomForest(class ~ ., data = df.train)

plot(rf)
varImpPlot(rf)

# Applying the algorithm to the test data
preds.rf <- predict(rf, newdata = df.test, type="response")

# Creating a confusion matrix
cm.rf <- table(predicted = preds.rf, truth = df.test$class)
cm.rf

# Checking accuracy
round(((cm.rf[1,1] + cm.rf[2,2]) / nrow(df.test)) * 100, 2) # accuracy
round((cm.rf[2, 2] / (cm.rf[2, 2] + cm.rf[2, 1]) * 100), 2) # sensitivity
round((cm.rf[1, 1] / (cm.rf[1, 1] + cm.rf[1, 2]) * 100), 2) # specificity

# ROC analysis
res.rf <- roc(class ~ as.numeric(preds.rf), data = df.test)
res.rf$auc
ggroc(res.rf, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res.rf$auc, 2)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Optimising using feature selection

set.seed(seed)
plot_correlation(imputedata)
rf.opt <- randomForest(class ~ cp + thalach + thal + oldpeak + age + exang + 
                         perfusion + trestbps + slope + sex + restecg + ca +
                         drug + smoker + fam_hist + fbs, 
                       data = df.train)

varImpPlot(rf)
set.seed(seed)
rf.opt <- randomForest(class ~ cp + thalach + thal + oldpeak + age + exang + 
                         perfusion + trestbps + slope + sex + restecg + ca + 
                         drug + smoker + fam_hist, 
                       data = df.train)
plot(rf.opt)

# Applying the algorithm to the test data
preds.opt <- predict(rf.opt, df.test, type="response")

# Creating a confusion matrix
cm.opt <- table(predicted = preds.opt, truth = df.test$class)
cm.opt

# Checking accuracy
round(((cm.opt[1,1] + cm.opt[2,2]) / nrow(df.test)) * 100, 2)  # accuracy
round((cm.opt[2, 2] / (cm.opt[2, 2] + cm.opt[2, 1]) * 100), 2) # sensitivity
round((cm.opt[1, 1] / (cm.opt[1, 1] + cm.opt[1, 2]) * 100), 2) # specificity

# ROC analysis
res.opt <- roc(class ~ as.numeric(preds.opt), data = df.test)
res.opt$auc
ggroc(res.opt, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res.opt$auc, 2)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Optimising by changing train and test data sampling

# Changing train:test to 80:20
set.seed(seed)
bound.new <- (nrow(imputedata)/4)*3
imputedata <- imputedata[(sample(nrow(imputedata))),] 
df.train.opt <- imputedata[1:bound.new,]
df.test.opt <- imputedata[(bound.new+1):nrow(imputedata),] 

# Training the model
rf.opt2 <- randomForest(class ~ ., 
                        data = df.train.opt)
plot(rf.opt2)
varImpPlot(rf.opt2)

# Applying the algorithm to the test data
preds.opt2 <- predict(rf.opt2, df.test.opt, type="response")

# Creating a confusion matrix
cm.opt2 <- table(predicted = preds.opt2, truth = df.test.opt$class)
cm.opt2

# Checking accuracy
round(((cm.opt2[1,1] + cm.opt2[2,2]) / nrow(df.test.opt)) * 100, 2) # accuracy
round((cm.opt2[2, 2] / (cm.opt2[2, 2] + cm.opt2[2, 1]) * 100), 2) # sensitivity
round((cm.opt2[1, 1] / (cm.opt2[1, 1] + cm.opt2[1, 2]) * 100), 2) # specificity

# ROC analysis
res.opt2 <- roc(class ~ as.numeric(preds.opt2), data = df.test.opt)
res.opt2$auc
ggroc(res.opt2, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res.opt2$auc, 2)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Optimising using hyperparameter tuning

set.seed(seed)
ytrain <- df.train.opt$class
ytest <- df.test.opt$class
xtrain <- df.train.opt %>% select(-class)
xtest <- df.test.opt %>% select(-class)
bestmtry <- tuneRF(xtrain, ytrain, stepFactor=1.5, improve=1e-5, ntree=333)
print(bestmtry)

# Training the model
rf.opt3 <- randomForest(class ~ ., data = df.train.opt,
                        mtry = 3, ntree = 701) #401
plot(rf.opt3)

# Applying the algorithm to the test data
preds.opt3 <- predict(rf.opt3, df.test.opt, type="response")

# Creating a confusion matrix
cm.opt3 <- table(predicted = preds.opt3, truth = df.test.opt$class)
cm.opt3

# Checking accuracy
round(((cm.opt3[1,1] + cm.opt3[2,2]) / nrow(df.test.opt)) * 100, 2) # accuracy
round((cm.opt3[2, 2] / (cm.opt3[2, 2] + cm.opt3[2, 1]) * 100), 2) # sensitivity
round((cm.opt3[1, 1] / (cm.opt3[1, 1] + cm.opt3[1, 2]) * 100), 2) # specificity

# ROC analysis
res.opt3 <- roc(class ~ as.numeric(preds.opt3), data = df.test.opt)
res.opt3$auc
ggroc(res.opt3, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res.opt3$auc, 2)))
