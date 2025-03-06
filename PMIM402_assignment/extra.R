## Extra logistic regression stuff

# model <- stats::step(lr)
# preds.mdl <- predict(model, df.test2, type="response")
# cm.mdl <- table(predicted = as.numeric(preds.mdl>0.5), truth = df.test2$class)
# cm.mdl
# round(((cm.mdl[1,1] + cm.mdl[2,2]) / nrow(df.test2)) * 100, 2)  # accuracy
# round((cm.mdl[2, 2] / (cm.mdl[2, 2] + cm.mdl[2, 1]) * 100), 2) # sensitivity
# round((cm.mdl[1, 1] / (cm.mdl[1, 1] + cm.mdl[1, 2]) * 100), 2) # specificity
# res.lr <- roc(class ~ preds.mdl, data = df.test2)
# res.lr$auc # area under the curve: 0.646
# ggroc(res.lr, legacy.axes = TRUE) +
#   labs(title = paste0("AUC = ", round(res.lr$auc, 2)))
# 
# 
# model2 <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
#   set_engine("glmnet") %>%
#   set_mode("classification") %>%
#   fit(class ~ ., data = df.train2)
# tidy(model2)
# 
# pred.class <- predict(model2, new_data = df.test2, type = "class")
# pred.prob <- predict(model2, new_data = df.test2, type = "prob")
# results <- df.test2 %>% select(class) %>% bind_cols(pred.class)
# cm.mdl2 <- table(predicted = results$.pred_class, truth = results$class)
# cm.mdl2
# round(((cm.mdl2[1,1] + cm.mdl2[2,2]) / nrow(df.test2)) * 100, 2)  # accuracy
# round((cm.mdl2[2, 2] / (cm.mdl2[2, 2] + cm.mdl2[2, 1]) * 100), 2) # sensitivity
# round((cm.mdl2[1, 1] / (cm.mdl2[1, 1] + cm.mdl2[1, 2]) * 100), 2) # specificity
# 
# log.reg <- logistic_reg(mixture = tune(), penalty = tune(), engine = "glmnet")
# grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))
# log.reg.wf <- workflow() %>% 
#   add_model(log.reg) %>%
#   add_formula(class ~ .)
# folds <- vfold_cv(df.train2, v = 7)
# log.reg.tuned <- tune_grid(
#   log.reg.wf,
#   resamples = folds,
#   grid = grid,
#   control = control_grid(save_pred = TRUE)
# )
# select_best(log.reg.tuned, metric = "roc_auc")
# 
# modelfinal <- logistic_reg(penalty = 1, mixture = 0) %>%
#   set_engine("glmnet") %>%
#   set_mode("classification") %>%
#   fit(class ~ ., data = df.train2)
# tidy(modelfinal)
# predfinal <- predict(modelfinal, new_data = df.test2, type = "class")
# results <- df.test2 %>% select(class) %>% bind_cols(predfinal)
# cm.fnl <- table(predicted = results$.pred_class, truth = results$class)
# cm.fnl
# round(((cm.fnl[1,1] + cm.fnl[2,2]) / nrow(df.test2)) * 100, 2) # accuracy
# round((cm.fnl[2, 2] / (cm.fnl[2, 2] + cm.fnl[2, 1]) * 100), 2) # sensitivity
# round((cm.fnl[1, 1] / (cm.fnl[1, 1] + cm.fnl[1, 2]) * 100), 2) # specificity

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Extra Naive Bayes stuff

x <- df.train %>% select(-class)
y <- df.train$class
nb2 <- train(x, y, 'nb', trControl = trainControl(method = 'cv', number = 10))
nb2

preds.nb2 <- as.data.frame(predict(nb2$finalModel, df.test)) 
cm.nb2 <- table(predicted = preds.nb2$class, truth = df.test$class)
cm.nb2
round(((cm.nb2[1,1] + cm.nb2[2,2]) / nrow(df.test)) * 100, 2)  # accuracy
round((cm.nb2[2, 2] / (cm.nb2[2, 2] + cm.nb2[2, 1]) * 100), 2) # sensitivity
round((cm.nb2[1, 1] / (cm.nb2[1, 1] + cm.nb2[1, 2]) * 100), 2) # specificity

##

bound <- (nrow(imputedata)/5)*4
imputedata <- imputedata[(sample(nrow(imputedata))),]
df.train2 <- imputedata[1:bound,]
df.test2 <- imputedata[(bound+1):nrow(imputedata),]

x2 <- df.train2 %>% select(-class)
y2 <- df.train2$class
nb3 <- train(x2, y2, 'nb', trControl = trainControl(method = 'cv', number = 10))
nb3

preds.nb3 <- as.data.frame(predict(nb3$finalModel, df.test2)) 
cm.nb3 <- table(predicted = preds.nb3$class, truth = df.test2$class)
cm.nb3
round(((cm.nb2[1,1] + cm.nb2[2,2]) / nrow(df.test)) * 100, 2)  # accuracy
round((cm.nb2[2, 2] / (cm.nb2[2, 2] + cm.nb2[2, 1]) * 100), 2) # sensitivity
round((cm.nb2[1, 1] / (cm.nb2[1, 1] + cm.nb2[1, 2]) * 100), 2) # specificity

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Naive Bayes

# Training the model using the training data
nb <- naiveBayes(class ~ ., data = df.train)

# Applying the model to the test data and creating a confusion matrix
preds.nb <- predict(nb, df.test)
cm.nb <- table(predicted = preds.nb, truth = df.test$class)
cm.nb

# Checking accuracy
round(((cm.nb[1,1] + cm.nb[2,2]) / nrow(df.test)) * 100, 2) # accuracy
round((cm.nb[2, 2] / (cm.nb[2, 2] + cm.nb[2, 1]) * 100), 2) # sensitivity
round((cm.nb[1, 1] / (cm.nb[1, 1] + cm.nb[1, 2]) * 100), 2) # specificity

# ROC analysis
res.nb <- roc(class ~ as.numeric(preds.nb), data = df.test)
res.nb$auc 
ggroc(res.nb, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res.nb$auc, 2)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Optimising Naive Bayes by changing the sampling ratio of train and test data

# Changing train:test to 80:20
bound.new <- (nrow(imputedata)/5)*4
imputedata <- imputedata[(sample(nrow(imputedata))),] 
df.train.opt <- imputedata[1:bound.new,]
df.test.opt <- imputedata[(bound.new+1):nrow(imputedata),] 

# Training the model
nb.opt <- naiveBayes(class ~ ., data = df.train.opt)
nb.opt

# Applying the model to the test data
preds.opt <- predict(nb.opt, df.test.opt)
cm.opt <- table(predicted = preds.opt, truth = df.test.opt$class)
cm.opt

# Checking accuracy
round(((cm.opt[1,1] + cm.opt[2,2]) / nrow(df.test.opt)) * 100, 2) # accuracy
round((cm.opt[2, 2] / (cm.opt[2, 2] + cm.opt[2, 1]) * 100), 2) # sensitivity
round((cm.opt[1, 1] / (cm.opt[1, 1] + cm.opt[1, 2]) * 100), 2) # specificity

# ROC analysis
res.opt <- roc(class ~ as.numeric(preds.opt), data = df.test.opt)
res.opt$auc 
ggroc(res.opt, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res.opt$auc, 2)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Optimising Naive Bayes by removing features

# Creating a correlation matrix to find highly correlated variables
cormatrix <- rcorr(as.matrix(imputedata))
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

# Removing slope as it was highly correlated with restecg
df.train.opt2 <- df.train.opt %>% select(-Patient_ID)
df.test.opt2 <- df.train.opt %>% select(-Patient_ID)

# Training the model
nb.opt2 <- naiveBayes(class ~ ., data = df.train.opt2)
nb.opt2

# Applying the model to the test data
preds.opt2 <- predict(nb.opt2, df.test.opt2)
cm.opt2 <- table(predicted = preds.opt2, truth = df.test.opt2$class)
cm.opt2

# Checking accuracy
round(((cm.opt2[1, 1] + cm.opt2[2, 2]) / nrow(df.test.opt2)) * 100, 2) # accuracy
round((cm.opt[2, 2] / (cm.opt[2, 2] + cm.opt[2, 1]) * 100), 2) # sensitivity
round((cm.opt[1, 1] / (cm.opt[1, 1] + cm.opt[1, 2]) * 100), 2) # specificity

# ROC analysis
res.opt <- roc(class ~ as.numeric(preds.opt), data = df.test.opt)
res.opt$auc 
ggroc(res.opt, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res.opt$auc, 2)))

## Support Vector Machines

# Setting global parameters
control <- trainControl(method = "cv", number = "5", savePredictions = TRUE, 
                        classProbs = TRUE)

# Training the SVM algorithm using training data
svmfit <- train(class ~ ., data = df.train, 
                method = "svmLinear", fitControl = control)
svmfit

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

ytrain <- df.train$class
ytest <- df.test$class
xtrain <- df.train %>% select(-class)
xtest <- df.test %>% select(-class)

# Run RFE
result_rfe1 <- rfe(x = xtrain,
                   y = ytrain,
                   sizes = c(1:17),
                   rfeControl = control)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)

# Print the results visually
ggplot(data = result_rfe1, metric = "Accuracy") + theme_bw()
ggplot(data = result_rfe1, metric = "Kappa") + theme_bw()

# Post prediction
postResample(predict(result_rfe1, xtest), ytest)