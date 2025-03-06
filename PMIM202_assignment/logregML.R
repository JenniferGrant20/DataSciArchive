#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Testing the validity of the predictions 

newdata <- logregdata %>% 
  select(rltnship_status, anxiety_severity, other_children,
         depression_severity)

library(pROC) # For roc() and ggroc()

pred <- predict(model3,
                newdata,
                type = "response")
pred 

pred_outcome <- ifelse(pred < 0.5, 0, 1)
pred_outcome <- factor(pred_outcome,
                       levels = c(0, 1),
                       labels = c("No", 
                                  "Yes"))

tab <- table(logregdata$intention_to_feed, pred_outcome,
             dnn = c("observed", "predicted"))
tab

# correctly predicted the absence of intention to breastfeed for 7 women
# incorrectly predicted the presence of intention to breastfeed for 127 women
# incorrectly predicted the absence of intention to breastfeed for 3 women
# correctly predicted the presence of intention to breastfeed for 607 women

accuracy <- sum(diag(tab)) / sum(tab)
accuracy
# 82.93%

sensitivity <- tab[2, 2] / (tab[2, 2] + tab[2, 1])
sensitivity
# 99.67%

specificity <- tab[1, 1] / (tab[1, 1] + tab[1, 2])
specificity
# 6.72%

res <- roc(intention_to_feed ~ fitted(model3),
           data = logregdata)

res$auc # area under the curve: 0.646

ggroc(res, legacy.axes = TRUE) +
  labs(title = paste0("AUC = ", round(res$auc, 2)))
