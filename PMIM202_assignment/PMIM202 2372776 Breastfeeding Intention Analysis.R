#### Breastfeeding Intention and Duration Analysis in Wales 2024 ####
#### PMIM202J Health Data Modelling 
#### May 2024 
#### Candidate no: 2372776

#-------------------------------------------------------------------------------
# Packages
library(tidyverse)
library(mice)
library(rpart)
library(rpart.plot)
library(caret)
library(RColorBrewer)

#-------------------------------------------------------------------------------
#### Read the csv file
df <- read.csv("Born in Wales Data June 2024.csv", stringsAsFactors = FALSE)

#-------------------------------------------------------------------------------
#### Preliminary data cleaning
## Description: Explore and reformat the data so that it is more manageable
## (remove ambiguous, irrelevant variables or variables with a lot of missing
## data). Create a binary variable for breastfeeding intention 

# Reformat the column names for greater accessibility
names(df) <- gsub("\\.", "_", names(df))

# Print the updated column names
print(names(df))

# Investigate the unique values in the 'How are you planning to feed your
# baby' column
unique(df$How_are_you_planning_to_feed_your_baby_)

# Create binary variable for breastfeeding intention
df <- df %>%
  mutate(breastfeeding_intent = case_when(
    How_are_you_planning_to_feed_your_baby_ %in% c("Breast milk only", 
                                                   "Breast and bottle", 
                                                   "Breast if possible ", 
                                                   "Breast and expressed milk 
                                                   via bottle", 
                                                   "Breast if all well. If not 
                                                   fed is best either way ") ~ 1,
    How_are_you_planning_to_feed_your_baby_ == "Bottle milk only" ~ 0,
    How_are_you_planning_to_feed_your_baby_ %in% c("", "Don't know yet", 
                                                   "Dont know yet", 
                                                   "Planned breastfeeding, ended 
                                                   up formula feeding") ~ NA_real_,
    TRUE ~ NA_real_
  ))

nrow(df)

# Remove NA values
df <- df %>%
  filter(!is.na(breastfeeding_intent))

# Remove unneeded variables
df <- df %>%
  select(-c(WIMD_2019_Rank,
            WIMD_2019_Decile,
            WIMD_2019_Quintile,
            WIMD_2019_Quartile,
            Have_you_had_symptoms_that_are_associated_with_COVID19__fever__dry_cough__loss_of_taste_or_smell__fatigue__muscle_pain__,
            When_did_your_symptoms_start__approximately__,
            What_symptoms_did_you_have_,
            What_treatment_did_you_have__,
            Have_you_had_a_test_,
            What_is_your_view_on_having_the_COVID_vaccination_in_pregnancy__have_you_or_would_you_have_the_COVID_vaccination_when_pregnant_and_why_,
            PLEASE_PRESS_SUBMIT_AT_THE_BOTTOM__Thank_you_very_much_for_completing_this_questionnaire__If_you_feel_there_is_something_that_we_should_have_asked_or_something_you_were_not_able_to_tell_us_please____,
            Worrying_too_much_about_different_things_,
            Trouble_relaxing_,
            Being_so_restless_that_it_is_hard_to_sit_still_,
            Becoming_easily_annoyed_or_irritable_,
            Feeling_afraid_as_if_something_awful_might_happen_,
            Please_tell_us_the_type_and_amount_of_physical_activity_involved_in_your_work,
            How_many_people__who_are_not_part_of_your_household__did_you_talk_to_in_person_yesterday__e_g__were_within_1_metre_and_exchanged_words_but_did_not_touch___,
            How_many_people__who_are_not_part_of_your_household__did_you_have_direct_physical_contact_with_yesterday__hugged__touched__,
            Cycling__including_cycling_to_work_and_during_leisure_time,
            Do_you_feel_you_were_able_to_answer_the_last_two_questions_above_accurately_,
            Number_of_children,
            How_old_are_your_other_children__please_separate_each_child_s_age_by_a_comma___E_g___2__4_and_8,
            What_is_your_ethnic_group_,
            How_are_you_planning_to_feed_your_baby_,
            Nationality,
            What_is_your_current_relationship_status__,
            Would_you_consider_yourself_to_be_,
            Do_you_drink_alcohol_,
            Not_being_able_to_stop_or_control_worrying_,
            any_serious_relationship_difficulties_with_your_husband_or_partner_or_separated_or_divorced_,
            any_serious_legal_or_financial_problems_,
            were_you_or_someone_close_to_you_a_victim_of_violence_or_crime_,
            someone_close_with_a_serious_illness,
            the_death_of_someone_close_to_you__,
            was_this_stressful_event_related_to_coronavirus_,
            During_this_time_did_you_have_someone_who_could_support_you_emotional_or_financially,
            X0n_a_scale_of_1_to_10__how_stressful_was_this_time___1_is_not_at_all__10_is_overwhelming__,
            How_would_you_describe_your_experience_of_this_pregnancy__support_from_midwife__how_you_feel_about_being_pregnant__,
            What_is__your_occupation__,
            Do_you_work,
            What_is_the_main_language_spoken_in_your_home_,
            Little_interest_or_pleasure_in_doing_things_,
            Feeling_down__depressed__or_hopeless_,
            Trouble_falling_or_staying_asleep__or_sleeping_too_much,
            Feeling_tired_or_having_little_energy_,
            Poor_appetite_or_overeating_,
            Feeling_bad_about_yourself_or_that_you_are_a_failure_of_have_let_yourself_or_family_down_,
            Trouble_concentrating_on_things__such_as_reading_the_newspaper_or_watching_television_,
            Moving_or_speaking_so_slowly_that_other_people_could_have_noticed__Or_the_opposite_being_so_fidgety_or_restless_that_you_have_been_moving_around_a_lot_more_than_usual_,
            Thoughts_that_you_would_be_better_off_dead__or_of_hurting_yourself_in_some_way_,
            Has_COVID19_change_the_type_of_birth_you_feel_you_will_have_,
            Have_you_had_any_periods_of_bad_stress_in_your_pregnancy__,
            How_many_people_live_in_your_home__not_including_you__
  ))

# Rename remaining variables
df <- df %>%
  rename(
    system_id = SYSTEM_ID,
    start_time = Start_time,
    due_date = Expected_date_of_delivery_of_your_baby,
    low_mood = Have_you_experienced_low_mood_during_your_pregnancy_,
    smoker = Do_you_smoke_,
    anxiety = Feeling_nervous__anxious_or_on_edge_,
    sport = Physical_exercise_such_as_swimming__jogging__aerobics__football__tennis__gym_workout_etc_,
    walking = Walking__including_walking_to_work__shopping__for_pleasure_etc_,
    housework_childcare = Housework_Childcare,
    gardening_diy = Gardening_DIY,
    walking_pace = How_would_you_describe_your_usual_walking_pace___Please_mark_one_box_only_,
    weight = My_weight__before_pregnancy__in_Kg_is_,
    height = My_height_in_centimetres_is__,
    healthcare_support = I_feel_satisfied_with_the_support_and_care_I_have_received_in_my_pregnancy_from_my_health_care_team__1_is_strongly_disagree__5_is_strongly_agree_,
    maternity_care_type = What_type_of_maternity_care_are_you_receiving_now_,
    other_children = Do_you_have_other_children__,
    education_level = What_is_the_highest_level_of_education_you_have_reached_,
    working = Are_you_currently_working_,
    income = What_is_the_number_that_best_describes_your_TOTAL_household_income_BEFORE_TAX_,
  )

# Move the 'breastfeeding_intent' variable to the second column position
df <- df %>%
  select(1, breastfeeding_intent, 2:(ncol(df)-1))

#-------------------------------------------------------------------------------
#### Variable cleaning
## Description: Identify NA values, convert variables into factors suitable for
## logistic regression 

# Replace blank values with NA for all columns in the dataframe
df[df == ""] <- NA

# Count the number of NA values in each variable
na_count <-sapply(df, function(y) sum(length(which(is.na(y)))))
na_count


### 'low_mood'
# Convert low_mood to a binary variable
df$low_mood <- ifelse(df$low_mood == "Yes", 1, 0)

# Temporary data frame
temp_df <- df[, c("breastfeeding_intent", "low_mood")]

# Impute missing values
imputed_data <- mice(temp_df, m = 5, method = "pmm", seed = 123)

imputed_df <- complete(imputed_data)

# Incorporate the imputed values into the original data frame
df$low_mood <- imputed_df$low_mood


### 'smoker'
# Create new variable for smoking status
df$smoking_status <- NA

# Recode values in smoker into smoking_status with conditional statements
df$smoking_status <- case_when(
  grepl("never smoked", df$smoker, ignore.case = TRUE) ~ "Never Smoker",
  grepl("stopped|quit|have smoked|used to", df$smoker, ignore.case = TRUE) 
  ~ "Former Smoker",
  grepl("smoke", df$smoker, ignore.case = TRUE) ~ "Current Smoker",
  TRUE ~ NA_character_
)

# Convert to factor
df$smoking_status <- factor(df$smoking_status, 
                            levels = c("Never Smoker", "Former Smoker", 
                                       "Current Smoker"),
                            labels = c(0, 1, 2))

# Remove 'smoker' variable
df <- subset(df, select = -smoker)


### 'anxiety'
# Convert to factor
df$anxiety <- factor(df$anxiety,
                     levels = c("Not at all", "Several days", 
                                "More than half the days", "Nearly everyday"),
                     labels = c(0, 1, 2, 3))


### 'sport'
# Convert to factor
df$sport <- factor(df$sport,
                   levels = c("None", "Some but less than 1 hour", 
                              "1 hour but less than 3 hours", 
                              "3 hours or more"),
                   labels = c(0, 1, 2, 3))


### 'walking'
# Convert to factor
df$walking <- factor(df$walking,
                     levels = c("None", "Some but less than 1 hour", 
                                "1 hour but less than 3 hours", 
                                "3 hours or more"),
                     labels = c(0, 1, 2, 3))


### 'housework_childcare'
df$housework_childcare <- factor(df$housework_childcare,
                                 levels = c("None", "Some but less than 1 hour", 
                                            "1 hour but less than 3 hours", 
                                            "3 hours or more"),
                                 labels = c(0, 1, 2, 3))


### 'gardening_diy'
df$gardening_diy <- factor(df$gardening_diy,
                           levels = c("None", "Some but less than 1 hour", 
                                      "1 hour but less than 3 hours", 
                                      "3 hours or more"),
                           labels = c(0, 1, 2, 3))


### 'walking_pace'
df$walking_pace <- factor(df$walking_pace,
                          levels = c("Slow pace", "Steady average pace", 
                                     "Brisk pace", 
                                     "Fast pace"),
                          labels = c(0, 1, 2, 3))


### 'healthcare_support' - no cleaning necessary


### 'maternity_care_type'
# Create a new variable 'maternity_care'
df$maternity_care <- NA

# Recode the original responses into the new variable
df$maternity_care <- case_when(
  grepl("Consultant led care", 
        df$maternity_care_type, ignore.case = TRUE) ~ "Consultant-led",
  grepl("Midwife led care|Health visitor|Community midwife", 
        df$maternity_care_type, ignore.case = TRUE) ~ "Midwife-led",
  grepl("Consultant led.*midwife led|Midwife and consultant|mixture of both|both", 
        df$maternity_care_type, ignore.case = TRUE) ~ "Shared care",
  grepl("Don't know|Not sure|Too early|N/A|None|Was consultant care|Gave birth|Already have baby|Rainbow clinic",
        df$maternity_care_type, ignore.case = TRUE) ~ "Not applicable",
  TRUE ~ NA_character_
)

# Convert the new variable to a factor with the desired labels
df$maternity_care <- factor(df$maternity_care, 
                            levels = c("Not applicable", "Consultant-led", 
                                       "Midwife-led", "Shared care"),
                            labels = c(0, 1, 2, 3))

# Remove the original 'maternity_care_type' variable
df <- df %>% select(-maternity_care_type)


### 'other_children'
# Replace 'Prefer not to say' with NA
df$other_children <- ifelse(df$other_children == "Prefer not to say", 
                            NA, as.character(df$other_children))

# Convert other_children to a factor with meaningful levels
df$other_children <- factor(df$other_children, 
                            levels = c("No", "Yes"),
                            labels = c(0, 1))


### 'education_level'
# Create a new variable 'education_category'
df$education_category <- NA

# Recode the original responses into the new variable
df$education_category <- case_when(
  df$education_level %in% c("None", "None ") ~ "No formal education",
  df$education_level %in% c("Exams at age 16 (GCSE or equivalent)", 
                            "NVQ via work", 
                            "Vocational qualifications") ~ "Secondary education",
  df$education_level %in% c("Exams at age 18 (A level or equivalent)", "2 nvqs",
                            "CACHE L5", "Higher national diploma ", "Diploma ", 
                            "Diploma of Higher Education", 
                            "Diploma of higher education ", "Dip he", 
                            "DipHE") ~ "Further education",
  df$education_level %in% c("University degree", 
                            "Bachelor of Science in nursing. Qualified nurse in the Philippines and UK", 
                            "Currently studying degree ") ~ "Undergraduate degree",
  df$education_level %in% c("University higher degree", "Master?s Degree ", 
                            "Masters", "PGCE", "Post grad") ~ "Postgraduate degree",
  df$education_level %in% c("Phd", "PhD", "Clinical doctorate", 
                            "Doctorate") ~ "Doctoral degree",
  TRUE ~ NA_character_
)

# Convert the new variable to a factor with meaningful labels
df$education_category <- factor(df$education_category,
                                levels = c("No formal education", 
                                           "Secondary education", 
                                           "Further education", 
                                           "Undergraduate degree", 
                                           "Postgraduate degree", 
                                           "Doctoral degree"),
                                labels = c(0, 1, 2, 3, 4, 5))

# Combine categories to account for small values
df$education_category <- as.factor(case_when(
  df$education_category %in% c(0, 1) ~ "0",
  df$education_category == 2 ~ "1",
  df$education_category == 3 ~ "2",
  df$education_category %in% c(4, 5) ~ "3"
))

# Rename categories to appropriate names
df$education_category <- factor(df$education_category,
                                levels = c("0", "1", "2", "3"),
                                labels = c("Secondary education or less", 
                                           "Further education", 
                                           "Undergraduate degree", 
                                           "Postgrad or higher"))

# Remove the original 'education_level' variable
df <- df %>% select(-education_level)


### 'working'
df$working <- case_when(
  grepl("No", 
        df$working, ignore.case = TRUE) ~ "No",
  grepl("Yes", 
        df$working, ignore.case = TRUE) ~ "Yes",
  TRUE ~ NA_character_
)

df$working <- factor(df$working,
                     levels = c('No','Yes'),
                     labels = c(0, 1))


### 'income'
# Replace 'Prefer not to say' and other values with NA
df$income <- ifelse(df$income %in% c("Prefer not to say", "Perfer not to say", 
                                     "Monthly less than \\x9c5k"), NA, 
                    as.character(df$income))

# Recode the income ranges into the new variable
df$income <- case_when(
  grepl("^Less than", df$income) ~ "Less than £10,000",
  grepl("^Between.+10.+19", df$income) ~ "£10,000 - £19,999",
  grepl("^Between.+20.+29", df$income) ~ "£20,000 - £29,999",
  grepl("^Between.+30.+39", df$income) ~ "£30,000 - £39,999",
  grepl("^Between.+40.+49", df$income) ~ "£40,000 - £49,999",
  grepl("50,000|\\x9c200,000|150,000|earning", df$income) ~ "£50,000 or more",
  TRUE ~ NA_character_
)

# Convert to ordered factor
df$income <- factor(df$income, 
                    levels = c("Less than £10,000", 
                               "£10,000 - £19,999", 
                               "£20,000 - £29,999", 
                               "£30,000 - £39,999", 
                               "£40,000 - £49,999", 
                               "£50,000 or more"),
                    labels = c(0, 1, 2, 3, 4, 5),
                    exclude = NULL)

head(df)
names(df)


#-------------------------------------------------------------------------------
#### Demographics Table
## Description: create a demographics table using the 'tableone' and
## 'kableExtra'packages 

library(tableone)
library(dplyr)
library(kableExtra)
library(stringr)

# Create bmi_demo variable
df <- df %>%
  mutate(bmi_demo = case_when(
    weight / ((height / 100)^2) < 18.5 ~ "Underweight",
    weight / ((height / 100)^2) >= 18.5 & 
      weight / ((height / 100)^2) < 25 ~ "Normal",
    weight / ((height / 100)^2) >= 25 & 
      weight / ((height / 100)^2) < 30 ~ "Overweight",
    weight / ((height / 100)^2) >= 30 ~ "Obese",
    TRUE ~ NA_character_
  )) %>%
  mutate(bmi_demo = factor(bmi_demo, levels = c("Underweight", "Normal", 
                                                "Overweight", "Obese")))

# Variables to include in the table
vars_to_include <- c("low_mood", "anxiety", "sport", "walking", 
                     "housework_childcare", "gardening_diy", "walking_pace", 
                     "healthcare_support", "other_children", "working", 
                     "income", "smoking_status", "maternity_care", 
                     "education_category", "bmi_demo")

# Create a new data frame with only the variables we need
demo_data <- df %>%
  select(breastfeeding_intent, all_of(vars_to_include))

# Ensure breastfeeding_intent is a factor
demo_data$breastfeeding_intent <- factor(demo_data$breastfeeding_intent, 
                                         levels = c(0, 1),
                                         labels = c("Don't intend to breastfeed", 
                                                    "Intend to breastfeed"))

# Create the table
demo_table <- CreateTableOne(vars = vars_to_include, 
                             strata = "breastfeeding_intent", 
                             data = demo_data, 
                             test = FALSE)

# Convert to data frame
demo_table_df <- as.data.frame(print(demo_table, showAllLevels = TRUE, 
                                     printToggle = FALSE, noSpaces = TRUE))

# Rename columns
colnames(demo_table_df)[1] <- "Characteristic"

# Check the number of columns and rename accordingly
if(ncol(demo_table_df) == 3) {
  colnames(demo_table_df)[2:3] <- c("Don't intend to breastfeed", 
                                    "Intend to breastfeed")
} else if(ncol(demo_table_df) == 4) {
  colnames(demo_table_df)[2:4] <- c("All", "Don't intend to breastfeed", 
                                    "Intend to breastfeed")
} else {
  stop("Unexpected number of columns in demo_table_df")
}

# Replace NA with empty string for better display
demo_table_df[is.na(demo_table_df)] <- ""

# Create the formatted table
kable(demo_table_df, format = "html", 
      caption = "Table 1. Demographic Characteristics of Breastfeeding Intention") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", 
                                      "responsive")) %>%
  add_header_above(c(" " = 1, "All" = 1, "Don't intend to breastfeed" = 1, 
                     "Intend to breastfeed" = 1)) %>%
  column_spec(1, bold = T) %>%
  collapse_rows(columns = 1, valign = "top")

# Remove bmi_demo variable after creating the table
df <- df %>% select(-bmi_demo)


#-------------------------------------------------------------------------------
#### Imputation
## Description: impute NA values with Predictive Mean Matching (pmm) using the
## MICE package. Create BMI variable from imputed weight and height variables. 

# Impute the data, excluding non-predictor variables
imputedata <- df %>% select(-start_time, -due_date, -system_id)
imputedata_multi <- mice(imputedata, m = 5, method = 'pmm', seed = 123)

# Perform logistic regression on the first imputed dataset
imputedata <- complete(imputedata_multi, action = 1)

# Calculate BMI
imputedata$bmi <- imputedata$weight / ((imputedata$height / 100)^2)

# Create ordinal BMI category
imputedata$bmi_category <- cut(imputedata$bmi, 
                               breaks = c(0, 18.5, 25, 30, Inf),
                               labels = c("Underweight", "Normal", "Overweight",
                                          "Obese"),
                               right = FALSE)

# Convert to factor
imputedata$bmi_category <- factor(imputedata$bmi_category, 
                                  levels = c("Underweight", "Normal", 
                                             "Overweight", "Obese"),
                                  labels = c(0, 1, 2, 3))
imputedata <- imputedata %>% select(-bmi, -height, -weight)

# Check for multicollinearity
library(car)
vif_model <- lm(breastfeeding_intent ~ ., data = imputedata)
vif_results <- vif(vif_model)
print(vif_results)

# Ensure outcome variable is properly coded
table(imputedata$breastfeeding_intent)


#-------------------------------------------------------------------------------
#### Logistic Regression
## Description: create an initial logistic regression model for general
## comparison with the subsequent Decision Tree model.

logistic_model <- glm(breastfeeding_intent ~ .,
                      data = imputedata, 
                      family = binomial(link = "logit"))

summary(logistic_model)

# Odds ratios and confidence intervals
exp(cbind(OR = coef(logistic_model), confint(logistic_model)))

# Model performance
library(pROC)
roc_curve <- roc(imputedata$breastfeeding_intent, predict(logistic_model, 
                                                          type = "response"))
auc(roc_curve)


#-------------------------------------------------------------------------------
#### Decision tree
library(rpart)
library(rpart.plot)

# Create the decision tree model
tree_model <- rpart(breastfeeding_intent ~ ., 
                    data = imputedata, 
                    method = "class",
                    control = rpart.control(minsplit = 20, cp = 0.0057471))

# Plot the decision tree
rpart.plot(tree_model, 
           extra = 101,
           box.palette = "RdYlGn",
           shadow.col = "gray",
           nn = TRUE)

# Print the summary of the tree
printcp(tree_model)

# Print the detailed output of the tree
summary(tree_model)

# Determine variable importance
var_importance <- tree_model$variable.importance
print(var_importance)


#-------------------------------------------------------------------------------
#### Informed refinement of Logistic Regression model
## Description: create a simplified model, check for multicollinearity, create
## a step model, address perfect separation and cross validate. 

simplified_model <- glm(breastfeeding_intent ~ ., 
                        data = imputedata, 
                        family = binomial(link = "logit"))
summary(simplified_model)

# Check for multicollinearity
library(car)
vif(simplified_model)

# Remove predictors with high VIF values
library(MASS)
step_model <- stepAIC(simplified_model, direction = "both")
summary(step_model)

# Extract the main variable names from the step_model
step_vars <- attr(terms(step_model), "term.labels")

# Address perfect separation with Firth model
library(logistf)
firth_formula <- as.formula(paste("breastfeeding_intent ~", 
                                  paste(step_vars, collapse = " + ")))
firth_model <- logistf(firth_formula, data = imputedata)
summary(firth_model)

# Cross validation
library(caret)
cv_model <- train(firth_formula, data = imputedata, 
                  method = "glm", family = "binomial",
                  trControl = trainControl(method = "cv", number = 10))
print(cv_model)

summary(cv_model)

# Predict on the entire dataset
predictions <- predict(cv_model, newdata = imputedata)

# Ensure both predictions and actual values have same levels
levels_to_use <- c("0", "1")
predictions <- factor(predictions, levels = levels_to_use)
imputedata$breastfeeding_intent <- factor(imputedata$breastfeeding_intent, 
                                          levels = levels_to_use)

# Ensure outcome variable is a factor
imputedata$breastfeeding_intent <- as.factor(imputedata$breastfeeding_intent)

# Retrain as caret package having issues recognizing it as classification model
cv_model <- train(breastfeeding_intent ~ ., 
                  data = imputedata, 
                  method = "glm",
                  family = binomial(),
                  trControl = trainControl(method = "cv", number = 10))

# Calculate ROC-AUC
prob_predictions <- predict(cv_model, newdata = imputedata, type = "prob")[,2]
roc_obj <- roc(imputedata$breastfeeding_intent, prob_predictions)
auc_value <- auc(roc_obj) 
print(paste("AUC:", auc_value))


#-------------------------------------------------------------------------------
#### Addressing Class Imbalance: ROSE (Random Over-Sampling Examples)
library(ROSE)

# Extract the formula from the step_model
step_formula <- formula(step_model)

# Create a new balanced dataset using ROSE with the step_formula
balanced_data <- ROSE(step_formula, data = imputedata, 
                      N = nrow(imputedata))$data

# Check the new class distribution
table(balanced_data$breastfeeding_intent)

# Refit model using the balanced data
balanced_firth_model <- logistf(step_formula, data = balanced_data)
summary(balanced_firth_model)

# Cross-validation with balanced data
balanced_cv_model <- train(step_formula, data = balanced_data, 
                           method = "glm", family = "binomial",
                           trControl = trainControl(method = "cv", number = 10))
print(balanced_cv_model)

# Make predictions on the original imbalanced data
balanced_predictions <- predict(balanced_cv_model, newdata = imputedata)
balanced_conf_matrix <- confusionMatrix(balanced_predictions, 
                                        imputedata$breastfeeding_intent)
print(balanced_conf_matrix)

# Calculate ROC-AUC for the balanced model
balanced_prob_predictions <- predict(balanced_cv_model, newdata = imputedata, 
                                     type = "prob")[,2]
balanced_roc_obj <- roc(imputedata$breastfeeding_intent, 
                        balanced_prob_predictions)
balanced_auc_value <- auc(balanced_roc_obj)
print(paste("Balanced AUC:", balanced_auc_value))

summary(balanced_cv_model)


#-------------------------------------------------------------------------------
#### Addressing Class Imbalance: Random Forest

library(randomForest)
# Ensure breastfeeding intent is a factor
imputedata$breastfeeding_intent <- as.factor(imputedata$breastfeeding_intent)

# Determine the size of each class
class_sizes <- table(imputedata$breastfeeding_intent)
min_class_size <- min(class_sizes)

# Create the Random Forest model
rf_model <- randomForest(breastfeeding_intent ~ ., 
                         data = imputedata, 
                         ntree = 500,
                         mtry = floor(sqrt(ncol(imputedata) - 1)),
                         importance = TRUE,
                         sampsize = rep(min_class_size, 2),
                         strata = imputedata$breastfeeding_intent)

# Print the model summary
print(rf_model)

# Make predictions
rf_predictions <- predict(rf_model, imputedata)

# Create confusion matrix
library(caret)
rf_conf_matrix <- confusionMatrix(rf_predictions, imputedata$breastfeeding_intent)
print(rf_conf_matrix)

# Calculate ROC-AUC
library(pROC)
rf_prob_predictions <- predict(rf_model, imputedata, type = "prob")[,2]
rf_roc_obj <- roc(imputedata$breastfeeding_intent, rf_prob_predictions)
rf_auc_value <- auc(rf_roc_obj)
print(paste("Random Forest AUC:", rf_auc_value))

# Plot variable importance
varImpPlot(rf_model, main = "Variable Importance")

# Print variable importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$variable <- rownames(importance_df)
importance_df <- importance_df[order(importance_df$MeanDecreaseGini, 
                                     decreasing = TRUE),]
print(importance_df)

# Get top 5 important variables
top_vars <- rownames(importance_df)[1:5]

# Create partial dependence plots
par(mfrow=c(2,3))
for(i in 1:5) {
  partialPlot(rf_model, imputedata, top_vars[i], 
              main=paste("Partial Dependence on", top_vars[i]))
}

#-------------------------------------------------------------------------------
#### Model Plots
library(broom)
library(sjPlot)
library(ggplot2)
library(gridExtra)


### Cross validation model - ROC
library(pROC)
library(ggplot2)

# Predict probabilities using cv_model
cv_prob_predictions <- predict(cv_model, newdata = imputedata, type = "prob")[,2]

# Create ROC object
cv_roc_obj <- roc(imputedata$breastfeeding_intent, cv_prob_predictions)

# Plot ROC curve
ggroc(cv_roc_obj) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve for Cross-Validated Logistic Regression Model",
       x = "False Positive Rate", 
       y = "True Positive Rate") +
  theme_minimal() +
  annotate("text", x = 0.75, y = 0.25, 
           label = paste("AUC:", round(auc(cv_roc_obj), 3)))


# Calculate sensitivity and specificity at various thresholds
roc_results <- coords(cv_roc_obj, "all")

# Print AUC with confidence interval
ci_auc <- ci.auc(cv_roc_obj)
print(paste("AUC:", round(auc(cv_roc_obj), 3), 
            "95% CI:", round(ci_auc[1], 3), "-", round(ci_auc[3], 3)))

# Find optimal threshold
optimal_threshold <- coords(cv_roc_obj, "best", best.method = "youden")
print(paste("Optimal threshold:", round(optimal_threshold[1, "threshold"], 3)))
print(paste("Sensitivity at optimal threshold:", 
            round(optimal_threshold[1, "sensitivity"], 3)))
print(paste("Specificity at optimal threshold:", 
            round(optimal_threshold[1, "specificity"], 3)))


### Cross validation model - OR plot
coef_summary <- summary(cv_model$finalModel)$coefficients
odds_ratios <- exp(coef_summary[, "Estimate"])
ci_lower <- exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"])
ci_upper <- exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"])

plot_data <- data.frame(
  term = rownames(coef_summary),
  estimate = odds_ratios,
  conf.low = ci_lower,
  conf.high = ci_upper
)

# Create custom labels for the plot
anxiety_labels <- c("1" = "Anxiety: several days",
                    "2" = "Anxiety: more than half the days",
                    "3" = "Anxiety: nearly everyday")

bmi_labels <- c("1" = "BMI: normal",
                "2" = "BMI: overweight",
                "3" = "BMI: obese")

education_labels <- c("Secondary education or less" = "Education: secondary or less",
                      "Further education" = "Education: further education",
                      "Undergraduate degree" = "Education: undergraduate degree",
                      "Postgrad or higher" = "Education: postgrad or higher")

gardening_labels <- c("1" = "Gardening: <1 hour",
                      "2" = "Gardening: 1-3 hours",
                      "3" = "Gardening: 3+ hours")

healthcare_labels <- c("1" = "Healthcare support: 1",
                       "2" = "Healthcare support: 2",
                       "3" = "Healthcare support: 3",
                       "4" = "Healthcare support: 4",
                       "5" = "Healthcare support: 5")

housework_labels <- c("1" = "Housework/childcare: <1 hour",
                  "2" = "Housework/childcare: 1-3 hours",
                  "3" = "Housework/childcare: 3+ hours")

income_labels <- c("1" = "Income: £10,000 - £19,999",
                   "2" = "Income: £20,000 - £29,999",
                   "3" = "Income: £30,000 - £39,999",
                   "4" = "Income: £40,000 - £49,999",
                   "5" = "Income: £50,000+")

low_mood_labels <- c("0" = "No low mood", "1" = "Low mood")

maternity_labels <- c("1" = "Maternity care: consultant-led",
                      "2" = "Maternity care: midwife-led",
                      "3" = "Maternity care: shared care")

other_children_labels <- c("1" = "Other children")

smoking_labels <- c("1" = "Smoker: former",
                    "2" = "Smoker: current")

sport_labels <- c("1" = "Sport: <1 hour",
                  "2" = "Sport: 1-3 hours",
                  "3" = "Sport: 3+ hours")

walking_labels <- c("1" = "Walking: <1 hour",
                         "2" = "Walking: 1-3 hours",
                         "3" = "Walking: 3+ hours")

walking_pace_labels <- c("1" = "Walking pace: steady average",
                         "2" = "Walking pace: brisk",
                         "3" = "Walking pace: fast")

working_labels <- c("1" = "Working")

# Function to replace labels
replace_labels <- function(term) {
  variable <- sub("^([a-zA-Z_]+).*", "\\1", term)
  level <- sub("^[a-zA-Z_]+", "", term)
  
  label_list <- switch(variable,
                       anxiety = anxiety_labels,
                       bmi_category = bmi_labels,
                       education_category = education_labels,
                       gardening_diy = gardening_labels,
                       healthcare_support = healthcare_labels,
                       housework_childcare = housework_labels,
                       income = income_labels,
                       low_mood = low_mood_labels,
                       maternity_care = maternity_labels,
                       other_children = other_children_labels,
                       smoking_status = smoking_labels,
                       sport = sport_labels,
                       walking = walking_labels,
                       walking_pace = walking_pace_labels,
                       working = working_labels,
                       NULL)
  
  if (!is.null(label_list)) {
    if (variable == "education_category") {
      return(label_list[level])
    } else if (level %in% names(label_list)) {
      return(label_list[level])
    }
  }
  return(term)
}

# Apply the label replacement
plot_data$term <- sapply(plot_data$term, replace_labels)

# Create the plot
ggplot(plot_data, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(x = NULL, y = "Odds Ratio", 
       title = NULL) +
  theme_minimal() +
  scale_y_log10() +
  theme(axis.text.y = element_text(hjust = 0))

# Get tidy summary of the model
tidy_summary <- tidy(cv_model$finalModel, conf.int = TRUE, exponentiate = TRUE)
print(tidy_summary, n = 39)


#-------------------------------------------------------------------------------
#### Factors affecting duration of breastfeeding
follow_up_data <- read.csv("Born in Wales Follow-up Data June 2024.csv", 
                           stringsAsFactors = FALSE)

# Add system_id variable back to the imputedata data frame
imputedata_surv <- imputedata
imputedata_surv$system_id <- df$system_id

# Merge imputedata_surv with follow_up_data
merged_data <- merge(imputedata_surv, follow_up_data, by.x = "system_id", 
                     by.y = "STUDY_ID", all.x = TRUE)

# Create income_category variable with high/low income
merged_data$income_category <- ifelse(merged_data$income %in% c("0", "1"), 
                                      "Low income", "Higher income")

# Create survival object
library(survival)
surv_object <- Surv(time = merged_data$Follow.Up.Time, 
                    event = merged_data$Stopped.Breastfeeding)

# Perform survival analysis
surv_fit <- survfit(Surv(Follow.Up.Time, Stopped.Breastfeeding) ~ low_mood + 
                      smoking_status + income + other_children + bmi_category, 
                    data = merged_data)
summary(surv_fit)

# Create Kaplan Meier curves
library(survminer)
library(ggplot2)

# Create individual Kaplan-Meier curves with adjusted legends and customizations
p1 <- ggsurvplot(survfit(Surv(Follow.Up.Time, Stopped.Breastfeeding) ~ low_mood, 
                         data = merged_data),
                 title = "Low Mood", title.position = "center", legend = "top", 
                 legend.labs = c("No", "Yes"), legend.title = element_blank(),
                 legend.text.size = 5, 
                 xlab = "Time (days)", 
                 conf.int = TRUE, conf.int.style = "ribbon",
                 conf.int.alpha = 0.2, conf.int.fill = "gray",
                 palette = c("#E69F00", "#56B4E9"))

p2 <- ggsurvplot(survfit(Surv(Follow.Up.Time, Stopped.Breastfeeding) ~ 
                           smoking_status, data = merged_data),
                 title = "Smoking Status", title.position = "center", 
                 legend = "top", 
                 legend.labs = c("Never Smoker", "Former Smoker", 
                                 "Current Smoker"), 
                 legend.title = element_blank(),
                 legend.text.size = 5,
                 xlab = "Time (days)",
                 conf.int = TRUE, conf.int.style = "ribbon",
                 conf.int.alpha = 0.2, conf.int.fill = "gray",
                 palette = c("#009E73", "#F0E442", "#0072B2"))

p3 <- ggsurvplot(survfit(Surv(Follow.Up.Time, Stopped.Breastfeeding) ~ income, 
                         data = merged_data),
                 title = "Income", title.position = "center",
                 legend = "top", 
                 legend.labs = c("Less than £10,000", "£10,000 - £19,999", 
                                 "£20,000 - £29,999", "£30,000 - £39,999", 
                                 "£40,000 - £49,999", "£50,000 or more"), 
                 legend.title = element_blank(),
                 legend.text.size = 5,
                 xlab = "Time (days)",
                 conf.int = TRUE, conf.int.style = "ribbon",
                 conf.int.alpha = 0.2, conf.int.fill = "gray",
                 palette = c("#E69F00", "#56B4E9", "#009E73", 
                             "#F0E442", "#0072B2", "#D55E00"))

p4 <- ggsurvplot(survfit(Surv(Follow.Up.Time, Stopped.Breastfeeding) ~ 
                           other_children, data = merged_data),
                 title = "Other Children", title.position = "center",
                 legend = "top", 
                 legend.labs = c("No", "Yes"), legend.title = element_blank(),
                 legend.text.size = 5,
                 xlab = "Time (days)",
                 conf.int = TRUE, conf.int.style = "ribbon",
                 conf.int.alpha = 0.2, conf.int.fill = "gray",
                 palette = c("#E69F00", "#56B4E9"))

p5 <- ggsurvplot(survfit(Surv(Follow.Up.Time, Stopped.Breastfeeding) ~ 
                           bmi_category, data = merged_data),
                 title = "BMI", title.position = "center",
                 legend = "top", 
                 legend.labs = c("Underweight", "Normal", "Overweight", 
                                 "Obese"), legend.title = element_blank(),
                 legend.text.size = 5,
                 xlab = "Time (days)",
                 conf.int = TRUE, conf.int.style = "ribbon",
                 conf.int.alpha = 0.2, conf.int.fill = "gray",
                 palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442"))

p6 <- ggsurvplot(survfit(Surv(Follow.Up.Time, Stopped.Breastfeeding) ~ 
                           income_category, data = merged_data),
                 title = "Income: Low vs High", title.position = "center",
                 legend = "top", 
                 legend.labs = c("Low income (<£20,000 pa)", 
                                 "Higher income (£20,000+ pa)"), 
                 legend.title = element_blank(),
                 legend.text.size = 5,
                 xlab = "Time (days)",
                 conf.int = TRUE, conf.int.style = "ribbon",
                 conf.int.alpha = 0.2, conf.int.fill = "gray",
                 palette = c("#E69F00", "#56B4E9"))

# Arrange the plots in a grid
arranged_plots <- arrange_ggsurvplots(list(p1, p2, p3, p4, p5, p6), 
                                      ncol = 2, nrow = 3, 
                                      surv.plot.height = 0.4, 
                                      surv.plot.width = 0.4,
                                      main = "Kaplan-Meier Curves for Breastfeeding Duration")

# Save the arranged plots
ggsave("km_curves_updated.png", plot = arranged_plots, 
       width = 16, height = 16, dpi = 300)

### Cox Regression

library(survival)

cox_model <- coxph(Surv(Follow.Up.Time, Stopped.Breastfeeding) ~ 
                     low_mood + smoking_status + income_category + 
                     other_children + bmi_category, 
                   data = merged_data)

summary(cox_model)

# Forest plot of hazard ratios
ggforest(cox_model, data = merged_data)

# Check proportional hazards assumption
phtest <- cox.zph(cox_model)
print(phtest)
plot(phtest)

### Create table for Cox regression
library(tidyverse)
library(finalfit)
library(flextable)
library(officer)

# Create the cox_results data frame as before
cox_results <- summary(cox_model)$coefficients %>% 
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  mutate(
    HR = exp(coef),
    CI_lower = exp(coef - 1.96 * `se(coef)`),
    CI_upper = exp(coef + 1.96 * `se(coef)`)
  )

# Format the table
cox_table <- cox_results %>%
  mutate(
    `Hazard Ratio (95% CI)` = sprintf("%.2f (%.2f-%.2f)", HR, CI_lower, CI_upper),
    `p-value` = ifelse(`Pr(>|z|)` < 0.001, "<0.001", sprintf("%.3f", `Pr(>|z|)`))
  ) %>%
  dplyr::select(Variable, `Hazard Ratio (95% CI)`, `p-value`)

# Create a flextable
ft <- flextable(cox_table)

# Customize the table
ft <- ft %>%
  set_caption("Cox Proportional Hazards Model Results") %>%
  theme_apa() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  fontsize(size = 11, part = "all")

# View the table in RStudio
ft

# Save as a Word document
save_as_docx(ft, path = "cox_results_table.docx")


