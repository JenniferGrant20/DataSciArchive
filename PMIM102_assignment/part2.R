library(tidyverse)
library(RPostgreSQL) 
library(moments)
library(ggpubr)

drv <- dbDriver('PostgreSQL') 
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',                  
                 port=5432, user='postgres',                  
                 password="") #.rs.askForPassword('Password:')) 

user_option <- "no_input"

# Extracting data on metformin prescribing from the database
metformin_query <- paste0("select practiceid, sum(quantity) as metformin
                          from gp_data_up_to_2015
                          where bnfcode like '0601022B0%'
                          group by practiceid")
metformin_rx <- dbGetQuery(con, metformin_query)

# Extracting data on prescribing of all drugs from the database
all_rx_query <- paste0("select practiceid, sum(quantity) as total_rx
                  from gp_data_up_to_2015
                  group by practiceid")
all_rx <- dbGetQuery(con, all_rx_query)

# Extracting data on rate of hypertension at each practice
hyp_query <- paste0("select orgcode as practiceid, ratio as hypertension_rate
                     from qof_achievement
                     where indicator = 'HYP001'")
hyp_rates <- dbGetQuery(con, hyp_query)

# Extracting data on rate of obesity at each practice
ob_query <- paste0("select orgcode as practiceid, ratio as obesity_rate
                   from qof_achievement
                   where indicator = 'OB001W'")
ob_rates <- dbGetQuery(con, ob_query)

# Joining the extracted data by practiceid into one table
df <- merge(x = metformin_rx, y = all_rx, by = 'practiceid')
df <- transform(df, metformin_rate = metformin / total_rx)
df <- merge(x = df, y = hyp_rates, by = 'practiceid')
df <- merge(x = df, y = ob_rates, by = 'practiceid')

# skewness should be between -2 and +2
# kurtosis should be between -7 and +7

# Testing the 3 variables for normality
string1 <- "Before running a statistical test, we must test each variable for 
normality. If the data is normally distributed, we can use a parametric test, 
such as the Pearson correlation coefficient. If it is not, we must use a non-
parametric test, such as the Kendall rank correlation coefficient."
string2 <- "To test for normality, it is better to use both visual inspection
and descriptive statistics, therefore a Q-Q plot will be displayed for each
variable, as well as skewness and kurtosis values."
string3 <- "Rate of metformin prescribing"
cat(string1, string2, string3, sep="\n\n")

ggqqplot(df$metformin_rate)
string1 <- "The skewness is"
skewness <- skewness(df$metformin_rate)
cat(string1, skewness)
string1<- "The kurtosis is"
kurtosis <- kurtosis(df$metformin_rate)
cat(string1, kurtosis)

string1 <- "As the skewness value is between -2 and +2, the kurtosis value
is between -7 and +7, and the data roughly follows the straight line on the
Q-Q plot, it can be assumed that the data for rate of metformin prescribing
is normally distributed."
string2 <- "Proceed to the next variable?
1. Yes
b. Back to menu
q. Quit"
cat(string1, string2, sep="\n\n")

user_suboption <- readline("Select an option:")
  if (user_suboption == "y"){
    string1 <- "Proceeding to the next variable."
    string2 <- "Rate of hypertension"
    cat(string1, string2, sep="\n\n")
    
  } else if (user_suboption == "b"){
    next
    
  } else if (user_suboption == "q"){
    cat("Thank you for using my program. Goodbye!")
    dbDisconnect(con)
    break
    
  } else {
    string1 <- "Option"
    string2 <- "is not defined. Please enter a listed number or letter."
    cat(string1, user_option, string2)

  }
  

ggqqplot(df$hypertension_rate)
string1 <- "The skewness is"
skewness <- skewness(df$hypertension_rate)
cat(string1, skewness)
string1<- "The kurtosis is"
kurtosis <- kurtosis(df$hypertension_rate)
cat(string1, kurtosis)

string1 <- "As the skewness value is between -2 and +2, the kurtosis value
is between -7 and +7, and the data roughly follows the straight line on the
Q-Q plot, it can be assumed that the data for rate of hypertension is normally 
distributed."
string2 <- "Proceed to the next variable?
1. Yes
b. Back to menu
q. Quit"
cat(string1, string2, sep="\n\n")

user_suboption <- readline("Select an option:")
if (user_suboption == "y"){
  string1 <- "Proceeding to the next variable."
  string2 <- "Rate of obesity"
  cat(string1, string2, sep="\n\n")
  
} else if (user_suboption == "b"){
  next
  
} else if (user_suboption == "q"){
  cat("Thank you for using my program. Goodbye!")
  dbDisconnect(con)
  break
  
} else {
  string1 <- "Option"
  string2 <- "is not defined. Please enter a listed number or letter."
  cat(string1, user_option, string2)
  
}

ggqqplot(df$obesity_rate)
string1 <- "The skewness is"
skewness <- skewness(df$obesity_rate)
cat(string1, skewness)
string1<- "The kurtosis is"
kurtosis <- kurtosis(df$obesity_rate)
cat(string1, kurtosis)

string1 <- "As the skewness value is between -2 and +2, the kurtosis value
is between -7 and +7, and the data roughly follows the straight line on the
Q-Q plot, it can be assumed that the data for rate of hypertension is normally 
distributed."
string2 <- "Proceed to the analysis?
1. Yes
b. Back to menu
q. Quit"
cat(string1, string2, sep="\n\n")

user_suboption <- readline("Select an option:")
if (user_suboption == "y"){
  string1 <- "Proceeding."
  string2 <- "Pearson correlational analysis of the relationship between
  rate of metformin prescribing and rate of hypertension"
  cat(string1, string2, sep="\n\n")
  
} else if (user_suboption == "b"){
  next
  
} else if (user_suboption == "q"){
  cat("Thank you for using my program. Goodbye!")
  dbDisconnect(con)
  break
  
} else {
  string1 <- "Option"
  string2 <- "is not defined. Please enter a listed number or letter."
  cat(string1, user_option, string2)
  
}

hyp_pearson <- cor.test(df$metformin_rate, df$hypertension_rate, 
                        method = "pearson")

degf <- hyp_pearson$parameter
pvalue <- hyp_pearson$p.value
stat <- round(hyp_pearson$estimate, 2)
stat <- str_replace(stat, "^0", "")
stat <- paste("=", stat)

if (pvalue < 0.001){
  pvalue <- "< .001"
} else {
  pvalue <- signif(pvalue, 2)
  pvalue <- round(pvalue, 3)
  pvalue <- str_replace(pvalue, "^0", "")
  pvalue <- paste("=", pvalue)
}

string1 <- "It was found that there was a significant weak positive correlation 
between the rate of metformin prescribing and the rate of hypertension at Welsh
GP practices."
string2 <- paste(paste0("r(", degf, ")"), stat, ", p", pvalue)
cat(string1, string2)

ob_pearson <- cor.test(df$metformin_rate, df$obesity_rate, 
                       method = "pearson")

degf <- ob_pearson$parameter
pvalue <- ob_pearson$p.value
stat <- round(ob_pearson$estimate, 2)
stat <- str_replace(stat, "^0", "")
stat <- paste("=", stat)

if (pvalue < 0.001){
  pvalue <- "< .001"
} else {
  pvalue <- signif(pvalue, 2)
  pvalue <- round(pvalue, 3)
  pvalue <- str_replace(pvalue, "^0", "")
  pvalue <- paste("=", pvalue)
}

string1 <- "It was found that there was a significant weak positive correlation 
between the rate of metformin prescribing and the rate of obesity at Welsh
GP practices."
string2 <- paste(paste0("r(", degf, ")"), stat, ", p", pvalue)
cat(string1, string2)

# which relationship is stronger?
# compare the $estimate

stat_compare <- data.frame(variable = c("hypertension", "obesity"),
                           r = c(hyp_pearson$estimate, ob_pearson$estimate))
stat_max <- which.max(stat_compare$r)

if (stat_max == 1){
  cat("The relationship between the rate of hypertension and the rate of 
  metformin prescribing is stronger, as denoted by the larger r value 
  (correlation coefficient).")
} else if (stat_max == 2){
  cat("The relationship between the rate of obesity and the rate of 
  metformin prescribing is stronger, as denoted by the larger r value 
  (correlation coefficient).")
}