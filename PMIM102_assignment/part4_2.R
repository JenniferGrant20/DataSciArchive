cat("Option 5 selected", "\n\n")

# Extract data from database

df <- paste0("SELECT b.hb, a.ratio
                      FROM qof_achievement  a
                      INNER JOIN gp_data_up_to_2015 b
                      ON a.orgcode = b.practiceid
                      WHERE a.indicator = 'HYP001' AND b.hb = '7A4'
                            OR a.indicator = 'HYP001' AND b.hb = '7A7'
                      GROUP BY b.practiceid, b.hb, a.indicator, a.ratio")
df <- dbGetQuery(con, df)

# Testing for assumption of normality

cat("\n", "Normality assumption: rate of hypertension in area 7A4")
table_7A4 <- df[df$hb == '7A4',]
normality(table_7A4$ratio, "hypertension")

cat("\n\n", "Normality assumption: rate of hypertension in area 7A7")
table_7A7 <- df[df$hb == '7A7',]
normality(table_7A7$ratio, "hypertension")

if (skewness(table_7A4$ratio) >= -2 & skewness(table_7A4$ratio) <= 2 &
    kurtosis(table_7A4$ratio) >= -7 & kurtosis(table_7A4$ratio) <= 7 & 
    skewness(table_7A7$ratio) >= -2 & skewness(table_7A7$ratio) <= 2 &
    kurtosis(table_7A7$ratio) >= -7 & kurtosis(table_7A7$ratio) <= 7){
  test_type <- "parametric"
} else {
  test_type <- "nonparametric"
}

# Testing for equality of variances

if (test_type == "parametric"){
  homoscedasticity <- bartlett.test(ratio ~ hb, data = df)
  cat("\n\n")
  print(homoscedasticity)
  p_var <- homoscedasticity$p.value
} else if (test_type == "nonparametric"){
  homoscedasticity <- fligner.test(ratio ~ hb, data = df)
  cat("\n\n")
  print(homoscedasticity)
  p_var <- homoscedasticity$p.value
}

if (p_var > 0.05 & test_type == "parametric"){
  
  # Code for parametric two sample t-test
  
  string1 <- "The result was non-significant, meaning homoscedasticity"
  string2 <- "can be assumed, and a parametric test conducted."
  cat(string1, string2) 
  
  stattest <- t.test(ratio ~ hb, data = df)
  
  # Plotting a boxplot for visual representation of the data
  
  bxp <- ggboxplot(
    df, x = "hb", y = "ratio", 
    ylab = "Rate of Hypertension", xlab = "Health Board", 
    add = "jitter"
  )
  
  print(bxp)
  
  # Reporting the results
  
  string1 <- "The mean hypertension ratio in group 7A4 is"
  string2 <- "whereas the mean in group 7A7 is"
  cat("\n", string1, stattest$estimate[[1]], 
      string2, stattest$estimate[[2]])
  
  pvalue <- stattest$p.value
  
  if (pvalue <= 0.05){
    string1 <- "A Welch two-samples t-test showed that the difference"
    string2 <- "was statistically significant."
    cat("\n", string1, string2)
  } else if (pvalue > 0.05){
    string1 <- "A Welch two-samples t-test showed that the difference"
    string2 <- "was statistically non-significant."
    cat("\n", string1, string2)
  }
  
  pvalue <- pformat(pvalue)
  degf <- round(stattest$parameter, 0)
  stat <- round(stattest$statistic, 2)
  stat <- str_replace(stat, "^0", "")
  stat <- paste("=", stat)
  string1 <- paste(paste0("t(", degf, ")"), stat, ", p", pvalue)
  cat("\n", string1)
  
} else if (p_var <= 0.05 | test_type == "nonparametric"){
  
  # Code for non-parametric Mann Whitney U test
  
  string1 <- "The result was significant, meaning homoscedasticity"
  string2 <- "cannot be assumed, and a non-parametric test conducted."
  cat(string1, string2)   
  
  stattest <- wilcox.test(ratio ~ hb, data = df)
  
  # Plotting a boxplot for visual representation of the data
  
  bxp <- ggboxplot(
    df, x = "hb", y = "ratio", 
    ylab = "Rate of Hypertension", xlab = "Health Board", 
    add = "jitter"
  )
  
  print(bxp)
  
  # Reporting the results
  
  pvalue <- stattest$p.value
  
  if (pvalue <= 0.05){
    string1 <- "A Mann Whitney U test showed that the difference"
    string2 <- "was statistically significant."
    cat("\n", string1, string2)
  } else if (pvalue > 0.05){
    string1 <- "A Mann Whitney U test showed that the difference"
    string2 <- "was statistically non-significant."
    cat("\n", string1, string2)
  }
  
  pvalue <- pformat(pvalue)
  stat <- round(stattest$statistic, 2)
  stat <- str_replace(stat, "^0", "")
  stat <- paste("=", stat)
  string1 <- paste("U", stat, ", p", pvalue)
  cat("\n\n", string1)
  
} 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Allowing for user input

cat("Option 6 selected", "\n\n")
string1 <- "In this analysis, you may choose two health boards"
string2 <- "to compare the prevalence of a user-specified condition."

# Allowing the user to choose 2 health boards to compare

hb <- data.frame(name = c("Betsi Cadwaladr University Health Board",
                          "Hywel Dda Health Board",
                          "Abertawe Bro Morgannwg University Health Board",
                          "Cardiff & Vale University Health Board",
                          "Cwm Taf Health Board",
                          "Aneurin Bevan Health Board",
                          "Powys Teaching Health Board"),
                 code = c("7A1", "7A2", "7A3", "7A4", "7A5", "7A6", "7A7"))
view(hb)

user_hb1 <- readline("Code of the first health board you wish to analyse:")
user_hb1_name <- hb[hb$code == user_hb,]
user_hb1_name <- user_hb_name$name

user_hb2 <- readline("Code of the second health board you wish to analyse:")
user_hb2_name <- hb[hb$code == user_hb,]
user_hb2_name <- user_hb_name$name

# Allowing the user to choose a health condition to analyse

diag <- data.frame(condition = c("atrial fibrillation", "asthma", "cancer",
                                 "coronary heart disease", 
                                 "chronic obstructive pulmonary disease",
                                 "dementia", "diabetes", "epilepsy",
                                 "heart failure", "hypertension",
                                 "learning disabilities", "obesity",
                                 "osteoporosis", "peripheral arterial disease",
                                 "rheumatoid arthritis", 
                                 "schizophrenia, bipolar affective disorder, 
                                 and other psychoses", 
                                 "stroke and transient ischaemic attacks"),
                   code = c("AF001", "AST001", "CAN001", "CHD001", "COPD001",
                            "DEM001", "DM001", "EP001", "HF001", "HYP001",
                            "LD001", "OB001W", "OST001", "PAD001", "RA001",
                            "MH001", "STIA001"))
view(diag)

user_diag <- readline("Code of the condition you wish to analyse:")
user_diag_name <- diag[diag$code == user_diag,]
user_diag_name <- user_diag_name$condition

# Extract data from database

df <- paste0("SELECT b.hb, a.ratio
                      FROM qof_achievement  a
                      INNER JOIN gp_data_up_to_2015 b
                      ON a.orgcode = b.practiceid
                      WHERE a.indicator = '", user_diag, 
                             "'AND b.hb = '", user_hb1,
                       "'OR a.indicator = '", user_diag, 
                             "'AND b.hb = '", user_hb2,
                      "'GROUP BY b.practiceid, b.hb, a.indicator, a.ratio")
df <- dbGetQuery(con, df)

# Testing for assumption of normality

string1 <- "Normality assumption: rate of"
string2 <- "in area"
cat("\n", string1, user_diag_name, string2, user_hb1)
table_hb1 <- df[df$hb == user_hb1,]
normality(table_hb1$ratio, "drug")

string1 <- "Normality assumption: rate of"
string2 <- "in area"
cat("\n", string1, user_diag_name, string2, user_hb2)
table_hb2 <- df[df$hb == user_hb2,]
normality(table_hb2$ratio, "drug")

if (skewness(table_hb1$ratio) >= -2 & skewness(table_hb1$ratio) <= 2 &
    kurtosis(table_hb1$ratio) >= -7 & kurtosis(table_hb1$ratio) <= 7 & 
    skewness(table_hb2$ratio) >= -2 & skewness(table_hb2$ratio) <= 2 &
    kurtosis(table_hb2$ratio) >= -7 & kurtosis(table_hb2$ratio) <= 7){
  test_type <- "parametric"
} else {
  test_type <- "nonparametric"
}

# Testing for equality of variances

if (test_type == "parametric"){
  homoscedasticity <- bartlett.test(ratio ~ hb, data = df)
  cat("\n\n")
  print(homoscedasticity)
  p_var <- homoscedasticity$p.value
} else if (test_type == "nonparametric"){
  homoscedasticity <- fligner.test(ratio ~ hb, data = df)
  cat("\n\n")
  print(homoscedasticity)
  p_var <- homoscedasticity$p.value
}

if (p_var > 0.05 & test_type == "parametric"){
  
  # Code for parametric two sample t-test
  
  string1 <- "The result was non-significant, meaning homoscedasticity"
  string2 <- "can be assumed, and a parametric test conducted."
  cat(string1, string2) 
  
  stattest <- t.test(ratio ~ hb, data = df)
  
  # Plotting a boxplot for visual representation of the data
  
  bxp <- ggboxplot(
    df, x = "hb", y = "ratio", 
    ylab = paste("Rate of", user_diag_name), xlab = "Health Board", 
    add = "jitter"
  )
  
  print(bxp)
  
  # Reporting the results
  
  string1 <- paste("The mean", user_diag_name, "ratio in group", user_hb1, "is")
  string2 <- paste("whereas the mean in group", user_hb2, "is")
  cat("\n", string1, stattest$estimate[[1]], 
      string2, stattest$estimate[[2]])
  
  pvalue <- stattest$p.value
  
  if (pvalue <= 0.05){
    string1 <- "A Welch two-samples t-test showed that the difference"
    string2 <- "was statistically significant."
    cat("\n", string1, string2)
  } else if (pvalue > 0.05){
    string1 <- "A Welch two-samples t-test showed that the difference"
    string2 <- "was statistically non-significant."
    cat("\n", string1, string2)
  }
  
  pvalue <- pformat(pvalue)
  degf <- round(stattest$parameter, 0)
  stat <- round(stattest$statistic, 2)
  stat <- str_replace(stat, "^0", "")
  stat <- paste("=", stat)
  string1 <- paste(paste0("t(", degf, ")"), stat, ", p", pvalue)
  cat("\n", string1)
  
} else if (p_var <= 0.05 | test_type == "nonparametric"){
  
  # Code for non-parametric Mann Whitney U test
  
  string1 <- "The result was significant, meaning homoscedasticity"
  string2 <- "cannot be assumed, and a non-parametric test conducted."
  cat(string1, string2)   
  
  stattest <- wilcox.test(ratio ~ hb, data = df)
  
  # Plotting a boxplot for visual representation of the data
  
  bxp <- ggboxplot(
    df, x = "hb", y = "ratio", 
    ylab = paste("Rate of", user_diag_name), xlab = "Health Board", 
    add = "jitter"
  )
  
  print(bxp)
  
  # Reporting the results
  
  pvalue <- stattest$p.value
  
  if (pvalue <= 0.05){
    string1 <- "A Mann Whitney U test showed that the difference"
    string2 <- "was statistically significant."
    cat("\n", string1, string2)
  } else if (pvalue > 0.05){
    string1 <- "A Mann Whitney U test showed that the difference"
    string2 <- "was statistically non-significant."
    cat("\n", string1, string2)
  }
  
  pvalue <- pformat(pvalue)
  stat <- round(stattest$statistic, 2)
  stat <- str_replace(stat, "^0", "")
  stat <- paste("=", stat)
  string1 <- paste("U", stat, ", p", pvalue)
  cat("\n\n", string1)
  
} 


  