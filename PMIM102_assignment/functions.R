#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for extracting and joining tables for analysing the relationship
# between hypertension and obesity rates and rate of prescribing a certain drug

compile_df <- function(bnfcode){
  drug_query <- paste0("SELECT practiceid, SUM(quantity) AS quantity
                       FROM gp_data_up_to_2015
                       WHERE bnfcode LIKE'", bnfcode,
                       "%'GROUP BY practiceid")
  drug_rx <- dbGetQuery(con, drug_query)
  
  all_rx_query <- paste0("SELECT practiceid, SUM(quantity) AS total_rx
                         FROM gp_data_up_to_2015
                         GROUP BY practiceid")
  all_rx <- dbGetQuery(con, all_rx_query)
  
  hyp_query <- paste0("SELECT orgcode AS practiceid, ratio AS hypertension_ratio
                      FROM qof_achievement
                      WHERE indicator = 'HYP001'")
  hyp_rates <- dbGetQuery(con, hyp_query)
  
  ob_query <- paste0("SELECT orgcode AS practiceid, ratio AS obesity_ratio
                     FROM qof_achievement
                     WHERE indicator = 'OB001W'")
  ob_rates <- dbGetQuery(con, ob_query)
  
  df <- merge(x = drug_rx, y = all_rx, by = 'practiceid')
  df <- transform(df, drug_ratio = quantity / total_rx)
  df <- merge(x = df, y = hyp_rates, by = 'practiceid')
  df <- merge(x = df, y = ob_rates, by = 'practiceid')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for testing normality

normality <- function(df_variable, var_type){
  
  if (var_type == "hypertension"){
    
    plot <- ggqqplot(df_variable)
    print(plot)
    string1 <- "The skewness is"
    skewness <- skewness(df_variable)
    cat("\n", string1, skewness)
    string1<- "The kurtosis is"
    kurtosis <- kurtosis(df_variable)
    cat("\n", string1, kurtosis)
    
    string1 <- "The skewness value satisfies the normality assumption."
    string2 <- "The kurtosis value also satisfies the normality assumption."
    cat("\n\n", string1,"\n\n", string2)
    
  } else if (var_type == "obesity"){
    
    plot <- ggqqplot(df_variable)
    print(plot)
    string1 <- "The skewness is"
    skewness <- skewness(df_variable)
    cat("\n", string1, skewness)
    string1<- "The kurtosis is"
    kurtosis <- kurtosis(df_variable)
    cat("\n", string1, kurtosis)
    
    string1 <- "The skewness value satisfies the normality assumption."
    string2 <- "The kurtosis value also satisfies the normality assumption."
    cat("\n\n", string1,"\n\n", string2)
    
  } else if (var_type == "drug"){
    
    plot <- ggqqplot(df_variable)
    print(plot)
    string1 <- "The skewness is"
    skewness <- skewness(df_variable)
    cat("\n", string1, skewness)
    string1<- "The kurtosis is"
    kurtosis <- kurtosis(df_variable)
    cat("\n", string1, kurtosis)
    
    if (skewness >= -2 & skewness <= +2){
      string1 <- "The skewness value satisfies the normality assumption."
      
    } else {
      string1 <- "The skewness value does not satisfy the normality assumption."
    }
    
    if (kurtosis >= -7 & kurtosis <= +7){
      string2 <- "The kurtosis value satisfies the normality assumption."
      
    } else {
      string2 <- "The kurtosis value does not satisfy the normality assumption."
    }
    
    cat("\n\n", string1,"\n\n", string2)
    
  }
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function which formats p value for reporting statistical test results

pformat <- function(pvalue){
  
  if (pvalue < 0.001){
    pvalue <- "< .001"
  } else {
    pvalue <- signif(pvalue, 2)
    pvalue <- round(pvalue, 3)
    pvalue <- str_replace(pvalue, "^0", "")
    pvalue <- paste("=", pvalue)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for Pearson or Kendall correlation

correlation <- function(bnfcode, df, name_of_drug, test){
  
  if (test == "pearson"){
    
    cat("\n\n", "Pearson correlation:")
    
    hyp_test <- cor.test(x = df$drug_ratio, y = df$hypertension_ratio, 
                            method = "pearson")
    
  } else if (test == "kendall"){
    
    cat("\n\n", "Kendall rank correlation:")
    
    hyp_test <- cor.test(df$drug_ratio, df$hypertension_ratio, 
                            method = "kendall")
    
  }
  
  
  degf <- hyp_test$parameter
  pvalue <- hyp_test$p.value
  pvalue <- pformat(pvalue)
  stat <- round(hyp_test$estimate, 2)
  stat <- str_replace(stat, "^0", "")
  stat <- paste("=", stat)
  
  
  psignif <- function(pvalue, x){
    
    if (pvalue <= 0.05){
      x <- "significant"
    } else if (pvalue > 0.05){
      x <- "non-significant"
    }
  }
  
  
  rstrength <- function(estimate, x){
    
    if (estimate >= 0 & estimate <= 0.19){
      x <- 'very weak positive'
    } else if (estimate >= 0.20 & estimate <= 0.39){
      x <- 'weak positive'
    } else if (estimate >= 0.40 & estimate <= 0.59){
      x <- 'moderate positive'
    } else if (estimate >= 0.60 & estimate <= 0.79){
      x <- 'strong positive'
    } else if (estimate >= 0.80 & estimate <= 1){
      x <- 'very strong positive'
    } else if (estimate >= -0 & estimate <= -0.19){
      x <- 'very weak positive'
    } else if (estimate >= -0.20 & estimate <= -0.39){
      x <- 'weak positive'
    } else if (estimate >= -0.40 & estimate <= -0.59){
      x <- 'moderate positive'
    } else if (estimate >= -0.60 & estimate <= -0.79){
      x <- 'strong positive'
    } else if (estimate >= -0.80 & estimate <= -1){
      x <- 'very strong positive'
    }
  }
  
  
  string1 <- "It was found that there was a"
  hyp_signif <- psignif(hyp_test$p.value) 
  hyp_rstrength <- rstrength(hyp_test$estimate)
  string1 <- paste(string1, hyp_signif, hyp_rstrength)
  string1 <- paste(string1, "correlation between the rate of")
  string2 <- "prescribing and the rate of hypertension at Welsh GP practices."
  string3 <- paste(paste0("r(", degf, ")"), stat, ", p", pvalue)
  cat("\n\n", string1, "\n", name_of_drug, string2, "\n", string3)
  
  
  if (test == "pearson"){
    
    ob_test <- cor.test(x = df$drug_ratio, y = df$obesity_ratio, 
                         method = "pearson")
    
  } else if (test == "kendall"){
    
    ob_test <- cor.test(df$drug_ratio, df$obesity_ratio, 
                         method = "kendall")
    
  }

  
  degf <- ob_test$parameter
  pvalue <- ob_test$p.value
  pvalue <- pformat(pvalue)
  stat <- round(ob_test$estimate, 2)
  stat <- str_replace(stat, "^0", "")
  stat <- paste("=", stat)
  
  string1 <- "It was found that there was a"
  ob_signif <- psignif(ob_test$p.value) 
  ob_rstrength <- rstrength(ob_test$estimate)
  string1 <- paste(string1, ob_signif, ob_rstrength)
  string1 <- paste(string1, "correlation between the rate of")
  string2 <- "prescribing and the rate of obesity at Welsh GP practices."
  string3 <- paste(paste0("r(", degf, ")"), stat, ", p", pvalue)
  cat("\n\n", string1, "\n", name_of_drug, string2, "\n", string3)
  
  # which relationship is stronger?
  
  if (hyp_test$estimate < 0 & ob_test$estimate > 0){
    hyp_r <- 0 - hyp_test$estimate 
    ob_r <- ob_test$estimate
  } else if (hyp_test$estimate > 0 & ob_test$estimate < 0){
    hyp_r <- hyp_test$estimate 
    ob_r <- 0 - ob_test$estimate
  } else if (hyp_test$estimate < 0 & ob_test$estimate < 0){
    hyp_r <- 0 - hyp_test$estimate
    ob_r <- 0 - ob_test$estimate
  } else if (hyp_test$estimate > 0 & ob_test$estimate > 0){
    hyp_r <- hyp_test$estimate
    ob_r <- ob_test$estimate
  }
  
  stat_compare <- data.frame(variable = c("hypertension", "obesity"),
                             r = c(hyp_r, ob_r),
                             p = c(hyp_test$p.value,
                                   ob_test$p.value))
  
  stat_max <- which.max(stat_compare$r)
  
  if (stat_max == 1 & hyp_test$p.value < 0.05 & ob_test$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_test$p.value < 0.05 & 
             ob_test$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger as the 
    analysis for obesity was non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_test$p.value > 0.05 & 
             ob_test$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger as the 
    analysis for hypertension was non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_test$p.value > 0.05 & 
             ob_test$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_test$p.value < 0.05 & 
             ob_test$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_test$p.value < 0.05 & 
             ob_test$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_test$p.value > 0.05 & 
             ob_test$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger but 
    both relationships are non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_test$p.value > 0.05 & 
             ob_test$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger but 
    both relationships are non-significant."
    cat("\n\n", string1, string2)
  }
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
