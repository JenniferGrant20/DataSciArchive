
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function

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

drug_list_query <- "SELECT DISTINCT(bnfchemical) as bnfcode, chemicaldesc as drug 
                   FROM bnf
                   WHERE bnfsection = '601'"
drug_list <- dbGetQuery(con, drug_list_query)
view(drug_list)

bnfcode <- readline("Specify the BNF code of the drug you wish to analyse:")

name_of_drug <- drug_list[drug_list$bnfcode == bnfcode,]
name_of_drug <- name_of_drug$drug
name_of_drug <- trimws(name_of_drug)

df <- compile_df(bnfcode)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function 

normality <- function(df_variable){
  plot <- ggqqplot(df_variable)
  print(plot)
  string1 <- "The skewness is"
  skewness <- skewness(df_variable)
  cat("\n", string1, skewness)
  string1<- "The kurtosis is"
  kurtosis <- kurtosis(df_variable)
  cat("\n", string1, kurtosis)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

normality(df$hypertension_ratio)
normality(df$obesity_ratio)
normality(df$drug_ratio)


if (skewness(df$drug_ratio) >= -2 & skewness(df$drug_ratio) <= 2 &
    kurtosis(df$drug_ratio) >= -7 & kurtosis(df$drug_ratio) <= 7){
  pearson()
} else {
  kendall()
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for Pearson correlation

pearson <- function(){
  
  cat("\n\n", "Pearson correlation:")
  
  hyp_pearson <- cor.test(df$drug_ratio, df$hypertension_ratio, 
                          method = "pearson")
  
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
  
  degf <- hyp_pearson$parameter
  pvalue <- hyp_pearson$p.value
  pvalue <- pformat(pvalue)
  stat <- round(hyp_pearson$estimate, 2)
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
  hyp_signif <- psignif(hyp_pearson$p.value) 
  hyp_rstrength <- rstrength(hyp_pearson$estimate)
  string1 <- paste(string1, hyp_signif, hyp_rstrength)
  string1 <- paste(string1, "correlation between the rate of")
  string2 <- "prescribing and the rate of hypertension at Welsh GP practices."
  string3 <- paste(paste0("r(", degf, ")"), stat, ", p", pvalue)
  cat("\n\n", string1, "\n", name_of_drug, string2, "\n", string3)
  
  
  ob_pearson <- cor.test(df$drug_ratio, df$obesity_ratio, 
                         method = "pearson")
  
  degf <- ob_pearson$parameter
  pvalue <- ob_pearson$p.value
  pvalue <- pformat(pvalue)
  stat <- round(ob_pearson$estimate, 2)
  stat <- str_replace(stat, "^0", "")
  stat <- paste("=", stat)
  
  string1 <- "It was found that there was a"
  ob_signif <- psignif(ob_pearson$p.value) 
  ob_rstrength <- rstrength(ob_pearson$estimate)
  string1 <- paste(string1, ob_signif, ob_rstrength)
  string1 <- paste(string1, "correlation between the rate of")
  string2 <- "prescribing and the rate of obesity at Welsh GP practices."
  string3 <- paste(paste0("r(", degf, ")"), stat, ", p", pvalue)
  cat("\n\n", string1, "\n", name_of_drug, string2, "\n", string3)
  
  # which relationship is stronger?
  
  if (hyp_pearson$estimate < 0 & ob_pearson$estimate > 0){
    hyp_r <- 0 - hyp_pearson$estimate 
    ob_r <- ob_pearson$estimate
  } else if (hyp_pearson$estimate > 0 & ob_pearson$estimate < 0){
    hyp_r <- hyp_pearson$estimate 
    ob_r <- 0 - ob_pearson$estimate
  } else if (hyp_pearson$estimate < 0 & ob_pearson$estimate < 0){
    hyp_r <- 0 - hyp_pearson$estimate
    ob_r <- 0 - ob_pearson$estimate
  } else if (hyp_pearson$estimate > 0 & ob_pearson$estimate > 0){
    hyp_r <- hyp_pearson$estimate
    ob_r <- ob_pearson$estimate
  }
  
  stat_compare <- data.frame(variable = c("hypertension", "obesity"),
                             r = c(hyp_r, ob_r),
                             p = c(hyp_pearson$p.value,
                                   ob_pearson$p.value))
  
  stat_max <- which.max(stat_compare$r)
  
  if (stat_max == 1 & hyp_pearson$p.value < 0.05 & ob_pearson$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_pearson$p.value < 0.05 & 
             ob_pearson$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger as the 
    analysis for obesity was non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_pearson$p.value > 0.05 & 
            ob_pearson$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger as the 
    analysis for hypertension was non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_pearson$p.value > 0.05 & 
             ob_pearson$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_pearson$p.value < 0.05 & 
            ob_pearson$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_pearson$p.value < 0.05 & 
             ob_pearson$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_pearson$p.value > 0.05 & 
             ob_pearson$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger but 
    both relationships are non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_pearson$p.value > 0.05 & 
             ob_pearson$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger but 
    both relationships are non-significant."
    cat("\n\n", string1, string2)
  }
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function for Kendall rank correlation

kendall <- function(){
  
  cat("\n\n", "Kendall rank correlation:")
  
  hyp_kendall <- cor.test(df$drug_ratio, df$hypertension_ratio, 
                          method = "kendall")
  
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
  
  degf <- hyp_kendall$parameter
  pvalue <- hyp_kendall$p.value
  pvalue <- pformat(pvalue)
  stat <- round(hyp_kendall$estimate, 2)
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
  hyp_signif <- psignif(hyp_kendall$p.value) 
  hyp_rstrength <- rstrength(hyp_kendall$estimate)
  string1 <- paste(string1, hyp_signif, hyp_rstrength)
  string1 <- paste(string1, "correlation between the rate of")
  string2 <- "prescribing and the rate of hypertension at Welsh GP practices."
  string3 <- paste(paste0("r(", degf, ")"), stat, ", p", pvalue)
  cat("\n\n", string1, "\n", name_of_drug, string2, "\n", string3)
  
  
  ob_kendall <- cor.test(df$drug_ratio, df$obesity_ratio, 
                         method = "kendall")
  
  degf <- ob_kendall$parameter
  pvalue <- ob_kendall$p.value
  pvalue <- pformat(pvalue)
  stat <- round(ob_kendall$estimate, 2)
  stat <- str_replace(stat, "^0", "")
  stat <- paste("=", stat)
  
  string1 <- "It was found that there was a"
  ob_signif <- psignif(ob_kendall$p.value) 
  ob_rstrength <- rstrength(ob_kendall$estimate)
  string1 <- paste(string1, ob_signif, ob_rstrength)
  string1 <- paste(string1, "correlation between the rate of")
  string2 <- "prescribing and the rate of obesity at Welsh GP practices."
  string3 <- paste(paste0("r(", degf, ")"), stat, ", p", pvalue)
  cat("\n\n", string1, "\n", name_of_drug, string2, "\n", string3)
  
  # which relationship is stronger?
  
  if (hyp_kendall$estimate < 0 & ob_kendall$estimate > 0){
    hyp_r <- 0 - hyp_kendall$estimate 
    ob_r <- ob_kendall$estimate
  } else if (hyp_kendall$estimate > 0 & ob_kendall$estimate < 0){
    hyp_r <- hyp_kendall$estimate 
    ob_r <- 0 - ob_kendall$estimate
  } else if (hyp_kendall$estimate < 0 & ob_kendall$estimate < 0){
    hyp_r <- 0 - hyp_kendall$estimate
    ob_r <- 0 - ob_kendall$estimate
  } else if (hyp_kendall$estimate > 0 & ob_kendall$estimate > 0){
    hyp_r <- hyp_kendall$estimate
    ob_r <- ob_kendall$estimate
  }
  
  stat_compare <- data.frame(variable = c("hypertension", "obesity"),
                             r = c(hyp_r, ob_r),
                             p = c(hyp_kendall$p.value,
                                   ob_kendall$p.value))
  
  stat_max <- which.max(stat_compare$r)
  
  if (stat_max == 1 & hyp_kendall$p.value < 0.05 & ob_kendall$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_kendall$p.value < 0.05 & 
             ob_kendall$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger as the 
    analysis for obesity was non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_kendall$p.value > 0.05 & 
             ob_kendall$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger as the 
    analysis for hypertension was non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_kendall$p.value > 0.05 & 
             ob_kendall$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_kendall$p.value < 0.05 & 
             ob_kendall$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_kendall$p.value < 0.05 & 
             ob_kendall$p.value < 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger."
    cat("\n\n", string1, string2)
  } else if (stat_max == 1 & hyp_kendall$p.value > 0.05 & 
             ob_kendall$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of hypertension is stronger but 
    both relationships are non-significant."
    cat("\n\n", string1, string2)
  } else if (stat_max == 2 & hyp_kendall$p.value > 0.05 & 
             ob_kendall$p.value > 0.05){
    string1 <- paste("The relationship between the rate of", name_of_drug)
    string2 <- "prescribing and the rate of obesity is stronger but 
    both relationships are non-significant."
    cat("\n\n", string1, string2)
  }
  
}

