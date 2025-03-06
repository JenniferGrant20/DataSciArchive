# Function for testing normality

normality <- function(df_variable, var_type){
  
  # df_variable should be a variable from a data frame, e.g. df$drug_ratio
  # var_type should be "hypertension", "obesity", or "drug" depending on what 
  # is being analysed (hypertension and obesity ratios are the same in any
  # analysis whereas the drug ratio can differ widely between drugs, so the 
  # code accounts for the unpredictability of the latter var_type)
  
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
