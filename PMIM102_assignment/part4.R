# move options 5 and 6 to start menu
# change start menu option 1 to practice-specific analyses
# add start menu option for health board-specific analyses
# check bnfcode is valid in options 5 and 6

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hb <- data.frame(name = c("Betsi Cadwaladr University Health Board",
                          "Hywel Dda Health Board",
                          "Abertawe Bro Morgannwg University Health Board",
                          "Cardiff & Vale University Health Board",
                          "Cwm Taf Health Board",
                          "Aneurin Bevan Health Board",
                          "Powys Teaching Health Board"),
                 code = c("7A1", "7A2", "7A3", "7A4", "7A5", "7A6", "7A7"))
view(hb)

user_hb <- readline("Code of the health board you wish to analyse:")
user_hb_name <- hb[hb$code == user_hb,]
user_hb_name <- user_hb_name$name

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Option 7: 

string1 <- "Option 7 selected"
string2 <- "Generating a table showing the top ten drugs by total cost spent at"
cat(string1, "\n\n", string2, user_hb_name)
option_7 <- paste0("SELECT b.chemical AS drug, SUM(actcost) AS cost
                   FROM gp_data_up_to_2015 a
                   LEFT JOIN chemsubstance b 
                   ON b.bnfchemical = LEFT(a.bnfcode, 9)
                   WHERE hb ='", user_hb,
                   "'GROUP BY b.chemical
                   ORDER BY cost DESC
                   LIMIT 10")
top_10_cost <- dbGetQuery(con, option_7)
print(top_10_cost)
cat("\n", "Analysis complete")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Option 8:

string1 <- "Option 8 selected"
string2 <- "Generating a table showing the top five prescribed drug categories 
by total cost spent at the specified health board."
cat(string1, string2, sep="\n\n")
option_8 <- paste0("SELECT sectiondesc AS drug_category, cost
                           FROM (
                                 SELECT o.hb, p.bnfchemical, o.bnfcode, 
                                 p.sectiondesc, o.bnfname, 
                                 SUM(o.actcost) OVER (PARTITION BY 
                                 p.sectiondesc) AS cost
                                 FROM gp_data_up_to_2015 o 
                                 LEFT JOIN
                                 bnf p
                                 ON p.bnfchemical = LEFT(o.bnfcode, 9)
                                 WHERE hb ='", user_hb, 
                                 "'GROUP BY p.sectiondesc, p.bnfchemical, 
                                 o.bnfcode, o.bnfname, o.hb, o.actcost
                           ) X
                           GROUP BY sectiondesc, cost
                           ORDER BY cost DESC
                           LIMIT 5")
option_8_table <- dbGetQuery(con, option_8)
print(option_8_table)
cat("\n", "Analysis complete")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Option 9:

string1 <- "Option 9 selected"
string2 <- "Generating a table ranking the health boards by number of practices"
cat(string1, string2, sep="\n\n")
option_9 <- "SELECT hb as code, COUNT(practiceid) AS quantity
            FROM (
                SELECT DISTINCT(a.practiceid), b.hb
                FROM address a
                INNER JOIN gp_data_up_to_2015 b
                ON a.practiceid = b.practiceid
               ) x
            GROUP BY hb
            ORDER BY quantity DESC"
option_9_table <- dbGetQuery(con, option_9)
option_9_table <- cbind(name = hb$name, option_9_table)
print(option_9_table)
cat("\n", "Analysis complete")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Option 10: 

# get user to specify period as YYYYMM
# Monthly prescription data is available from April 2013 (201304) to December
# 2015 (201512).

user_period <- readline("Enter a period (YYYYMM):")

check_period <- "SELECT DISTINCT(period)
                 FROM gp_data_up_to_2015"
check_period <- dbGetQuery(con, check_period)

if (any(check_period$period == user_period)){
  cat("Period", user_period, "selected.")
} else {
  print("Invalid period. Check format matches YYYYMM and try again.")
}

option_10 <- paste0("SELECT DISTINCT(hb) as code, SUM(quantity) AS total_items
                    FROM gp_data_up_to_2015
                    WHERE period ='", user_period,
                    "'GROUP BY hb
                    ORDER BY total_items DESC")
option_10_table <- dbGetQuery(con, option_10)
option_10_table <- cbind(name = hb$name, option_10_table)
print(option_10_table)

hb_treemap <- treemap(option_10_table,
                      index="name",
                      vSize="total_items",
                      type="index",
                      fontsize.labels = 10,
                      fontface.labels = 3)
print(hb_treemap)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Option 11

# extract data

df <- paste0("SELECT b.hb, a.ratio
                      FROM qof_achievement  a
                      INNER JOIN gp_data_up_to_2015 b
                      ON a.orgcode = b.practiceid
                      WHERE a.indicator = 'HYP001' AND b.hb = '7A4'
                            OR a.indicator = 'HYP001' AND b.hb = '7A7'
                      GROUP BY b.practiceid, b.hb, a.indicator, a.ratio")
df <- dbGetQuery(con, df)


# normality test

table_7A4 <- df[df$hb == '7A4',]
normality(table_7A4$ratio, "hypertension")

table_7A7 <- df[df$hb == '7A7',]
normality(table_7A7$ratio, "hypertension")

# normally distributed?

if (skewness(table_7A4$ratio) >= -2 & skewness(table_7A4$ratio) <= 2 &
    kurtosis(table_7A4$ratio) >= -7 & kurtosis(table_7A4$ratio) <= 7 & 
    skewness(table_7A7$ratio) >= -2 & skewness(table_7A7$ratio) <= 2 &
    kurtosis(table_7A7$ratio) >= -7 & kurtosis(table_7A7$ratio) <= 7){
      
      # true: bartlett's test
  
      homoscedasticity <- bartlett.test(ratio ~ hb, data = df)
      print(homoscedasticity)
      p_var <- homoscedasticity$p.value
     
        # equality of variances?
      
      if (p_var <= 0.05){
       
        # false: u test
        
        string1 <- "The result was significant, meaning homoscedasticity"
        string2 <- "cannot be assumed, and a non-parametric test conducted."
        cat(string1, string2)   
        
        stattest <- wilcox.test(ratio ~ hb, data = df)
        
        bxp <- ggboxplot(
          df, x = "hb", y = "ratio", 
          ylab = "Rate of Hypertension", xlab = "Health Board", 
          add = "jitter"
        )
        
        print(bxp)
        
        pvalue <- stattest$p.value
        
        if (pvalue <= 0.05){
          string1 <- "A Mann Whitney U test showed that the difference"
          string2 <- "was statistically significant"
          cat(string1, string2)
        } else if (pvalue > 0.05){
          string1 <- "A Mann Whitney U test showed that the difference"
          string2 <- "was statistically non-significant"
          cat(string1, string2)
        }
        
        pvalue <- pformat(pvalue)
        stat <- round(stattest$statistic, 2)
        stat <- str_replace(stat, "^0", "")
        stat <- paste("=", stat)
        string1 <- paste("U", stat, ", p", pvalue)
        cat(string1)
        
      } else if (p_var > 0.5) {
        
        # true: t test
        
        string1 <- "The result was non-significant, meaning homoscedasticity"
        string2 <- "can be assumed, and a parametric test conducted."
        cat(string1, string2) 
        
        stattest <- t.test(ratio ~ hb, data = df)
        
        bxp <- ggboxplot(
          df, x = "hb", y = "ratio", 
          ylab = "Rate of Hypertension", xlab = "Health Board", 
          add = "jitter"
        )
        
        print(bxp)
        
        string1 <- "The mean hypertension ratio in group 7A4 is"
        string2 <- "whereas the mean in group 7A7 is"
        cat(string1, stattest$estimate[[1]], string2, stattest$estimate[[2]])
        
        pvalue <- stattest$p.value
        
        if (pvalue <= 0.05){
          string1 <- "A Welch two-samples t-test showed that the difference"
          string2 <- "was statistically significant"
          cat(string1, string2)
        } else if (pvalue > 0.05){
          string1 <- "A Welch two-samples t-test showed that the difference"
          string2 <- "was statistically non-significant"
          cat(string1, string2)
        }
        
        pvalue <- pformat(pvalue)
        degf <- round(stattest$parameter, 0)
        stat <- round(stattest$statistic, 2)
        stat <- str_replace(stat, "^0", "")
        stat <- paste("=", stat)
        string1 <- paste(paste0("t(", degf, ")"), stat, ", p", pvalue)
        cat(string1)
        
        }
      
      } else {
        
        # false: fligner-killeen's test
        
        homoscedasticity <- fligner.test(ratio ~ hb, data = df)
        print(homoscedasticity)
        p_var <- homoscedasticity$p.value
        
        if (p_var <= 0.05){
          string1 <- "The result was significant, meaning homoscedasticity"
          string2 <- "cannot be assumed, and a non-parametric test conducted."
          cat(string1, string2)    
          
          stattest <- wilcox.test(ratio ~ hb, data = df)
          
          bxp <- ggboxplot(
            df, x = "hb", y = "ratio", 
            ylab = "Rate of Hypertension", xlab = "Health Board", 
            add = "jitter"
          )
          
          print(bxp)
          
          pvalue <- stattest$p.value
          
          if (pvalue <= 0.05){
            string1 <- "A Mann Whitney U test showed that the difference"
            string2 <- "was statistically significant"
            cat(string1, string2)
          } else if (pvalue > 0.05){
            string1 <- "A Mann Whitney U test showed that the difference"
            string2 <- "was statistically non-significant"
            cat(string1, string2)
          }
          
          pvalue <- pformat(pvalue)
          stat <- round(stattest$statistic, 2)
          stat <- str_replace(stat, "^0", "")
          stat <- paste("=", stat)
          string1 <- paste("U", stat, ", p", pvalue)
          cat(string1)
          
          } else if (p_var > 0.5) {
            string1 <- "The result was non-significant, meaning homoscedasticity"
            string2 <- "can be assumed, and a non-parametric test conducted."
            cat(string1, string2)  
            
            stattest <- wilcox.test(ratio ~ hb, data = df)
            
            bxp <- ggboxplot(
              df, x = "hb", y = "ratio", 
              ylab = "Rate of Hypertension", xlab = "Health Board", 
              add = "jitter"
            )
            
            print(bxp)
            
            pvalue <- stattest$p.value
            
            if (pvalue <= 0.05){
              string1 <- "A Mann Whitney U test showed that the difference"
              string2 <- "was statistically significant"
              cat(string1, string2)
            } else if (pvalue > 0.05){
              string1 <- "A Mann Whitney U test showed that the difference"
              string2 <- "was statistically non-significant"
              cat(string1, string2)
            }
            
            pvalue <- pformat(pvalue)
            stat <- round(stattest$statistic, 2)
            stat <- str_replace(stat, "^0", "")
            stat <- paste("=", stat)
            string1 <- paste("U", stat, ", p", pvalue)
            cat(string1)
            
          }
        }


# correlation matrix for earlier part

df_new <- data.frame(df$drug_ratio, df$hypertension_ratio, df$obesity_ratio)
# need to find way to change names
res <- cor(df_new, method = "pearson")
res
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)