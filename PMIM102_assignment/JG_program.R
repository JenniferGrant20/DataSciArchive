#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Student No. 2005070
# PMIM102J Assignment (2023/24)
# GP Database Project: Program File

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Contents List (referring to the assessment PDF)

# Specific Questions                         # Open-ended Analysis
# 1.  line 58                                # 1. line 736
# 2.  line 369                               # 2. line 976
# 3.  line 537                               # 3. line 1147

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries used, sourcing functions, database link and defining variable
# outside of the loop

library(RPostgreSQL) 
library(tidyverse)
library(ggpubr)
library(moments)
library(treemap)
library(car)

source("JG_functions.R")

drv <- dbDriver('PostgreSQL') 
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',                  
                 port=5432, user='postgres',                  
                 password="") #.rs.askForPassword('Password:')) 

user_option <- "no_input"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating function and while loop (loop while user doesn't select quit)

myprogram <- function(){
  string1 <- "Welcome to Jennifer’s program"
  string2 <- "This program will allow you to run analyses on Welsh GP practice" 
  string3 <- "data from 2015."
  cat(string1, "\n\n", string2, string3, "\n\n")
  while (user_option != 'q'){
    
    # Creating main menu of options for analysis and allowing the user to select
    
    string1 <- "Main Menu:
    1. GP practice-specific analyses
    2. Correlation between metformin prescriptions and hypertension/obesity
    3. Correlation between a chosen diabetic drug and hypertension/obesity
    4. Health board-specific analyses
    5. Comparison between Cardiff and Powys' prevalence of hypertension
    6. Comparison between user-specified health boards' prevalence of a condition
    q. Quit"
    cat("\n", string1)
    user_option <- readline("Select an option:")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Main menu option 1
    # Allowing the user to specify a practice ID for practice-specific analyses
    
    if (user_option == "1"){
      
      string1 <- "Option 1 selected"
      string2 <- "Please specify a GP practice to run analyses on."
      cat(string1, string2, sep="\n\n")
      user_practiceid <- readline("Practice ID:")
      
      # Checking that the user entered a valid practice ID 
      # (user gets 2 chances before being redirected to main menu)
      
      id_options <- "select * from address"
      id_options_table <- dbGetQuery(con, id_options)
      
      if (any(id_options_table$practiceid == user_practiceid)){
        cat("Practice", user_practiceid, "selected.")
      } else {
        cat("Invalid practice ID. Check format matches W00000 and try again.")
        user_practiceid <- readline("Practice ID:")
        if (any(id_options_table$practiceid == user_practiceid)){
          cat("Practice", user_practiceid, "selected.")
        } else {
          cat("Invalid practice ID. Check format matches W00000 and try again.")
          next          
        }
      }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Sub menu of analyses which appears when a practice ID is specified
      # and allowing user to input selection
      
      while (user_option != 'q' | user_option != 'b'){ 
        string1 <- "Sub Menu:
      1. Ten most commonly prescribed drugs
      2. Five most commonly used prescribed drug categories
      3. Size of GP practice
      4. Obesity and hypertension rates
      b. Back to main menu
      q. Quit"
        cat("\n\n", string1)
        user_option <- readline("Select an analysis to run:")
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Code for sub option 1 
        # Top ten most commonly prescribed drugs
        
        if (user_option == "1"){
          
          string1 <- "Option 1 selected"
          string2 <- "Generating a table showing the top ten most commonly" 
          string3 <- "prescribed drugs and their prescription quantities at the" 
          string4 <- "specified practice"
          cat(string1, "\n\n", string2, string3, string4, "\n\n")
          
          # Joining GP prescription data to BNF data to avoid overly specific
          # drug categorisation ('bnfname') and instead using the basic name of 
          # the drug ('chemical'), i.e. 'Paracetamol' instead of separate items
          # for different dosages and tablets/capsules. The sum of prescriptions 
          # for each drug was therefore grouped by 'chemical'. Ordered by 
          # descending quantity and limited by 10 to provide the top 10.
          
          option_1 <- paste0("SELECT b.chemical AS drug, SUM(quantity) AS quantity
                              FROM gp_data_up_to_2015 a
                              LEFT JOIN chemsubstance b 
                              ON b.bnfchemical = LEFT(a.bnfcode, 9)
                              WHERE practiceid ='", user_practiceid,
                            "'GROUP BY b.chemical
                              ORDER BY quantity DESC
                              LIMIT 10")
          top_10_drugs <- dbGetQuery(con, option_1)
          print(top_10_drugs)
          cat("\n", "Analysis complete")
          
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for sub option 2: top five most commonly prescribed drug
          # categories
          
        } else if (user_option == "2"){
          
          string1 <- "Option 2 selected"
          string2 <- "Generating a table showing the top five most commonly"
          string3 <- "prescribed drug categories and their prescription"
          string4 <- "quantities at the specified practice."
          cat(string1, "\n\n", string2, string3, string4, "\n\n")
          
          # Selecting the name of each drug category (drug_category) and the sum 
          # of prescription quantities grouped by drug category (total) for the
          # specified practice ID. Ordering by total descending and limiting to
          # 5 to provide the top 5.
          
          option_2 <- paste0("SELECT sectiondesc AS drug_category, total
                              FROM (
                                 SELECT o.practiceid, p.bnfchemical, o.bnfcode, 
                                        p.sectiondesc, o.bnfname, o.quantity, 
                                        SUM(o.quantity) OVER (PARTITION BY 
                                        p.sectiondesc) AS total
                                 FROM gp_data_up_to_2015 o 
                                 LEFT JOIN bnf p
                                 ON p.bnfchemical = LEFT(o.bnfcode, 9)
                                 WHERE o.practiceid ='", user_practiceid,
                               "'GROUP BY p.sectiondesc, p.bnfchemical, 
                                 o.bnfcode, o.bnfname, o.practiceid, o.quantity
                              ) X
                              GROUP BY sectiondesc, total
                              ORDER BY total DESC
                              LIMIT 5")
          option_2_table <- dbGetQuery(con, option_2)
          print(option_2_table)
          cat("\n", "Analysis complete")
          
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for sub option 3
          # Returning the size category of specified practice
          
        } else if (user_option == "3"){
          
          cat("Option 3 selected")
          
          # Using numerator (patient diagnoses) as a proxy for practice size, 
          # extracting all practice IDs and the sum of each practice's 
          # numerators (quantity), and ordering by quantity descending
          
          option_3 <- paste0("SELECT orgcode AS practiceid, SUM(numerator) AS 
                           quantity 
                           FROM qof_achievement
                           GROUP BY orgcode
                           ORDER BY quantity DESC")
          option_3_table <- dbGetQuery(con, option_3)
          
          # Deleting the first row because it is not a practice ID but 'WAL' and
          # is therefore a nationwide sum not of interest here
          option_3_table <- option_3_table[-c(1), ]
          
          # Calculating the mean size of GP practices to use as cut-off point
          # between small and large categories
          mean_size <- mean(option_3_table[,2])
          
          # Numerically defining the specified practice's size
          practice_size <- option_3_table[option_3_table$practiceid == 
                                            user_practiceid, ]
          
          # Returning the result of whether the practice is small or large
          
          size_result <- if (practice_size[, 2] >= mean_size) {
            string1 <- paste("Practice", user_practiceid, "is large.")
            cat("\n\n", string1, "\n\n", "Analysis complete")
          } else {
            string1 <- paste("Practice", user_practiceid, "is small.")
            cat("\n", string1, "\n", "Analysis complete")
          }
          
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for sub option 4
          # Comparing the rates of hypertension and obesity
          
        } else if (user_option == "4"){
          
          string1 <- "Option 4 selected"
          string2 <- "Generating a table showing the rates of hypertension and"
          string3 <- "obesity at the specified GP practice, across all GP" 
          string4 <- "practices in Wales, and at practices categorised as small" 
          string5 <- "or large."
          cat(string1, "\n\n", string2, string3, string4, string5, "\n\n")
          
          # Extracting the data on hypertension and obesity rates from database
          
          opt4 <- paste0("SELECT orgcode AS practiceid, ratio, centile, indicator
                          FROM qof_achievement
                          WHERE indicator = 'HYP001'
                          OR indicator = 'OB001W'")
          opt4_table <- dbGetQuery(con, opt4)
          
          # Splitting the rates table to create a hypertension rates table
          opt4_table_hyp <- opt4_table[opt4_table$indicator == 'HYP001', ]
          
          # Finding the average rate of hypertension across practices
          nat_hyp_av <- round(mean(opt4_table_hyp$ratio) * 100, 1)
          
          # Finding the specified practice's rate of hypertension
          practice_hyp <- opt4_table_hyp[opt4_table_hyp$practiceid == 
                                           user_practiceid, ]
          practice_hyp <- practice_hyp$ratio
          practice_hyp <- round(practice_hyp * 100, 1)
          
          # Splitting the rates table to a create an obesity rates table
          opt4_table_ob <- opt4_table[opt4_table$indicator == 'OB001W', ]
          
          # Finding the average rate of obesity across practices
          nat_ob_av <- round(mean(opt4_table_ob$ratio) * 100, 1)
          
          # Finding the specified practice's rate of obesity
          practice_ob <- opt4_table_ob[opt4_table_ob$practiceid == 
                                         user_practiceid, ]
          practice_ob <- practice_ob$ratio
          practice_ob <- round(practice_ob * 100, 1)
          
          # Using the size code from above (option 3) to compare size categories 
          option_3 <- paste0("SELECT orgcode AS practiceid, SUM(numerator) AS 
                           quantity 
                           FROM qof_achievement
                           GROUP BY orgcode
                           ORDER BY quantity DESC")
          option_3_table <- dbGetQuery(con, option_3)
          option_3_table <- option_3_table[-c(1), ]
          mean_size <- mean(option_3_table[,2])
          
          # Joining the hypertension and obesity rates tables to the size table
          size_hyp_table <- merge(x = option_3_table, y = opt4_table_hyp, 
                                  by = "practiceid", all.y = TRUE)
          size_ob_table <- merge(x = option_3_table, y = opt4_table_ob, 
                                 by = "practiceid", all.y = TRUE)
          
          # Finding the average rates of hypertension in large & small practices
          
          large_hyp_table <- na.omit(size_hyp_table[size_hyp_table$quantity >= 
                                                      mean_size, ])
          large_hyp_av <- round(mean(large_hyp_table$ratio) * 100, 1)
          
          small_hyp_table <- na.omit(size_hyp_table[size_hyp_table$quantity < 
                                                      mean_size, ])
          small_hyp_av <- round(mean(small_hyp_table$ratio) * 100, 1)
          
          # Finding the average rates of obesity in large and small practices
          
          large_ob_table <- na.omit(size_ob_table[size_ob_table$quantity >= 
                                                    mean_size, ])
          large_ob_av <- round(mean(large_ob_table$ratio) * 100, 1)
          
          small_ob_table <- na.omit(size_ob_table[size_ob_table$quantity < 
                                                    mean_size, ])
          small_ob_av <- round(mean(small_ob_table$ratio) * 100, 1)
          
          # Storing the previously defined practice average, national average, 
          # small practice average, and large practice average of both 
          # hypertension and obesity in a table to easily compare values and
          # printing it for the user
          
          rates_table <- data.frame('practice_category' = c(user_practiceid, 
                                                            'nationwide', 
                                                            'small', 
                                                            'large'),
                                    'hypertension_rate' = c(practice_hyp, 
                                                            nat_hyp_av, 
                                                            small_hyp_av, 
                                                            large_hyp_av),
                                    'obesity_rate' = c(practice_ob, 
                                                       nat_ob_av, 
                                                       small_ob_av, 
                                                       large_ob_av))
          print(rates_table)
          
          # Creating a barplot from the above table to visually display the data 
          
          rates_graph <- barplot(t(rates_table[c('hypertension_rate', 
                                                 'obesity_rate')]), 
                                 beside = T,        
                                 names = rates_table$practice_category, 
                                 density = c(20, 20), 
                                 angle = c(0, 45), 
                                 col = c("red", "blue"),
                                 xlab = "Practice Category",
                                 ylab = "Percentage Rate",
                                 main = "Rates of Hypertension and Obesity",
                                 ylim = c(0, (round(max(practice_hyp, 
                                                        nat_hyp_av, 
                                                        small_hyp_av, 
                                                        large_hyp_av, 
                                                        practice_ob, 
                                                        nat_ob_av, 
                                                        small_ob_av, 
                                                        large_ob_av),
                                                    0) * 1.3)),
                                 legend.text=c('Hypertension','Obesity'))
          
          string1 <- "In the lower right pane, you should see a barplot"
          string2 <- "displaying this data visually."
          cat("\n", string1, string2, "\n\n", "Analysis complete", "\n\n")
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for directing the user back to the start menu
          
        } else if (user_option == "b"){
          cat("Option b selected")
          cat("\n\n")
          break
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for exiting the program
          
        } else if (user_option == "q"){
          cat("Thank you for using my program. Goodbye!")
          dbDisconnect(con)
          break
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for invalid/undefined user options
          
        } else {
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
        }
      }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Main menu option 2
      # Correlation between rate of metformin prescriptions and rate of 
      # hypertension and obesity
      
    } else if (user_option == 2){
      
      cat("Option 2 selected")
    
      string1 <- "Before running a correlational analysis, we must test each 
      variable for normality. Skewness and kurtosis values will numerically 
      indicate whether normality can be assumed (if they lie between -2 and +2, 
      and -7 and +7 respectively). A Q-Q plot will also appear in the lower 
      right pane for visualisation of the distribution (if normal, the data 
      points will lie along a diagonal straight line). If normality is assumed,
      a parametric test can be conducted, such as Pearson's correlation, 
      otherwise a non-parametric test must be conducted, such as Kendall's rank."
      cat("\n\n", string1)
      
      # Defining variables and extracting the relevant dataset from the database
      # (sum of metformin prescriptions, sum of all prescriptions, metformin
      # prescriptions divided by all prescriptions for metformin ratio, 
      # hypertension ratio and obesity ratio)
      
      bnfcode <- '0601022B0'
      df <- compile_df(bnfcode)
      drug_list_query <- "SELECT DISTINCT(bnfchemical) as bnfcode, 
                           chemicaldesc as drug 
                           FROM bnf
                           WHERE bnfsection = '601'"
      drug_list <- dbGetQuery(con, drug_list_query)
      name_of_drug <- drug_list[drug_list$bnfcode == '0601022B0',]
      name_of_drug <- name_of_drug$drug
      name_of_drug <- trimws(name_of_drug)
      
      # Testing normality of metformin prescription data
      
      cat("\n\n", "Normality assumption: rate of metformin prescribing")
      normality(df$drug_ratio)
      
      # Checkpoint for user to proceed to next variable when ready
      
      cat("\n\n", "Proceed to the next variable?
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Testing normality of hypertension data
      
      cat("\n", "Normality assumption: rate of hypertension")
      normality(df$hypertension_ratio)
      
      # Another user checkpoint
      
      cat("\n\n", "Proceed to the next variable?
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Testing normality of obesity data
      
      cat("\n", "Normality assumption: rate of obesity")
      normality(df$obesity_ratio)
      
      # Another user checkpoint
      
      cat("\n\n", "Proceed to the analysis? 
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Correlational analysis
      
      correlation(bnfcode, df, name_of_drug, "pearson")
      
      cat("\n\n", "Analysis complete", "\n\n")
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Main menu option 3
      # Correlation between rate of specified diabetic drug prescriptions and 
      # rate of hypertension and obesity
      
    } else if (user_option == 3){
      
      cat("Option 3 selected", "\n\n")
      
      # Creating a table of diabetic drug options for user to choose from
      drug_list_query <- "SELECT DISTINCT(bnfchemical) as bnfcode, 
                           chemicaldesc as drug 
                           FROM bnf
                           WHERE bnfsection = '601'"
      drug_list <- dbGetQuery(con, drug_list_query)
      view(drug_list)
      
      # Allowing the user to select a drug and checking its validity
      
      bnfcode <- readline("BNF code of the drug you wish to analyse:")
      
      if (any(drug_list$bnfcode == bnfcode)){
        cat("BNF code", bnfcode, "selected.")
      } else {
        cat("Invalid BNF code. Copy and paste from the 'drug_list' table.")
        bnfcode <- readline("BNF code of the drug you wish to analyse:")
        
        if (any(drug_list$bnfcode == bnfcode)){
          cat("BNF code", bnfcode, "selected.")
        } else {
          cat("Invalid BNF code. Please try again.")
          next
        }
      } 
      
      string1 <- "Before running a correlational analysis, we must test each 
      variable for normality. Skewness and kurtosis values will numerically 
      indicate whether normality can be assumed (if they lie between -2 and +2, 
      and -7 and +7 respectively). A Q-Q plot will also appear in the lower 
      right pane for visualisation of the distribution (if normal, the data 
      points will lie along a diagonal straight line). If normality is assumed,
      a parametric test can be conducted, such as Pearson's correlation, 
      otherwise a non-parametric test must be conducted, such as Kendall's rank."
      cat("\n\n", string1)
      
      name_of_drug <- drug_list[drug_list$bnfcode == bnfcode,]
      name_of_drug <- name_of_drug$drug
      name_of_drug <- trimws(name_of_drug)
      
      # Extracting the relevant data from the database and checking there is 
      # sufficient data for the specified drug
      
      df <- compile_df(bnfcode)
      
      if (nrow(df) == 0){
        cat("\n\n", "Insufficient data to run analysis for", name_of_drug)
        cat("\n", "Please try again with  different drug", "\n")
        next
      }
      
      # Testing the specified drug data for normality
      
      cat("\n\n", "Normality assumption: rate of", name_of_drug, "prescribing")
      normality(df$drug_ratio)
      
      # Checkpoint for user
      
      cat("\n\n", "Proceed to the next variable?
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Testing hypertension for normality
      
      cat("\n", "Normality assumption: rate of hypertension")
      normality(df$hypertension_ratio)
      
      # User checkpoint
      
      cat("\n\n", "Proceed to the next variable?
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Testing obesity for normality
      
      cat("\n", "Normality assumption: rate of obesity")
      normality(df$obesity_ratio)
      
      # User checkpoint
      
      cat("\n\n", "Proceed to the analysis? 
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Determining whether a parametric or non-parametric test should be used
      # and reporting the result of the test
      
      if (skewness(df$drug_ratio) >= -2 & skewness(df$drug_ratio) <= 2 &
          kurtosis(df$drug_ratio) >= -7 & kurtosis(df$drug_ratio) <= 7){
        correlation(bnfcode, df, name_of_drug, "pearson")
      } else {
        correlation(bnfcode, df, name_of_drug, "kendall")
      }
      
      cat("\n\n", "Analysis complete", "\n\n")

      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Main menu option 4
      # Health board-specific analyses
      
    } else if (user_option == 4){
      
      # Creating a table of Welsh health boards and their codes, and allowing 
      # the user to specify a health board for analyses
      
      string1 <- "Health board-specific analyses selected"
      string2 <- "Please consult the table in the top left pane to choose a 
      health board."
      cat(string1, string2, sep="\n\n")
      
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
      
      # Checking that the user entered a valid health board code      
      
      if (any(hb$code == user_hb)){
        cat("Health board", user_hb, "selected.")
      } else {
        cat("Invalid code. Check format matches 7A0 and try again.")
        user_hb <- readline("Code of the health board you wish to analyse:")
       
         if (any(hb$code == user_hb)){
          cat("Health board", user_hb, "selected.")
        } else {
          cat("Invalid code. Check format matches 7A0 and try again.")
          next
        }
      }
      
      user_hb_name <- hb[hb$code == user_hb,]
      user_hb_name <- user_hb_name$name
      
      # Sub menu of analyses which appears when a health board is specified
      
      while (user_option != 'q' | user_option != 'b'){
        string1 <- "Sub Menu:
      1. Top ten drugs by total cost
      2. Top five drug categories by total cost
      3. Ranking of health boards by number of practices
      4. Monthly prescribing rate comparison
      b. Back to start menu
      q. Quit"
        cat("\n\n", string1)
        user_option <- readline("Select an analysis to run:")
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Sub-menu option 1
        # Top 10 drugs by cost at specified health board
        
        if (user_option == 1){
          
          string1 <- "Option 1 selected"
          string2 <- "Creating a table showing the top ten drugs by total amount"
          cat(string1, "\n\n", string2, "spent at", "\n", user_hb_name, "\n")
          
          # Selecting the drug chemical (as in option 1 sub-option 1) and 
          # the sum of the drug prescriptions costs, ordering by cost descending
          # and limiting to 10 to provide the top 10 drugs by amount spent.
          
          option_1 <- paste0("SELECT b.chemical AS drug, SUM(actcost) AS cost
                             FROM gp_data_up_to_2015 a
                             LEFT JOIN chemsubstance b 
                             ON b.bnfchemical = LEFT(a.bnfcode, 9)
                             WHERE hb ='", user_hb,
                           "'GROUP BY b.chemical
                             ORDER BY cost DESC
                             LIMIT 10")
          top_10_cost <- dbGetQuery(con, option_1)
          print(top_10_cost)
          cat("\n", "Analysis complete")
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Top 5 drug categories by total amount spent at specified health board
          
        } else if (user_option == 2){
          
          string1 <- "Option 2 selected"
          string2 <- "Generating a table showing the top five prescribed drug"
          string3 <- "categories by total amount spent at"
          cat(string1, "\n\n", string2, string3, user_hb_name, "\n")
          cat("Please be patient as this may take up to 30 seconds", "\n\n")
          
          # Selecting drug category and sum of cost grouped by drug category,
          # ordered by cost descending and limited to 5 to provide top 5
          # drug categories by amount spent
          
          option_2 <- paste0("SELECT sectiondesc AS drug_category, cost
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
          option_2_table <- dbGetQuery(con, option_2)
          print(option_2_table)
          cat("\n", "Analysis complete") 
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Ranking the health boards by number of practices
          
        } else if (user_option == 3){
          
          string1 <- "Option 3 selected"
          string2 <- "Generating a table ranking the health boards by number of"
          string3 <- "practices"
          cat(string1, "\n\n", string2, string3, "\n\n")
          
          # Selecting the health board codes and the number of practices with 
          # each code grouped by health board, and ordering by quantity 
          # descending
          
          option_3 <- "SELECT hb as code, COUNT(practiceid) AS quantity
                       FROM (
                             SELECT DISTINCT(a.practiceid), b.hb
                             FROM address a
                             INNER JOIN gp_data_up_to_2015 b
                             ON a.practiceid = b.practiceid
                       ) x
                       GROUP BY hb
                       ORDER BY quantity DESC"
          option_3_table <- dbGetQuery(con, option_3)
          option_3_table <- cbind(name = hb$name, option_3_table)
          print(option_3_table)
          
          # Creating a treemap to visualise the different health board sizes
          
          hb_treemap <- treemap(option_3_table,
                                index="name",
                                vSize="quantity",
                                type="index",
                                fontsize.labels = 10,
                                fontface.labels = 3)
          
          string1 <- "See lower right pane for boxplot displaying the health"
          string2 <- "board sizes."
          string3 <- "Analysis complete"
          cat("\n", string1, string2, "\n", string3)
          
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Monthly prescription data comparison between health boards
          
        } else if (user_option == 4){
          
          # Allowing the user to select a time period and checking it is valid
          
          string1 <- "Option 4 selected"
          string2 <- "Monthly prescription data is available from April 2013"
          string3 <- "(201304) to December 2015 (201512)."
          cat(string1, "\n\n", string2, string3)
          
          user_period <- readline("Enter a period (YYYYMM):")
          
          check_period <- "SELECT DISTINCT(period)
                          FROM gp_data_up_to_2015"
          check_period <- dbGetQuery(con, check_period)
          
          if (any(check_period$period == user_period)){
            cat("Period", user_period, "selected.", "\n\n")
          } else {
            cat("Invalid period. Check format matches YYYYMM and try again.")
            next
          }
          
          # Selecting the sum of prescription items for each health board and 
          # the health board codes from the specified period, ordering by 
          # total items descending
          
          option_4 <- paste0("SELECT DISTINCT(hb) as code, 
                                     SUM(quantity) AS total_items
                              FROM gp_data_up_to_2015
                              WHERE period ='", user_period,
                            "'GROUP BY hb
                              ORDER BY total_items DESC")
          option_4_table <- dbGetQuery(con, option_4)
          option_4_table <- cbind(name = hb$name, option_4_table)
          print(option_4_table)
          
          string1 <- "See the lower left pane for a tree map visually"
          string2 <- "representing this data"
          cat("\n\n", string1, string2)
          
          # Creating a tree map for visual representation of the above data
          
          hb_treemap <- treemap(option_4_table,
                                index="name",
                                vSize="total_items",
                                type="index",
                                fontsize.labels = 10,
                                fontface.labels = 3)
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for directing the user back to the start menu
          
        } else if (user_option == "b"){
          cat("Option b selected")
          cat("\n\n")
          break
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for exiting the program
          
        } else if (user_option == "q"){
          cat("Thank you for using my program. Goodbye!")
          dbDisconnect(con)
          break
        
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for invalid/undefined user options
          
        } else {
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
        }
      }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Main menu option 5
      # Investigating whether there is a significant difference between Cardiff 
      # and Powys health boards' hypertension prevalence using statistical tests
      
    } else if (user_option == 5){
      
      cat("Option 5 selected")
      string1 <- "Generating a statistical analysis of the difference between"
      string2 <- "Cardiff and Powys Health Boards' hypertension prevalence."
      string3 <- "For user reference, Cardiff's health board code is 7A4, whilst"
      string4 <- "Powys' is 7A7."
      cat("\n\n", string1, string2, "\n", string3, string4, "\n\n")
      
      # Extract data from database (hypertension ratios for all practices within
      # the two health boards)
      
      df <- paste0("SELECT b.hb, a.ratio
                    FROM qof_achievement  a
                    INNER JOIN gp_data_up_to_2015 b
                    ON a.orgcode = b.practiceid
                    WHERE a.indicator = 'HYP001' AND b.hb = '7A4'
                       OR a.indicator = 'HYP001' AND b.hb = '7A7'
                    GROUP BY b.practiceid, b.hb, a.indicator, a.ratio")
      df <- dbGetQuery(con, df)
      
      # Testing normality of first health board's hypertension ratio
     
      cat("\n", "Normality assumption: rate of hypertension in area 7A4")
      table_7A4 <- df[df$hb == '7A4',]
      normality(table_7A4$ratio)
      
      # User checkpoint
      
      cat("\n\n", "Proceed to the next variable?
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Testing normality of second health board's hypertension ratio
      
      cat("\n\n", "Normality assumption: rate of hypertension in area 7A7")
      table_7A7 <- df[df$hb == '7A7',]
      normality(table_7A7$ratio)
      
      # User checkpoint
      
      cat("\n\n", "Proceed to the next assumption and analysis?
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Testing both variables for equality of variances
      
      homoscedasticity <- bartlett.test(ratio ~ hb, data = df)
      cat("\n\n")
      print(homoscedasticity)

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
        
      # Reporting the results and interpretation
        
      mean1 <- round(stattest$estimate[[1]], 2)
      mean2 <- round(stattest$estimate[[2]], 2)
        
      string1 <- "The mean hypertension ratio in group 7A4 is"
      string2 <- "whereas the mean in group 7A7 is"
      cat("\n\n", string1, mean1, string2, mean2)
      cat("\n", "This data can be observed in the lower right pane's boxplot")
        
      pvalue <- stattest$p.value
        
      cat("\n\n", "Analysis")
        
      if (pvalue <= 0.05){
        string1 <- "A Welch two-samples t-test showed that the difference"
        string2 <- "between Cardiff and Powys health boards' prevalence of"
        string3 <- "hypertensions was statistically significant."
        cat("\n\n", string1, string2, string3)
        } else if (pvalue > 0.05){
          string1 <- "A Welch two-samples t-test showed that the difference"
          string2 <- "between Cardiff and Powys health boards' prevalence of"
          string3 <- "hypertensions was statistically non-significant."
          cat("\n\n", string1, string2)
        }
        
      pvalue <- pformat(pvalue)
      degf <- round(stattest$parameter, 0)
      stat <- round(stattest$statistic, 2)
      stat <- str_replace(stat, "^0", "")
      stat <- paste("=", stat)
      string1 <- paste(paste0("t(", degf, ")"), stat, ", p", pvalue)
      cat("\n", string1, "\n\n")
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Main menu option 6
      # Comparison between two health boards' prevalence of a health condition  
      # as specified by the user
      
      
    } else if (user_option == 6){
     
      cat("Option 6 selected", "\n\n")
      string1 <- "In this analysis, you may choose two health boards"
      string2 <- "to compare the prevalence of a user-specified condition."
      
      # Allowing the user to enter 2 health boards to compare and checking
      # the entries' validity
      
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
      user_hb1_name <- hb[hb$code == user_hb1,]
      user_hb1_name <- user_hb1_name$name
      
      user_hb2 <- readline("Code of the second health board you wish to analyse:")
      user_hb2_name <- hb[hb$code == user_hb2,]
      user_hb2_name <- user_hb2_name$name
      
      if (any(hb$code == user_hb1) & any(hb$code == user_hb2)){
        cat("Health boards", user_hb1, "and", user_hb2, "selected.", "\n")
        cat(user_hb1, "=", user_hb1_name, "and", user_hb2, "=", user_hb2_name)
      } else {
        print("Invalid health board code. Try again using the provided codes.")
        next
      }
      
      # Creating a table for the user to choose a health condition from for
      # analysis and checking the entered code's validity
      
      diag <- data.frame(condition = c("atrial fibrillation", "asthma", 
                                       "cancer", "coronary heart disease", 
                                       "chronic obstructive pulmonary disease",
                                       "dementia", "diabetes", "epilepsy",
                                       "heart failure", "hypertension",
                                       "learning disabilities", "obesity",
                                       "osteoporosis", 
                                       "peripheral arterial disease",
                                       "rheumatoid arthritis", 
                                       "schizophrenia, bipolar & other psychoses", 
                                       "stroke & transient ischaemic attacks"),
                         code = c("AF001", "AST001", "CAN001", "CHD001",
                                  "COPD001", "DEM001", "DM001", "EP001", 
                                  "HF001", "HYP001", "LD001", "OB001W", 
                                  "OST001", "PAD001", "RA001", "MH001", 
                                  "STIA001"))
      view(diag)
      
      user_diag <- readline("Code of the condition you wish to analyse:")
      
      if (any(diag$code == user_diag)){
        cat("Diagnosis code", user_diag, "selected.")
        cat("\n", "Please be patient while the data is extracted.")
      } else {
        cat("Invalid diagnosis code. Please copy and past from 'diag' table.")
        user_diag <- readline("Code of the condition you wish to analyse:")
       
         if (any(diag$code == user_diag)){
          cat("Diagnosis code", user_diag, "selected.")
        } else {
          cat("Invalid diagnosis code. Please copy and paste from 'diag' table.")
          next        
        }
      }
      
      user_diag_name <- diag[diag$code == user_diag,]
      user_diag_name <- user_diag_name$condition
      user_diag_name <- trimws(user_diag_name)
      
      # Extract data from database (health condition ratio for both specified
      # health boards)
      
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
      
      # Testing normality of first health board's health condition ratio
      
      string1 <- "Normality assumption: rate of"
      string2 <- "in area"
      cat("\n\n", string1, user_diag_name, string2, user_hb1)
      table_hb1 <- df[df$hb == user_hb1,]
      normality(table_hb1$ratio)
      
      # User checkpoint
      
      cat("\n\n", "Proceed to the next variable?
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Testing normality of second health board's health condition ratio
      
      string1 <- "Normality assumption: rate of"
      string2 <- "in area"
      cat("\n\n", string1, user_diag_name, string2, user_hb2)
      table_hb2 <- df[df$hb == user_hb2,]
      normality(table_hb2$ratio)
      
      # Creating test_type variable based on results of skewness and kurtosis
      # to inform whether a parametric or non-parametric test should be used
      
      if (skewness(table_hb1$ratio) >= -2 & skewness(table_hb1$ratio) <= 2 &
          kurtosis(table_hb1$ratio) >= -7 & kurtosis(table_hb1$ratio) <= 7 & 
          skewness(table_hb2$ratio) >= -2 & skewness(table_hb2$ratio) <= 2 &
          kurtosis(table_hb2$ratio) >= -7 & kurtosis(table_hb2$ratio) <= 7){
        test_type <- "parametric"
      } else {
        test_type <- "nonparametric"
      }
      
      # User checkpoint
      
      cat("\n\n", "Proceed to the next assumption and analysis?
        y. Yes
        m. Main menu
        q. Quit")
      
      user_option <- readline("Select an option:")
      
      if (user_option == "m"){
        next
      } else if (user_option == "q"){
        return()
      } else if (user_option == "y"){
        cat("\n", "Proceeding")
      } else {
        while (user_option != "y" & user_option != "m" & user_option != "q"){
          string1 <- "Option"
          string2 <- "is not defined. Please enter a listed number or letter."
          cat(string1, user_option, string2)
          user_option <- readline("Select an option:")
          if (user_option == "m"){
            next
          } else if (user_option == "q"){
            cat("Thank you for using my program. Goodbye!")
            dbDisconnect(con)
            return()
          } else if (user_option == "y"){
            cat("\n", "Proceeding")
          }
        }
      }
      
      if (user_option == "m"){
        next
      }
      
      # Testing both variables for equality of variances using different methods
      # depending on whether the normality assumption was satisfied or not
      # (normality assumed = parametric Bartlett's test, whereas
      # normality not assumed = non-parametric Fligner Killeen test)
      
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
      
      # Conducting a parametric or non-parametric test depending on the result
      # of the normality test (indicated by test_type) and the significance of
      # the homoscedasticity test
      
      if (p_var > 0.05 & test_type == "parametric"){
        
        # Code for parametric two sample t-test if both assumptions satisfied
        
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
        
        mean1 <- round(stattest$estimate[[1]], 2)
        mean2 <- round(stattest$estimate[[2]], 2)
        
        cat("\n\n", "Analysis")
        string1 <- paste("The mean", user_diag_name, "ratio in group", user_hb1)
        string2 <- paste("whereas the mean in group", user_hb2, "is")
        cat("\n\n", string1, "is", mean1, string2, mean2)
        cat("\n", "This data can be observed in the lower right pane's boxplot")
        
        pvalue <- stattest$p.value
        
        if (pvalue <= 0.05){
          string1 <- "A Welch two-samples t-test showed that the difference"
          string2 <- "was statistically significant."
          cat("\n\n", string1, string2)
        } else if (pvalue > 0.05){
          string1 <- "A Welch two-samples t-test showed that the difference"
          string2 <- "was statistically non-significant."
          cat("\n\n", string1, string2)
        }
        
        pvalue <- pformat(pvalue)
        degf <- round(stattest$parameter, 0)
        stat <- round(stattest$statistic, 2)
        stat <- str_replace(stat, "^0", "")
        stat <- paste("=", stat)
        string1 <- paste(paste0("t(", degf, ")"), stat, ", p", pvalue)
        cat("\n", string1, "\n\n")
        
      } else if (p_var <= 0.05 | test_type == "nonparametric"){
        
        # Code for non-parametric Mann Whitney U test if either assumption is 
        # violated
        
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
        
        cat("\n\n", "Analysis")
        pvalue <- stattest$p.value
        
        if (pvalue <= 0.05){
          string1 <- "A Mann Whitney U test showed that the difference"
          string2 <- "was statistically significant."
          cat("\n\n", string1, string2)
          cat("\n", "The data can be observed in the lower right pane's boxplot")
          
        } else if (pvalue > 0.05){
          string1 <- "A Mann Whitney U test showed that the difference"
          string2 <- "was statistically non-significant."
          cat("\n\n", string1, string2)
          cat("\n", "The data can be observed in the lower right pane's boxplot")
        }
        
        pvalue <- pformat(pvalue)
        stat <- round(stattest$statistic, 2)
        stat <- str_replace(stat, "^0", "")
        stat <- paste("=", stat)
        string1 <- paste("U", stat, ", p", pvalue)
        cat("\n\n", string1, "\n\n")
        
      } 
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Option for the user to quit the program
      
    } else if (user_option == "q"){
      string1 <- "Thank you for using my program. Goodbye!"
      cat(string1)
      dbDisconnect(con)
      break
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Result for invalid user option
      
    } else {
      string1 <- "Option"
      string2 <- "is not defined. Please enter a listed number or letter."
      cat(string1, user_option, string2)
      next
    }
  }
}

myprogram()
