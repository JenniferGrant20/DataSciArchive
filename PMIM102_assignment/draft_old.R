#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Student No. 2005070
# PMIM102J Assignment


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Libraries used, database link, and defining variables outside of the loop

library(tidyverse)
library(RPostgreSQL) 
library(ggpubr)
library(moments)
library(treemap)
library(car)

source("functions.R")

drv <- dbDriver('PostgreSQL') 
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',                  
                 port=5432, user='postgres',                  
                 password="") #.rs.askForPassword('Password:')) 

user_practiceid <- "no_input"
user_option <- "no_input"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating function and while loop

myprogram <- function(){
  string1 <- "Welcome to Jennifer’s program"
  string2 <- "This program will allow you to run analyses on Welsh GP practice 
  data from 2015."
  cat(string1, string2, sep="\n\n")
  while (user_option != 'q'){
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
    # Creating main menu for user to specify a GP practice
    
    string1 <- "Main Menu:
    1. GP practice-specific analyses
    2. Correlation between metformin prescriptions and hypertension/obesity
    3. Correlation between a chosen diabetic drug and hypertension/obesity
    4. Health board-specific analyses
    5. ??? t-test
    6. ??? t-test
    q. Quit"
    cat("\n", string1)
    user_option <- readline("Select an option:")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Allowing the user to specify a practice ID for practice-specific analyses
    
    if (user_option == "1"){
      string1 <- "Start selected"
      string2 <- "Please specify a GP practice to run analyses on."
      cat(string1, string2, sep="\n\n")
      user_practiceid <- readline("Practice ID:")
      
      # Checking that the user entered a valid practice ID       
      
      id_options <- "select * from address"
      id_options_table <- dbGetQuery(con, id_options)
      
      if (any(id_options_table$practiceid == user_practiceid)){
        cat("Practice", user_practiceid, "selected.")
      } else {
        print("Invalid practice ID. Check format matches W00000 and try again.")
      }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Main menu of analyses which appears when a practice ID is specified
      
      while (user_option <= 4){ 
        string1 <- "Sub Menu:
      1. Ten most commonly prescribed drugs
      2. Five most commonly used prescribed drug categories
      3. Size of GP practice
      4. Obesity and hypertension rates
      b. Back to start menu
      q. Quit"
        cat("\n\n", string1)
        user_option <- readline("Select an analysis to run:")
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Code for option 1: top ten most commonly prescribed drugs
        
        if (user_option == "1"){
          string1 <- "Option 1 selected"
          string2 <- "Generating a table showing the top ten most commonly 
        prescribed drugs and their prescription quantities at the specified 
        practice"
          cat(string1, string2, sep="\n\n")
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
          # Code for option 2: top five most commonly prescribed drug categories
          
        } else if (user_option == "2"){
          string1 <- "Option 2 selected"
          string2 <- "Generating a table showing the top five most commonly 
        prescribed drug categories and their prescription quantities at the
        specified practice."
          cat(string1, string2, sep="\n\n")
          option_2 <- paste0("SELECT sectiondesc AS drug_category, total
                           FROM (
                                 SELECT o.practiceid, p.bnfchemical, o.bnfcode, 
                                 p.sectiondesc, o.bnfname, o.quantity, 
                                 SUM(o.quantity) OVER (PARTITION BY 
                                 p.sectiondesc) AS total
                                 FROM gp_data_up_to_2015 o 
                                 LEFT JOIN
                                 bnf p
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
          # Code for option 3: returning the size category of specified practice
          
        } else if (user_option == "3"){
          cat("Option 3 selected")
          option_3 <- paste0("SELECT orgcode AS practiceid, SUM(numerator) AS 
                           quantity 
                           FROM qof_achievement
                           GROUP BY orgcode
                           ORDER BY quantity DESC")
          option_3_table <- dbGetQuery(con, option_3)
          option_3_table <- option_3_table[-c(1), ]
          
          # Calculating the mean size of GP practice to use as cut-off point
          # between small and large categories
          
          mean_size <- mean(option_3_table[,2])
          
          # Defining the specified practice's size
          
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
          # Code for option 4: comparing the rates of hypertension and obesity
          
        } else if (user_option == "4"){
          string1 <- "Option 4 selected"
          string2 <- "Generating a table showing the rates of hypertension and 
        obesity at the specified GP practice, across all GP practices in Wales,
        and at practices categorised as small or large."
          cat(string1, string2, sep = "\n\n")
          cat("\n\n")
          
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
          
          string1 <- "In the lower right pane, you should see a barplot 
        displaying this data visually."
          cat("\n", string1, "\n\n", "Analysis complete")
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Code for directing the user back to the start menu
          
        } else if (user_option == "b"){
          cat("Option b selected")
          cat("\n\n")
          
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
      # Correlation between rate of metformin prescriptions and rate of 
      # hypertension and obesity
      
    } else if (user_option == 2){
      
      string1 <- "Before running a statistical test, we must test each 
      variable for normality. If the data is normally distributed, we can use
      a parametric test, such as the Pearson correlation coefficient. If it is 
      not, we must use a non-parametric test, such as the Kendall rank 
      correlation coefficient."
      string2 <- "To test for normality, it is better to use both visual
      inspection and descriptive statistics, therefore a Q-Q plot will be 
      displayed for each variable, as well as skewness and kurtosis values."
      string3 <- "Skewness values should lie between -2 and +2 for normality to 
      be assumed, whilst kurtosis values should lie between -7 and +7. 
      Observe the Q-Q plots which will be displayed in the lower right pane to 
      provide a visualisation of the distribution. If normally distributed, the 
      data points will follow a diagonal straight line."
      cat(string1, string2, string3, sep="\n\n")
      
      # Defining variables and extracting the relevant dataset from the database
      
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
      
      cat("\n", "Normality assumption: rate of metformin prescribing")
      normality(df$drug_ratio, "drug")
      
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
      normality(df$hypertension_ratio, "hypertension")
      
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
      normality(df$obesity_ratio, "obesity")
      
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
      
      cat("Analysis complete")
      user_option <- "m"
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Correlation between rate of specified diabetic drug prescriptions and rate
      # of hypertension and obesity
      
    } else if (user_option == 3){
      
      drug_list_query <- "SELECT DISTINCT(bnfchemical) as bnfcode, 
                           chemicaldesc as drug 
                           FROM bnf
                           WHERE bnfsection = '601'"
      drug_list <- dbGetQuery(con, drug_list_query)
      view(drug_list)
      
      bnfcode <- readline("BNF code of the drug you wish to analyse:")
      
      string1 <- "Before running a statistical test, we must test each 
      variable for normality. If the data is normally distributed, we can use
      a parametric test, such as the Pearson correlation coefficient. If it is 
      not, we must use a non-parametric test, such as the Kendall rank 
      correlation coefficient."
      string2 <- "To test for normality, it is better to use both visual
      inspection and descriptive statistics, therefore a Q-Q plot will be 
      displayed for each variable, as well as skewness and kurtosis values."
      string3 <- "Skewness values should lie between -2 and +2 for normality to 
      be assumed, whilst kurtosis values should lie between -7 and +7. 
      Observe the Q-Q plots which will be displayed in the lower right pane to 
      provide a visualisation of the distribution. If normally distributed, the 
      data points will follow a diagonal straight line."
      cat(string1, string2, string3, sep="\n\n")
      
      name_of_drug <- drug_list[drug_list$bnfcode == bnfcode,]
      name_of_drug <- name_of_drug$drug
      name_of_drug <- trimws(name_of_drug)
      
      df <- compile_df(bnfcode)
      
      # Testing the specified drug for normality
      
      cat("\n", "Normality assumption: rate of", name_of_drug, "prescribing")
      normality(df$drug_ratio, "drug")
      
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
      normality(df$hypertension_ratio, "hypertension")
      
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
      normality(df$obesity_ratio, "obesity")
      
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
        pearson(bnfcode, df, name_of_drug)
      } else {
        kendall(bnfcode, df, name_of_drug)
      }
      
      cat("\n\n", "Analysis complete")
      user_option <- "m"
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Health board-specific analyses menu
      
    } else if (user_option == 4){
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Allowing the user to specify a health board for analyses
      
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
      user_hb_name <- hb[hb$code == user_hb,]
      user_hb_name <- user_hb_name$name
      
      # Checking that the user entered a valid health board code      
      
      if (any(hb$code == user_hb)){
        cat("Health board", user_hb, "selected.")
      } else {
        print("Invalid code. Check format matches 7A0 and try again.")
      }
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Main menu of analyses which appears when a health board is specified
      
      while (user_option <= 4){
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
        # Top 10 drugs by cost at specified health board
        
        if (user_option == 1){
          
          string1 <- "Option 1 selected"
          string2 <- "Generating a table showing the top ten drugs by total cost"
          cat(string1, "\n\n", string2, "spent at", user_hb_name)
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
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          # 
          
        } else if (user_option == 2){
          
          string1 <- "Option 2 selected"
          string2 <- "Generating a table showing the top five prescribed drug"
          string3 <- "categories by total cost spent at"
          cat(string1, "\n\n", string2, string3, user_hb_name)
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
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          #   
          
        } else if (user_option == 3){
          
          string1 <- "Option 3 selected"
          string2 <- "Generating a table ranking the health boards by number of"
          string3 <- "practices"
          cat(string1, "\n\n", string2, string3)
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
          cat("\n", "Analysis complete")
          
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          #   
          
        } else if (user_option == 4){
          
          string1 <- "Option 4 selected"
          string2 <- "Monthly prescription data is available from April 2013"
          string3 <- "(201304) to December 2015 (201512)."
          cat(string1, "\n\n", string2, string3)
          
          user_period <- readline("Enter a period (YYYYMM):")
          
          check_period <- "SELECT DISTINCT(period)
                          FROM gp_data_up_to_2015"
          check_period <- dbGetQuery(con, check_period)
          
          if (any(check_period$period == user_period)){
            cat("Period", user_period, "selected.")
          } else {
            print("Invalid period. Check format matches YYYYMM and try again.")
          }
          
          option_4 <- paste0("SELECT DISTINCT(hb) as code, 
                                      SUM(quantity) AS total_items
                              FROM gp_data_up_to_2015
                              WHERE period ='", user_period,
                             "'GROUP BY hb
                              ORDER BY total_items DESC")
          option_4_table <- dbGetQuery(con, option_4)
          option_4_table <- cbind(name = hb$name, option_4_table)
          print(option_4_table)
          
          hb_treemap <- treemap(option_4_table,
                                index="name",
                                vSize="total_items",
                                type="index",
                                fontsize.labels = 10,
                                fontface.labels = 3)
          print(hb_treemap)
        }
    }
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Investigating whether there is a significant difference between Cardiff 
      # and Powys health boards' hypertension prevalence using statistical tests
      
      } else if (user_option == 5){
      
      cat("Option 5 selected")
      
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
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # t-test ???
      
      
    }  else if (user_option == 6){
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Option for the user to quit the program
      
    } else if (user_option == "q"){
      string1 <- "Thank you for using my program. Goodbye!"
      cat(string1)
      dbDisconnect(con)
      break
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  