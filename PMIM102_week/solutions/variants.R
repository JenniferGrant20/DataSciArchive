# ------------------------------------------------------------------------------
# variants.R
# ------------------------------------------------------------------------------
# Covariants project.
# Add variants data.
# ------------------------------------------------------------------------------
# Pete Arnold
# 5 December 2021
# ------------------------------------------------------------------------------

# We can assume that there will be files like:
# alpha <- read_csv('data/countrySubmissionCountAlpha.csv')
# gamma <- read_csv('data/countrySubmissionCountGamma.csv')
# delta <- read_csv('data/countrySubmissionCountDelta.csv')
# mu <- read_csv('data/countrySubmissionCountMu.csv')
# omicron <- read_csv('data/countrySubmissionCountOmicron.csv')

# To create a function to do this, you will probably find it easier to work
# through the process you need to go through for one file and then consider
# how you can make that process work for other files but using variables where
# you have provided names, or symbols etc.

# The process we need to go through is:
# (1) get the data into data frames and tweak appropriately.
# alpha <- alpha %>% select(Country, Count_A=`Total #Alpha 202012/01 GRY (B.1.1.7+Q.*)`) %>%
#     mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
# gamma <- gamma %>% select(Country, Count_G=`Total #Gamma GR/501Y.V3 (P.1+P.1.*)`) %>%
#     mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
# delta <- delta %>% select(Country, Count_D=`Total #Delta GK (B.1.617.2+AY.*)`) %>%
#     mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
# mu <- mu %>% select(Country, Count_M=`Total #Mu GH (B.1.621+B.1.621.1)`) %>%
#     mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
# omicron <- omicron %>% select(Country, Count_O=`Total #GR/484A (B.1.1.529)`) %>%
#     mutate(Country=ifelse(Country=='USA', 'United States of America', Country))
# # Join these data with the base world data.
# covid_variants <- covid_world %>%
#     left_join(alpha, by='Country') %>%
#     left_join(gamma, by='Country') %>%
#     left_join(delta, by='Country') %>%
#     left_join(mu, by='Country') %>%
#     left_join(omicron, by='Country')

# # We can write the code in a different way which is more amenable to putting it
# # into a function. First we define all the fixed values as variables so that we
# # can change them programmatically.
# covid_variants <- covid_world
# variant_file <- 'data/countrySubmissionCountMu.csv'
# variant_name <- 'Count_M'
# variants_df <- covid_variants
# # Then we rewrite the code with those variables.
# variant <- read_csv(variant_file) %>%
#     select(Country, 2) %>%
#     mutate(Country=ifelse(Country=='USA', 'United States of America', Country))     
# names(variant)[2] <- variant_name
# covid_variants <- covid_variants %>%
#     left_join(variant, by='Country')
# # Then we can write a function to do this with those variables as parameters.

# Add a variant data to the world + Covid-19 data frame.
# Parameters:
# @param    df              The world + Covid-19 data frame.
# @param    variant_name    The name to be used for the added variant.
# @param    file_name       The file name containing the variant data.
# Return value:
# @return   df              The data-frame with map_data + covid + variant data.
add_variant <- function(df, variant_name, file_name){
    # Write some code to:
    # 1. Read the file into a data frame.
    # 2. Select appropriate columns.
    # 3. Clean the data (if necessary).
    # 4. Name the variant data column as specified.
    # 5. Join the variant data to the existing data frame.
    # 6. Return the finished data frame.
    variant <- read_csv(file_name) %>%
        select(Country, 2) %>%
        mutate(Country=ifelse(Country=='USA', 'United States of America', Country))     
    names(variant)[2] <- variant_name
    df <- df %>%
        left_join(variant, by='Country')
    return(df)
}

# Which results in this code.
# covid_variants <- covid_world
# covid_variants <- add_variant(covid_variants, 'Count_M', 'data/countrySubmissionCountMu.csv')
# covid_variants <- add_variant(covid_variants, 'Count_A', 'data/countrySubmissionCountAlpha.csv')
# covid_variants <- add_variant(covid_variants, 'Count_G', 'data/countrySubmissionCountGamma.csv')
# covid_variants <- add_variant(covid_variants, 'Count_D', 'data/countrySubmissionCountDelta.csv')
# covid_variants <- add_variant(covid_variants, 'Count_M', 'data/countrySubmissionCountOmicron.csv')

# But this is still a bit repetitive, so we could
# go one step further.
# And what we end up with.
# variants <- c(
#     'Count_A' = 'data/countrySubmissionCountAlpha.csv',
#     'Count_G' = 'data/countrySubmissionCountGamma.csv',
#     'Count_D' = 'data/countrySubmissionCountDelta.csv',
#     'Count_M' = 'data/countrySubmissionCountMu.csv',
#     'Count_O' = 'data/countrySubmissionCountOmicron.csv')
# covid_variants <- add_variants(covid_world, variants)# A function to call all those functions.
# Add a list of variants' data to the world + Covid-19 data frame.
# Parameters:
# @param    df          The world + Covid-19 data frame.
# @param    variants    A vector of file names in which the each entry is
#                       labeled with the required variant name e.g.
#                       c('name'='file-name')
# Return value:
# @return   df          The data-frame with map_data + covid + variant data.
add_variants <- function(df, variants){
    # Write some code to:
    # 1. Work through the variant vector.
    # 2. Add each variant to the data-frame.
    # 3. Return the finished data frame.
    for (i in 1:length(variants)){
        df <- add_variant(df, names(variants)[i], variants[i])
    }
    return(df)
}

