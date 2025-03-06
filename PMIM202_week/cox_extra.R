# ------------------------------------------------------------------------------
# Exploration of the Framingham dataset for regression and machine learning.
# ------------------------------------------------------------------------------
# Looking into whether this dataset can be used for teaching regression and
# machine learning.
# ------------------------------------------------------------------------------
# 1.12.2021 - 8.12.2023 Pete Arnold
# ------------------------------------------------------------------------------
# Available Data
# The data now include examination data from the first 32 clinical exams,
# selected ancillary data, and event follow-up through 2018.
#
# Objectives
# The objectives of the Framingham Study are to study the incidence and
# prevalence of cardiovascular disease (CVD) and its risk factors, trends in CVD
# incidence and its risk factors over time, and familial patterns of CVD and
# risk factors. Other important objectives include the estimation of incidence
# rates of disease and description of the natural history of cardiovascular
# disease, including the sequence of clinical signs and systems that precede the
# clinically recognizable syndrome and the consequences and course of clinically
# manifest disease.
#
# Background
# The Framingham Study began in 1948 under the U.S. Public Health Service and
# was transferred under the direct operations of the new National Heart
# Institute, NIH, in 1949. Participants were sampled from Framingham,
# Massachusetts, including both men and women. This was the first prospective
# study of cardiovascular disease and identified the concept of risk factors and
# their joint effects. The study has continued to examine participants every two
# years and is currently supported by a contract to Boston University from the
# NHLBI, and from many grants for specialized studies.
#
# Subjects
# At entry to the study in 1948-1952, the study recruited 5,209 men and women,
# ages 28-62 years, living in Framingham, MA. As of February 28, 1999, there are
# 993 surviving participants.
#
# Design
# The Framingham Study is a longitudinal investigation of constitutional and
# environmental factors influencing the development of CVD in men and women.
# Examination of participants has taken place every two years and the cohort has
# been followed for morbidity and mortality over that time period.
#
# The cardiovascular disease conditions under investigation include coronary
# heart disease (angina pectoris, myocardial infarction, coronary insufficiency
# and sudden and non-sudden death), stroke, hypertension, peripheral arterial
# disease and congestive heart failure.
# ------------------------------------------------------------------------------
library(tidyverse)      # For many things.
library(survival)       # The survival object.
library(survminer)      # Survival plots.

# ------------------------------------------------------------------------------
# 1. Load, tidy and explore the data
# ------------------------------------------------------------------------------
df <- read_csv('data/frmgham2.csv')

# Look at the variables.
sample_n(df, 10)
glimpse(df)
df %>%
    pivot_longer(cols=everything()) %>%
    ggplot(aes(value)) +
        facet_wrap(~name, scales="free") +
        geom_histogram()

# ------------------------------------------------------------------------------
# 2. Cox regression
# ------------------------------------------------------------------------------
source('cox_functions.R')

cox_df <- df %>%
    mutate(EDUC=as.factor(educ)) %>%
    # Need to remove as there are many NAs here.
    select(-HDLC, -LDLC) %>%
    # Remove the not really relevant data.
    select(-TIMEAP, -TIMEMI, -TIMEMIFC, -TIMECHD, -TIMESTRK, -TIMECVD) %>%
    # Remove non-significant variables.
    select(-TOTCHOL, -DIABP, -CURSMOKE)

cox_hypertension_to_death_all(cox_df)

cox_hypertension_to_death_age_sex(cox_df)

cox_hypertension_to_death_sex(cox_df)

cox_hypertension_to_death_sex_educ(cox_df)

cox_df <- df %>%
    mutate(EDUC=as.factor(educ)) %>%
    # Need to remove as there are many NAs here.
    select(-educ, -HDLC, -LDLC) %>%
    # Remove the not really relevant data.
    select(-TIMEAP, -TIMEMI, -TIMEMIFC, -TIMEHYP, -TIMESTRK, -TIMECVD) %>%
    # Remove non-significant variables.
    select(-CURSMOKE, -GLUCOSE, -BMI)

cox_anychd_to_death(cox_df)

# ------------------------------------------------------------------------------


