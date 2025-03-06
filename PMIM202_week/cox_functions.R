# ------------------------------------------------------------------------------
# Exploration of the Framingham dataset for regression and machine learning.
# ------------------------------------------------------------------------------
# Cox regressions.
# ------------------------------------------------------------------------------
# 2021.12.07 Pete Arnold
# ------------------------------------------------------------------------------

cox_hypertension_to_death_all <- function(df){
    # Data prep.
    death_df <- df %>%
        group_by(RANDID) %>% filter(row_number()==1) %>% ungroup() %>%
        filter(PREVHYP!=1 & HYPERTEN==1) %>%
        select(-RANDID, -TIME, -PREVHYP, -HYPERTEN)
    # Regression.
    cox <- coxph(data=death_df,
        formula=Surv(TIMEDTH - TIMEHYP, DEATH)~.)
    # Results.
    print(cox)
    # Checks.
    zph <- cox.zph(cox)
    print(zph)
    plot(zph)
}

cox_hypertension_to_death_age_sex <- function(df){
    # Data prep.
    death_df <- df %>%
        group_by(RANDID) %>% filter(row_number()==1) %>% ungroup() %>%
        filter(PREVHYP!=1 & HYPERTEN==1) %>%
        select(-RANDID, -TIME, -PREVHYP, -HYPERTEN)
    # Regression.
    cox <- coxph(data=death_df,
        formula=Surv(TIMEDTH - TIMEHYP, DEATH)~AGE+SEX)
    # Results.
    print(cox)
    # Checks.
    zph <- cox.zph(cox)
    print(zph)
    plot(zph)
}

cox_hypertension_to_death_sex <- function(df){
    # Data prep.
    death_df <- df %>%
        group_by(RANDID) %>% filter(row_number()==1) %>% ungroup() %>%
        filter(PREVHYP!=1 & HYPERTEN==1) %>%
        select(-RANDID, -TIME, -PREVHYP, -HYPERTEN)
    # Regression.
    cox <- coxph(data=death_df,
        formula=Surv(TIMEDTH - TIMEHYP, DEATH)~SEX)
    # Results.
    print(cox)
    # Checks.
    zph <- cox.zph(cox)
    print(zph)
    plot(zph)
}

cox_hypertension_to_death_sex_educ <- function(df){
    # Data prep.
    death_df <- df %>%
        group_by(RANDID) %>% filter(row_number()==1) %>% ungroup() %>%
        filter(PREVHYP!=1 & HYPERTEN==1) %>%
        select(-RANDID, -TIME, -PREVHYP, -HYPERTEN)
    # Regression.
    cox <- coxph(data=death_df,
        formula=Surv(TIMEDTH - TIMEHYP, DEATH)~SEX+EDUC)
    # Results.
    print(cox)
    # Checks.
    zph <- cox.zph(cox)
    print(zph)
    plot(zph)
    # And a survival fit.
    model <- survfit(data=death_df,
                formula=Surv(TIMEDTH - TIMEHYP, DEATH)~SEX+EDUC)
    print(model)
}

cox_anychd_to_death <- function(df){
    # Data prep.
    death_df <- df %>%
        group_by(RANDID) %>% filter(row_number()==1) %>% ungroup() %>%
        filter(PREVCHD!=1 & ANYCHD==1) %>%
        select(-RANDID, -TIME, -PREVCHD, -ANYCHD)
    # Regression.
    cox <- coxph(data=death_df,
        formula=Surv(TIMEDTH - TIMECHD, DEATH)~.)
    # Results.
    print(cox)
    # Checks.
    zph <- cox.zph(cox)
    print(zph)
    plot(zph)
    # And a survival fit.
    model <- survfit(data=death_df,
                formula=Surv(TIMEDTH - TIMECHD, DEATH)~1)
    print(model)
}
