# ------------------------------------------------------------------------------
# PMIM202 Health Data Modelling - Cox regression.
# ------------------------------------------------------------------------------
# 8.12.2023
# Dr Pete Arnold
# ------------------------------------------------------------------------------
# This code is the partner code for the powerpoint presentation.
#
# To use this file:
# Create a new R project and copy this file and the data directory into the root
# directory of the project (i.e. the same directory as your .Rproj file).
# You should then be able to open the project and run the R code without change.
# If you have problems with the paths, please let me know and I'll try to work
# out the solution - there is a package called here() which might help.
# ------------------------------------------------------------------------------
library(tidyverse)      # Used throughout for data processing.
library(survival)       # Survival analysis.
library(survminer)      # Survival plots
library(mfp)            # Breast cancer data.

# 1 Lung cancer survival
# ----------------------
# First off, a quick review of the survival analysis.

# 1.2 Variables
# -------------
data(cancer, package="survival")
glimpse(lung)
sample_n(lung, 10)

lung %>%
    pivot_longer(everything(), names_to='key', values_to='value') %>%
    ggplot(aes(value)) +
        facet_wrap(~key, scales="free") +
        geom_histogram(aes(fill=key), bins=20)

# 1.3 Data preparation
# --------------------
# We are not really interested in the institution code, as this might skew our
# data.
lung_2 <- lung %>% select(-inst)

# The status variable is currently set to 1 and 2, it is much more common to
# have this set to 0 and 1. We can also do the same with sex.
lung_2 <- lung_2 %>%
    mutate(status=ifelse(status==1, 0, 1)) %>%
    mutate(sex=ifelse(sex==1, 0, 1))

attach(lung_2)

# 1.4 Overall survival
# --------------------
# Get a table of the counts of each of the status values.
strata_table <- table(status)
row.names(strata_table) <- c("status=0","status=1")
strata_table

# Create a survival object:
surv.lung <- Surv(time, status)
head(surv.lung, 20)

# Fit a Kaplan-Meier survival curve.
model <- survfit(surv.lung~1)
# is the same as ...
# model <- survfit(Surv(time, status)~1, data=lung_2)

# Show the model results.
print(model)

# And the plot data.
summary(model)

# We are going to use the survminer package to enable plotting survival plots
# with ggplot (another option is autoplot from the ggfortify library).
ggsurvplot(model, data=lung_2)

# 1.5 Survival by gender
# ----------------------
# Now we can stratify the plots by another categorical variable.
strata_table <- table(status, sex)
row.names(strata_table) <- c("status=0","status=1")
colnames(strata_table) <- c("male","female")
strata_table

# Create a survival object and model.
model_sex <- survfit(Surv(time, status)~sex, data=lung_2)
print(model_sex)

# Plot the model.
ggsurvplot(model_sex, data=lung_2, legend.labs=c("Male","Female"))

# Show the difference between the survival curves
survdiff(Surv(time,status)~sex, data=lung_2)

# 1.6 Other variables of interest
# -------------------------------
# You could use a Cox regression to quickly view if there are other variables
# that might be of interest, we'll look at that in the Cox session. For now just
# try out a few, for example, ECOG performance.
model_sex_ecog <- survfit(Surv(time,status)~sex+ph.ecog, data=lung_2)
print(model_sex_ecog)

ggsurvplot(model_sex_ecog, data=lung_2)

# Or we can now look at the Cox regression with all the other variables
# included.
model_cox <- coxph(Surv(time,status)~., data=lung_2)
print(model_cox)

model_cox <- coxph(Surv(time, status)~sex+ph.ecog+ph.karno+wt.loss, data=lung_2)
summary(model_cox)

model_cox <- coxph(Surv(time, status)~sex+ph.ecog, data=lung_2)
summary(model_cox)

# So what are the problems of adding this variable to the model?

# It is ordinal data (categories). To some extent it describes the obvious: if
# you are less healthy you are more likely to die (endogenous data?). There
# might be an interaction between sex and ECOG.

# 2 Breast Cancer Survival
# ------------------------

# We'll look again at the breast cancer data and go straight into some Cox
# regressions. Again, this is the breast cancer data (Sauerbrei & Royston, 1999)
# within the mfp package (Ambler & Benner, 2015).

# 2.1 Data Description
# ----------------------

# | Variable Name	| Description |
# |---------------|-------------|
# | id | patient id 1…686 |
# | htreat | hormonal therapy, a factor at two levels 0 (no) and 1 (yes) |
# | age | of the patients in years |
# | menostat | menopausal status, a factor at two levels 1 (premenopausal) and
#              2 (postmenopausal) |
# | tumsize | tumor size (in mm) |
# | tumgrad | tumor grade, a ordered factor at levels 1 < 2 < 3 |
# | posnodal | number of positive nodes |
# | prm | progesterone receptor (in fmol) |
# | esm | estrogen receptor (in fmol) |
# | rfst | recurrence free survival time (in days) |
# | cens | censoring indicator (0 censored, 1 event) |

# 2.2 Variables
# -------------
data(GBSG)
sample_n(GBSG, 10)

# Histograms of all variables:
GBSG %>%
    select_if(is.numeric) %>%
    pivot_longer(everything(), names_to='key', values_to='value') %>%
    ggplot(aes(value)) +
        facet_wrap(~key, scales="free") +
        geom_histogram(aes(fill=key), bins=20)

# 2.3 Data Preparation
# --------------------
GBSG_2 <- GBSG %>% select(-id)

# 2.4 Calculate crude hazard ratios (Cox regression)
# --------------------------------------------------
coxph(Surv(rfst/365, cens)~., data=GBSG_2)
# Save this so we can refer to it later.
cox_all <- coxph(Surv(rfst, cens)~., data=GBSG_2)
summary(cox_all)

# 2.5 Cox regression stratified by hormonal therapy
# -------------------------------------------------
# It looks like women on hormone treatment have better chances of surviving, so
# let’s have a closer look at them.

# How many cases are there?
cases <- table(cens, htreat)
row.names(cases) <- c("cens=0","cens=1")
colnames(cases) <- c("no hormone treatment", "hormone treatment")
print(cases)

cox_htreat <- coxph(Surv(rfst, cens)~htreat, data=GBSG_2)
summary(cox_htreat)

# And, while we here, we can repeat the Kaplan-Meier plot.
surv_htreat <- survfit(Surv(rfst, cens)~htreat, data=GBSG_2)
print(surv_htreat)

ggsurvplot(surv_htreat, data=GBSG_2, legend.labs = c("no", "yes"))

# And look at the difference between the survival curves.
survdiff(Surv(rfst,cens) ~ htreat, data=GBSG_2)

# The Proportional Hazards Assumption of a Cox Regression can be tested using
# cox.zph (testing a constant hazard ratio).
cox_all <- coxph(Surv(rfst, cens)~., data=GBSG_2)
zph <- cox.zph(cox_all)
print(zph)

# It is, however, more informative to look at the plotted Schoenfeld residuals.
# We are looking for smooth fitted curves.
ggcoxzph(zph)
plot(zph)

# As you can see some variables vary over time and might affect your model.

# 3 Notes
# -------
# You might want to have a look at some R packages for Epidemiology, e.g.
#
# | Package | Description |
# |---------|-------------|
# | Epi | A Package for Statistical Analysis in Epidemiology e.g. create a lexis
#         object of your date |
# | popEpi | Functions for Epidemiological Analysis using Population Data e.g.
#            use the lexis object and calculate crude incidence rates (rate) |
# | epitools | Epidemiology Tools e.g. calculate odds ratios with confidence
#              intervals |

# References
# Ambler, G., & Benner, A. (2015). Mfp: Multivariable fractional polynomials.
# Retrieved from https://CRAN.R-project.org/package=mfp
#
# Loprinzi, C., Laurie, J., Wieand, H., Krook, J., Novotny, P., Kugler, J., …
# Klatt, N. (1994). Prospective evaluation of prognostic variables from
# patient-completed questionnaires. North Central Cancer Treatment Group.
# Journal of Clinical Oncology, 12, 601–607.
#
# Sauerbrei, W., & Royston, P. (1999). Building multivariable prognostic and
# diagnostic models: Transformation of the predictors by using fractional
# polynomials. Journal of the Royal Statistics Society Series A, 162(1), 71–94.
#
# Therneau, T. M., & Grambsch, P. M. (2000). Modeling survival data: Extending
# the Cox Model. New York: Springer.

# ------------------------------------------------------------------------------

# Bug fix for the ggcoxzph function.
# Step 1: type ggcoxzph into the console (note there are NO brackets at the end
#         of the function name.
# Step 2: copy the function into your code and edit the line
#         seval <- d * ((pmat %*% xtx) * pmat) %*% rep(1, df) to remove the d *
# Step 3: add the assignment operator to the function name (r to a new name).
# Step 4: run the definition.
ggcoxzph_fixed <- function (fit, resid = TRUE, se = TRUE, df = 4, nsmo = 40, var,
    point.col = "red", point.size = 1, point.shape = 19, point.alpha = 1,
    caption = NULL, ggtheme = theme_survminer(), ...)
{
    x <- fit
    if (!methods::is(x, "cox.zph"))
        stop("Can't handle an object of class ", class(x))
    xx <- x$x
    yy <- x$y
    d <- nrow(yy)
    df <- max(df)
    nvar <- ncol(yy)
    pred.x <- seq(from = min(xx), to = max(xx), length = nsmo)
    temp <- c(pred.x, xx)
    lmat <- splines::ns(temp, df = df, intercept = TRUE)
    pmat <- lmat[1:nsmo, ]
    xmat <- lmat[-(1:nsmo), ]
    qmat <- qr(xmat)
    if (qmat$rank < df)
        stop("Spline fit is singular, try a smaller degrees of freedom")
    if (se) {
        bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
        xtx <- bk %*% t(bk)
        # seval <- d * ((pmat %*% xtx) * pmat) %*% rep(1, df)
        seval <- ((pmat %*% xtx) * pmat) %*% rep(1, df)
    }
    ylab <- paste("Beta(t) for", dimnames(yy)[[2]])
    if (missing(var))
        var <- 1:nvar
    else {
        if (is.character(var))
            var <- match(var, dimnames(yy)[[2]])
        if (any(is.na(var)) || max(var) > nvar || min(var) <
            1)
            stop("Invalid variable requested")
    }
    if (x$transform == "log") {
        xx <- exp(xx)
        pred.x <- exp(pred.x)
    }
    else if (x$transform != "identity") {
        xtime <- as.numeric(dimnames(yy)[[1]])
        indx <- !duplicated(xx)
        apr1 <- approx(xx[indx], xtime[indx], seq(min(xx), max(xx),
            length = 17)[2 * (1:8)])
        temp <- signif(apr1$y, 2)
        apr2 <- approx(xtime[indx], xx[indx], temp)
        xaxisval <- apr2$y
        xaxislab <- rep("", 8)
        for (i in 1:8) xaxislab[i] <- format(temp[i])
    }
    plots <- list()
    plots <- lapply(var, function(i) {
        invisible(pval <- round(x$table[i, 3], 4))
        gplot <- ggplot() + labs(title = paste0("Schoenfeld Individual Test p: ",
            pval)) + ggtheme
        y <- yy[, i]
        yhat <- as.vector(pmat %*% qr.coef(qmat, y))
        if (resid)
            yr <- range(yhat, y)
        else yr <- range(yhat)
        if (se) {
            temp <- as.vector(2 * sqrt(x$var[i, i] * seval))
            yup <- yhat + temp
            ylow <- yhat - temp
            yr <- range(yr, yup, ylow)
        }
        if (x$transform == "identity") {
            gplot <- gplot + geom_line(aes(x = pred.x, y = yhat)) +
                xlab("Time") + ylab(ylab[i]) + ylim(yr)
        }
        else if (x$transform == "log") {
            gplot <- gplot + geom_line(aes(x = log(pred.x), y = yhat)) +
                xlab("Time") + ylab(ylab[i]) + ylim(yr)
        }
        else {
            gplot <- gplot + geom_line(aes(x = pred.x, y = yhat)) +
                xlab("Time") + ylab(ylab[i]) + scale_x_continuous(breaks = xaxisval,
                labels = xaxislab) + ylim(yr)
        }
        if (resid)
            gplot <- gplot + geom_point(aes(x = xx, y = y), col = point.col,
                shape = point.shape, size = point.size, alpha = point.alpha)
        if (se) {
            gplot <- gplot + geom_line(aes(x = pred.x, y = yup),
                lty = "dashed") + geom_line(aes(x = pred.x, y = ylow),
                lty = "dashed")
        }
        ggpubr::ggpar(gplot, ...)
    })
    names(plots) <- var
    class(plots) <- c("ggcoxzph", "ggsurv", "list")
    if ("GLOBAL" %in% rownames(x$table))
        global_p <- x$table["GLOBAL", 3]
    else global_p <- NULL
    attr(plots, "global_pval") <- global_p
    attr(plots, "caption") <- caption
    plots
}

ggcoxzph_fixed(zph)
