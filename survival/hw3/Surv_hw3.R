#install.packages("flexsurv")
library(survival)
library(survminer)
library(visreg)
library(flexsurv)
library(muhaz)
library(tidyverse)

# 0. Interpret SE for backup, coeff/SE for elevation
# @. Find the mean person 
# 1. Check linearity/time varying assumptions for PH
#      Linearity?: visreg(., "age"), ogle to see if approximately linear
#      Time-varying?: plot hazards (ggsurvplot(survfit(.), data, fun = "cloglog") to see if they are parallel
#         
# 2. Check concordance /w concordance.coxph(object = )
# 3. Check martingale/deviance/residuals with residuals(., type=c("martingale","deviance", "schoenfeld))
#      dfbetas with ggcoxdiagnostics(fit, type = "dfbetas")
#      whether subject has an event sooner or later than expected or if an observation is influential
# 4. 

# import data
katrina = read.csv("C:\\Users\\Grant\\Downloads\\survivalcsv\\katrina.csv", header=TRUE)
katrina$ID = 1:nrow(katrina)

## RECALL: Our goal is to model motor and surge failures together, censor all other reasons
# create an aft model to compare with the cox model
aft_mod = flexsurvreg(Surv(time=hour, event=reason %in% c(2, 3)) ~ backup + bridgecrane + servo +
                         trashrack + elevation + slope + age, data=katrina, dist="weibull")
# create a cox model to compare with the AFT model
cox_mod = coxph(Surv(time=hour, event=reason %in% c(2, 3)) ~ backup + bridgecrane + servo + 
                         trashrack + elevation + slope + age, data=katrina)
cox_modb = coxph(Surv(time=hour, reason==1) ~ backup + bridgecrane + servo + 
                  trashrack + elevation + slope + age, data=katrina)
# Quick notes on PH vs. AFT models
## AFT models: predictors have a multiplicative effect on FAILURE TIME
##  PH models: predictors have a multiplicative effect on the HAZARD
## A model is either a PH or AFT model, except for when the data follows a Weibull dist.

# PH assumptions
## Linear relationship between predictors and log(hazard)
## No interactions between hazards and time

summary(aft_mod)
summary(cox_mod)
summary(cox)
cox_mod$means # senseless 

## Interpreting some coefficients
summary(cox_mod)

# The coefficient of 0.9109 for backup indicates that having a backup pump decreases
#  the risk of failure by motor or surge related causes by (1-0.9109) 8.91% relative to not having a backup pump, meaning that it is 8.91% less likely to fail than comparable pumps at the same time point and for the same reasons
#  the standard error of our estimate for backup, 0.13682, indicates that we believe the true estimate for the effect of a backup oon
#  the risk of failure is between ((1-0.6966) * 100) percent and ((1 - 1.1910)*100) percent
# The coefficient of 0.9305 for elevation indicates that each one unit increase in elevation is associated 
#  with a reduction in the risk of failure by motor or related causes by (1-0.9305) 6.95%
#  the standard error for elevation indicates the that we expect the true effect to be between a 19.96% decrease in risk and a 8.18% increase in risk 
1-0.9305
# Survival curves
ggsurvplot(survfit(aft_mod), data = katrina, legend = "none", 
           xlab = "hour", ylab = "survival probability")
ggsurvplot(survfit(cox_mod), data = katrina, legend = "none", 
           xlab = "hour", ylab = "survival probability")
visreg(cox_mod, "age") # sure, yeah whatever that's linear 


# Time-varying

## check to see if the hazards are parallel to each other
##   by stratifying on backup
backup_strat <- coxph(Surv(hour, reason %in% c(2,3)) ~ strata(backup) + bridgecrane + servo + 
                     trashrack + elevation + slope + age, data=katrina)
ggsurvplot(survfit(cox_mod, backup_strat), data = katrina) # dataframe needed here?

### checking PH with schoenfeld residuals
# testing correlation of residuals with time
fit_zph <- cox.zph(cox_mod, transform = "km")
fit_zph # global tests/all tests significant, we reject the assumption of proportional hazards

## zph plots

# age

plot(fit_zph, var = "age")
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_mod$coef["age"], col = "purple", lty = 2, lwd = 2) # model estimate
# trying different transformations:
# identity
fit_zph_i <- cox.zph(cox_mod, transform = "identity")
plot(fit_zph_i, var = "age")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["age"], col = "purple", lty = 2, lwd = 2)
# log
fit_zph_log <- cox.zph(cox_mod, transform = "log")
plot(fit_zph_log, var = "age")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["age"], col = "purple", lty = 2, lwd = 2)


# backup

plot(fit_zph, var = "backup") # not really linear
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_mod$coef["backup"], col = "purple", lty = 2, lwd = 2) # model estimate
# trying different transformations:
# identity
fit_zph_i_back <- cox.zph(cox_mod, transform = "identity")
plot(fit_zph_i_back, var = "backup")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["backup"], col = "purple", lty = 2, lwd = 2)
# log
fit_zph_log_back <- cox.zph(cox_mod, transform = "log")
plot(fit_zph_log_back, var = "backup")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["backup"], col = "purple", lty = 2, lwd = 2)


# servo

plot(fit_zph, var = "servo") # not really linear
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_mod$coef["servo"], col = "purple", lty = 2, lwd = 2) # model estimate
# trying different transformations:
# identity
fit_zph_i_serv <- cox.zph(cox_mod, transform = "identity")
plot(fit_zph_i, var = "servo")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["servo"], col = "purple", lty = 2, lwd = 2)
# log
fit_zph_log_serv <- cox.zph(cox_mod, transform = "log")
plot(fit_zph_log_serv, var = "servo")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["servo"], col = "purple", lty = 2, lwd = 2)

# trash

plot(fit_zph, var = "trashrack") # not really linear
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_mod$coef["servo"], col = "purple", lty = 2, lwd = 2) # model estimate
# trying different transformations:
# identity
fit_zph_i_serv <- cox.zph(cox_mod, transform = "identity")
plot(fit_zph_i, var = "servo")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["servo"], col = "purple", lty = 2, lwd = 2)
# log
fit_zph_log_serv <- cox.zph(cox_mod, transform = "log")
plot(fit_zph_log_serv, var = "servo")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["servo"], col = "purple", lty = 2, lwd = 2)

### concordance
concordance(cox_mod)
??concordance
