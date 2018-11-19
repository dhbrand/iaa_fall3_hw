###### install.packages("flexsurv")
library(survival)
library(survminer)
library(visreg)
library(flexsurv)
library(muhaz)
library(tidyverse)
######

########################## NOTES ###############################################################################
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
#
# Quick notes on PH vs. AFT models
## AFT models: predictors have a multiplicative effect on FAILURE TIME
##  PH models: predictors have a multiplicative effect on the HAZARD
## A model is either a PH or AFT model, except for when the data follows a Weibull dist.

# PH assumptions
## Linear relationship between predictors and log(hazard)
## No interactions between hazards and time
########################## NOTES ###############################################################################


# import data
katrina = read.csv("C:\\Users\\Grant\\Downloads\\survivalcsv\\katrina.csv", header=TRUE)
katrina$ID = 1:nrow(katrina)

## RECALL: Our goal is to model motor and surge failures together, censor all other reasons
# create an aft model to compare with the cox model
aft_mod = survreg(Surv(time=hour, event=reason %in% c(2, 3)) ~ backup + bridgecrane + servo +
                    trashrack + elevation + slope + age, data=katrina, dist="weibull")
# create a cox model to compare with the AFT model
cox_mod = coxph(Surv(time=hour, event=reason %in% c(2, 3)) ~ backup + bridgecrane + servo + 
                  trashrack + elevation + slope + age, data=katrina)

# check model summaries
summary(aft_mod)
exp(coef(aft_mod))
summary(cox_mod) # will look at backup, elevation interpretations for the purpose of the assignment
cox_mod$means # senseless, get fractional coefficients for binary variables

###################### Interpreting some coefficients ###############################################################################
summary(cox_mod)

# The coefficient of 0.9109 for backup indicates that having a backup pump decreases
#  the risk of failure by motor or surge related causes by (1-0.9109) 8.91% relative to not having a backup pump, meaning that it is 8.91% less likely to fail than comparable pumps at the same time point and for the same reasons
#  the standard error of our estimate for backup, 0.13682, indicates that we believe the true estimate for the effect of a backup oon
#  the risk of failure is between ((1-0.6966) * 100) percent and ((1 - 1.1910)*100) percent

# The coefficient of 0.9046 for elevation indicates that each one unit increase in slope is associated 
#  with a reduction in the risk of failure by motor or surge related causes by (1-0.9046) 9.54%
#  the standard error for elevation indicates the that we expect the true effect to be between a 15.51% decrease in risk and a 3.14% decrease in risk 

# trashrack!!
###################### Interpreting some coefficients ###############################################################################

table(katrina$reason)

########################## Assumption Checking ###############################################################################
# Linearity via survival curves
ggsurvplot(survfit(cox_mod), data = katrina, legend = "none", 
           xlab = "Hour", ylab = "Survival Probability", title="Survival Curve for Cox PH")
visreg(cox_mod, "age") # sure, yeah whatever that's linear. linear assumption for Cox PH is satisfied
visreg(cox_mod, "age", xlab = "age", ylab = "partial residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()
visreg(cox_mod, "slope", xlab = "age", ylab = "partial residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()
visreg(cox_mod, "elevation", xlab = "age", ylab = "partial residuals", gg = TRUE,
       band = FALSE) +
  geom_smooth(col = "red", fill = "red") + theme_bw()

# Time-varying coefficients via checking PH with schoenfeld residuals
# testing correlation of residuals with time
fit_zph <- cox.zph(cox_mod, transform = "km")
fit_zph # global tests/all tests significant, we reject the assumption of proportional hazards

## zph plots

# age, not really linear!

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


# backup, not really linear!

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


# servo, not really linear! 

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

# elevation
plot(fit_zph, var = "elevation", main = "Elevation Residual's Correlation With Time") # not really linear
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_mod$coef["servo"], col = "purple", lty = 2, lwd = 2) # model estimate
# trying different transformations:
# identity
fit_zph_i_elev <- cox.zph(cox_mod, transform = "identity")
plot(fit_zph_i_elev, var = "elevation")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["elevation"], col = "purple", lty = 2, lwd = 2)
# log
fit_zph_log_elev <- cox.zph(cox_mod, transform = "log")
plot(fit_zph_log_elev, var = "elevation")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["elevation"], col = "purple", lty = 2, lwd = 2)


# trash
?plot
plot(fit_zph, var = "trashrack", main = "Trashrack Residual's Correlation With Time") # not really linear
abline(h = 0, col = "red") # reference line at 0
abline(h = cox_mod$coef["trashrack"], col = "purple", lty = 2, lwd = 2) # model estimate
# trying different transformations:
# identity
fit_zph_i_trash <- cox.zph(cox_mod, transform = "identity")
plot(fit_zph_i_trash, var = "trashrack")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["servo"], col = "purple", lty = 2, lwd = 2)
# log
fit_zph_log_trash <- cox.zph(cox_mod, transform = "log")
plot(fit_zph_log_trash, var = "trashrack")
abline(h = 0, col = "red")
abline(h = cox_mod$coef["trashrack"], col = "purple", lty = 2, lwd = 2)

#############################################################################################################

## check to see if the hazards are parallel to each other
# stratifying on trashrack
trash_strat <- coxph(Surv(hour, reason %in% c(2,3)) ~ strata(trashrack) + backup + bridgecrane + servo  
                     + elevation + slope + age, data=katrina)
summary(trash_strat)
ggsurvplot(survfit(trash_strat), data = katrina, fun = "cloglog", title="Plot of Trashrack Strata") # dataframe needed here?

# stratifying on backup
backup_strat <- coxph(Surv(hour, reason %in% c(2,3)) ~ strata(backup) + bridgecrane + servo + 
                        trashrack + elevation + slope + age, data=katrina)
summary(backup_strat)
ggsurvplot(survfit(backup_strat), data = katrina, fun = "cloglog") # dataframe needed here?
### concordance
concordance(cox_mod)
??concordance

############################ Bullet point 3 ###############################

katrina <- read.csv('Survival Analytics\\katrina.csv')
kat_long <- katrina[,-(1:8)]
#############The Loop###################
final <- data.frame(ID = NULL, Start = NULL, Stop = NULL, Motor = NULL, Event = NULL, New = NULL)
result <- rep(0,5)
for (ID in 1:nrow(katrina)){
  result[1] <- ID
  for (ii in 12:47){
    result[2:3] <- 0
    result[2] <- ii
    result[3] <- ii + 1
    kat_col <- katrina[ID,(ii-11):ii]
    if (sum(kat_col, na.rm = TRUE) == 12){
      result[4] <- 1
    }else{
      result[4] <- 0
    }
    result[5] <- ifelse(katrina$reason[ID] %in% c(2,3) & katrina$hour[ID] == result[3],1,0)
    final <- rbind(final,result)
    if(result[5] == 1){
      break
    }
  }
}
names(final) <- c("ID","Start","Stop","Motor","Survive2")
katrina$ID <- seq(1,770)
combined <- inner_join(final,katrina,by=c("ID")) %>%
  select(-starts_with('h'))
################Model######################
##Previous model from the first two points
cox_mod1 = coxph(Surv(time=hour, event=reason %in% c(2, 3)) ~ backup + bridgecrane + servo + 
                   trashrack + elevation + slope + age, data=katrina)
summary(cox_mod1)
#The new dataset has a new response variable that has the appropriately sensored obs called Survival2 
cox_mod1 = coxph(Surv(time=hour, event=reason %in% c(2, 3)) ~ backup + bridgecrane + servo + 
                   trashrack + elevation + slope + age, data=katrina)
#The time variable is Stop and the new response with the altered failure reasons is called Survive2
#See line 25 for the logic 
cox_mod2 = coxph(Surv(time=Stop, event=Survive2) ~ backup + bridgecrane + servo + 
                   trashrack + elevation + slope + age + Motor, data=combined)
summary(cox_mod2)
##The p-value for Motor is 0.1727, thus I would conclude that whether the motor of the pump was on for 12 hours has no significant
#effect on the failure of the pump. 