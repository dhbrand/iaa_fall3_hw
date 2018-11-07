##############################
#                            #
#       Orange Team 4        #
#                            #
#        Homework 2          #
#                            #
#                            #
#    Zachary Wasielewski     #
#                            #
##############################

# load packages
library(survival)
library(survminer)
library(muhaz)
library(data.table)
library(tidyverse)
library(flexsurv)

direct = "C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Survival/Data/"
katrina_raw = fread(paste(direct,"katrina.csv", sep=""))
hours = cbind('h1','h2','h3','h4','h5','h6','h7','h8','h9','h10','h11','h12','h13','h14','h15','h16','h17','h18','h19','h20','h21','h22','h23','h24','h25','h26','h27','h28','h29','h30','h31','h32','h33','h34','h35','h36','h37','h38','h39','h40','h41','h42','h43','h44','h45','h46','h47','h48')
katrina = subset(katrina_raw, select = !names(katrina_raw) %in% hours)

## AFT Model
fit <- survreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo 
               + trashrack + elevation + age + slope, data = katrina, dist = "weibull")
summary(fit) # I think this says exponential is not okay... which is weird to continue and use it
exp(coef(fit))


## Weibull
fit_wb <- flexsurvreg(Surv(hour, reason==1) ~ backup + bridgecrane + servo 
                      + trashrack + elevation + age + slope, data = katrina, dist = "weibull")

plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "weibull distribution")


## Exponential
fit_exp <- flexsurvreg(Surv(hour, reason==1) ~ backup + bridgecrane + servo 
                      + trashrack + elevation + age + slope, data = katrina, dist = "exponential")

plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "exponential distribution")


## Log Normal
fit_lnorm <- flexsurvreg(Surv(hour, reason==1) ~ backup + bridgecrane + servo 
                      + trashrack + elevation + age + slope, data = katrina, dist = "lognorm")

plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "log normal distribution")


## Log Logistic
fit_llogis <- flexsurvreg(Surv(hour, reason==1) ~ backup + bridgecrane + servo 
                      + trashrack + elevation + age + slope, data = katrina, dist = "llogis")

plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "log logistic distribution")


##### It appears to me that the best distribution of the data is likely the Weibull based on
##### the cumulative hazard falling within the confidence intervals the entire time. The other
##### distributions have trouble early hours or hour 8 of 18ish


## Weibull
fit1 <- survreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo 
               + trashrack + elevation + age + slope, data = katrina, dist = "weibull")
exp(coef(fit1)) 
    # interpretation of age:
        # The predicted time to flooding is 1.06 times shorter for every additional year of age
    # interpretation of trashrack:
        # For pumps who had a trashrack, the predicted flooding time was 0.79 times shorter than those who didn't

## Log Norm
fit2 <- survreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo 
               + trashrack + elevation + age + slope, data = katrina, dist = "lognorm")
exp(coef(fit2))
    # interpretation of age:
        # The predicted time to flooding is 1.01 times shorter for every additional year of age
    # interpretation of trashrack:
        # For pumps who had a trashrack, the predicted flooding time was 0.77 times shorter than those who didn't


## Find pumps that failed due to flooding early and figure our which didnt have a backup or servomechanism (or ones that are newer with low elevation)
    # find top 20 that fail early due to 
count(katrina[which(katrina$reason == 1 & katrina$hour < 16 & katrina$backup == 0 |
                      katrina$reason == 1 & katrina$hour < 16 & katrina$servo  == 0)])
    # list the 20 pumps by row
which(katrina$reason == 1 & katrina$hour < 16 & katrina$backup == 0 |
        katrina$reason == 1 & katrina$hour < 16 & katrina$servo  == 0)
    # find which need BOTH upgrade (choose servo because its more important)
which(katrina$reason == 1 & katrina$hour < 16 & katrina$servo  == 0)
      # 320 327 328 330 355 356 360 381 382 383 385 390 398 401 405 418 424 428 get servo mechanisms
      # 332 420 get backups?
