##############################
#                            #
#     MSA Class of 2019      #
#                            #
#     Survival Analysis:     #
#    Accelerated Failure     #
#        Time Models         #
#                            #
#       Matthew Austin       #
#                            #
##############################

# need these packages
library(survival)
library(survminer)
library(flexsurv)
library(dplyr)

# load data
recid <- read.csv("survival/data//recid.csv", header = TRUE)

### fitting AFT models
# fit AFT models using survreg()
# like glm(), there's a "dist" argument (default is weibull) that we'll get to
# in a bit, but everything else works the same as you've seen before
fit <- survreg(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar + paro +
                 prio, data = recid, dist = "weibull")
# and you can call a summary, which gives coefficient estimates, SEs, etc.
# notice the test for log(scale), which is testing whether log(scale) = 0,
# meaning testing if exponential is ok
summary(fit)
exp(coef(fit)) # exponentiate estimates

### checking distributions
# let's look at the fit of some different distributions
# we'll quickly switch to a different package for the purpose of plotting
library(flexsurv)
# the syntax in flexsurvreg() is the same as survreg()
# weibull distribution
fit_wb <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                        mar + paro + prio, data = recid, dist = "weibull")
# plot cumulative hazard along with KM estimates using plot(..., type = "cumhaz")
# you hope the curve and CI are pretty close to the KM estimates
# the "ci" option is the CI of your fitted distribution, the "conf.int" option
# is for the CI of the KM estimate
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "weibull distribution")

# exponential distribution
fit_exp <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                         mar + paro + prio, data = recid, dist = "exponential")
# plot shows that this distribution isn't a good fit
plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n", 
     xlab = "week", ylab = "cumulative hazard",
     main = "exponential distribution")

# some other distributions: lognormal
fit_lnorm <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                           mar + paro + prio, data = recid, dist = "lognormal")
# plot shows lack of fit in very early time periods
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard", main = "lognormal distribution")

# log-logistic
fit_llogis <- flexsurvreg(Surv(week, arrest == 1) ~ fin + age + race + wexp +
                            mar + paro + prio, data = recid, dist = "llogis")
plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "week", ylab = "cumulative hazard",
     main = "log-logistic distribution")

# these plots are intended to be guides, not a definitive be-all-end-all
# note that weibull, log-logistic, and possibly lognormal fit equally well here
# (probably because this dataset doesn't have a lot of events), but could give
# substantially different estimates!

# fun fact: if you pretend everyone actually had the event at whatever their
# time is, lognormal = linear regression of log(time). i'm like 93% sure i'm
# not just making that up so let's check:
# in Surv(), if you only specify time and not an event, then it assumes
# everyone had the event
fit_lnorm <- survreg(Surv(week) ~ fin + age + race + wexp + mar + paro + prio,
                     data = recid, dist = "lognormal")
fit_lm <- lm(log(week) ~ fin + age + race + wexp + mar + paro + prio,
             data = recid)
# compare estimated coefficients
data.frame("lnorm_estimate" = coef(fit_lnorm),
           "linreg_estimate" = coef(fit_lm))


### predictions ###
# predicted quantiles
# predict() returns a list with the type (called "fit") and se (called "se.fit")
survprob_75_50_25 <- predict(fit, type = "quantile", se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
# note that these are the quantiles for EVENT times, not survival times
# so 0.25 means this is the time where events have happened to 25% of people,
# so S(t) = 0.75 at whatever this time is
head(survprob_75_50_25$fit)

# or you get the mean/expected time to recidivism
pred_time <- predict(fit, type = "response", se.fit = TRUE)
with(pred_time, head(cbind(fit, se.fit)))

# to see predicted survival probabilities at the time where we actually
# observed the event, use 1- psurvreg()
# the first argument of psurvreg() is the observed times
# next is the "mean", which is the linear predictor
# and scale is the scale from the model
# and last, we specify the distribution
survprob_actual <- 1 - psurvreg(recid$week,
                                mean = predict(fit, type = "lp"),
                                scale = fit$scale,
                                distribution = fit$dist)
head(survprob_actual)

# or you can predict survival probabilities at some time of interest
# this first one is the estimated survival probability at their actual time
survprob_actual <- 1 - psurvreg(recid$week, mean = fit$linear,
                                scale = fit$scale,
                                distribution = fit$dist)
# let's look at the 10-week survival probabilities
# I'll still use psurvreg(), but my "time" for everyone is just 10 instead of
# their actual week
survprob_10wk <- 1 - psurvreg(10, mean = predict(fit, type = "lp"), 
                              scale = fit$scale,
                              distribution = fit$dist)
head(survprob_10wk)

# so now for the people who didn't get financial aid, we're going to predict
# the mean time to recidivism if they HAD gotten it
# to do this, we're assuming that they'll still have the event at the same
# estimated survival probability as they did previously
recid_nofin <- recid %>%
  # first, we need to get the linear predictor from the old model
  mutate(old_lp = predict(fit, type = "lp"),
         # add ID variable so we know which subjects they are
         ID = row_number()) %>%
  # next, we're only interested in those who had the event AND no financial aid
  dplyr::filter(arrest == 1, fin == 0) %>%
  # now i'm doing two things:
  # 1. find the survival prob at the time of their event
  # 2. pretending they did have financial aid (change fin from 0 to 1)
  mutate(old_time = week,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_fin = fin,
         fin = old_fin + 1)

# now with that dataset, i need to find their new time
results <- recid_nofin %>%
  # estimate their new linear predictor value if they had financial aid
  mutate(new_lp = predict(fit, newdata = recid_nofin, type = "lp"),
         # now, keeping the same survival probability as before, get the time
         # corresponding to the new_lp value
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         # and compute the difference
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
head(results)
