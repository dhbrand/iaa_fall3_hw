##############################
#                            #
#     MSA Class of 2019      #
#                            #
#     Survival Analysis:     #
#     Survival & Hazards     #
#                            #
#       Matthew Austin       #
#                            #
##############################

# need these packages
library(survival)
library(survminer)
library(muhaz)

# simple baseball example
lcs <- data.frame(
  team = c(
    "MIL", "LAD", "ATL", "COL", "CHC",
    "BOS", "HOU", "CLE", "NYY", "OAK"
  ),
  time = c(
    3, 4, 4, 4, 1,
    4, 3, 3, 5, 1
  ),
  event = c(
    0, 0, 1, 1, 1,
    0, 0, 1, 1, 1
  )
)
# you can see here i'm creating a dataset with a column for time and event
lcs

# in R, the most common tool (but certainly not the only way) for survival
# analysis is with the survival package (shocking, i know).
# to get started with survival analysis, we need to create a Surv object
# using those time and event columns
# Surv() takes both of these as the arguments "time" and "event" respectively.
# with "event == 1", you specify the code for an event (contrast to SAS where
# you specify the code for censoring). the default is 1 = event, 0 = censored,
# but as always, it doesn't hurt to specify and we'll need to do so later in
# course anyway
# the Surv() object now has the survival time for each observation, and a "+"
# to indicate censored observations
with(lcs, Surv(time = time, event = event == 1))

# to create survival curves, survfit() takes a formula just like usual
# ~ 1 to fit model with no predictors
lcs_fit <- survfit(Surv(time, event == 1) ~ 1, data = lcs)
# calling this gives the sample size, #events (elimination), median, and CI
lcs_fit
# and the summary gives us the time, number at risk, number of events,
# and S(t), which matches what we did in the slides
summary(lcs_fit)

# ok, moving on to the recidivism study
recid <- read.csv("data/recid.csv", header = TRUE)

### survival curves ###
recid_fit <- survfit(Surv(week, arrest == 1) ~ 1, data = recid)
# can you figure out why it doesn't give you the median here?
recid_fit

# you can plot the curve like so...
# plot(recid_fit, mark.time = TRUE,
#      xlab = "week", ylab = "survival probability",
#      main = "recidivism Kaplan-Meier curve")
# but the default R plots in the survival package are pretty bland. the
# survminer package has nice plots, and this is typically what i use
ggsurvplot(lcs_fit, data = lcs, conf.int = FALSE, palette = "grey")

# group by work experience and plot
recid_wexp <- survfit(Surv(time = week, event = arrest) ~ wexp, data = recid)
ggsurvplot(recid_wexp, conf.int = TRUE, palette = "grey")

### log-rank test ###
# use the survdiff() function
# this also takes the formula & data inputs as usual, but need a new argument:
# rho = 0 is the normal log-rank test, rho = 1 is the weighted test
survdiff(Surv(time = week, event = arrest) ~ wexp, rho = 0, data = recid)
survdiff(Surv(time = week, event = arrest) ~ wexp, rho = 1, data = recid)

### hazard functions ###
# unfortunately ggsurvplot() can't plot the hazard, so we'll use the muhaz
# package
# also unfortunately, we need to change the dataset a bit so that it plots
# correctly
# here, i am making a new variable week2, where i'm setting censored observations
# to have week2 = 53 so that the function doesn't plot them as if they all had
# the event in the last week
recid$week2 <- ifelse(recid$week == 52 & recid$arrest == 0, 53, recid$week)
# kphaz.fit() has the same arguments as Surv()
recid_haz <- with(recid, kphaz.fit(week2, arrest))
# and we plot it with kphaz.plot()
kphaz.plot(recid_haz, main = "hazard function")
# to see why i needed to restructure this, look at the plot using week instead
# of week2

### cumulative hazard ###
ggsurvplot(recid_fit, fun = "cumhaz", palette = "grey")
