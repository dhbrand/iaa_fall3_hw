############################
#                          #
#     MSA Class of 2019    #
#                          #
#    Survival Analysis:    #
#     Competing Risks      #
#                          #
#      Matthew Austin      #
#                          #
############################

# need these packages
library(survival)
library(survminer)
library(dplyr)

# load data
scotus <- read.csv("/your/data/folder/scotustenure.csv", header = TRUE)
# remove John Rutledge's second tenure
scotus <- scotus %>%
  dplyr::filter(!(subj == 5 & term == 2))

### set up data
# what we need to do is create separate data sets for death and for retirement
# this is sort of a roundabout way to do this, but it'll pay off later
# first thing: the event has to be a factor
event <- factor(scotus$status, 0:2, labels = c("sitting", "died", "retired"))
# next: create data sets
# the finegray() function in the survival package will do this automatically.
# this will add start, stop times (called fgstart, fgstop); a status variable
# (fgstatus) for the particular cause specified in the "etype" argument; and
# a vector of weights (fgwt) that we'll pass to the model functions
death_fgdata <- finegray(Surv(days/365.25, event) ~ .,
                         data = scotus, etype = "died")
retire_fgdata <- finegray(Surv(days/365.25, event) ~ .,
                          data = scotus, etype = "retired")
# (divided by 365.25 to get time in years)

### plot CIF
# need to fit survfit objects separately, then do a combined ggplot
sfdeath <- survfit(Surv(fgstart, fgstop, fgstatus) ~ 1,
                   data = death_fgdata, weight = fgwt)
sfret <- survfit(Surv(fgstart, fgstop, fgstatus) ~ 1,
                 data = retire_fgdata, weight = fgwt)
# combine into list
sflist <- list(death = sfdeath, ret = sfret)
# plot
# competing risks shit: finish this
ggsurvplot(sflist, combine = TRUE, fun = "event", conf.int = TRUE,
           break.x.by = 4, ylim = c(0, 1), legend.title = "status:",
           legend.labs = c("died", "retired"), break.y.by = 0.2, xlab = "years",
           ylab = "cumulative incidence", palette = c("black", "orange"))

### cause-specific hazard model
cox_death <- coxph(Surv(time = days/365.25, event = status == 1) ~ nomage +
                     nomyear + nomchief, data = scotus)
summary(cox_death)

### fine-gray model
fgdeath <- coxph(Surv(fgstart, fgstop, fgstatus) ~ nomage + nomyear + nomchief,
                 data = death_fgdata, weight = fgwt)
summary(fgdeath)
