## ------------------------------------------------------------------------
library(survival)
library(survminer)
library(muhaz)
library(tidyverse)

# Loading the katrina dataset
## ------------------------------------------------------------------------
katrina <- read_csv("survival/data/katrina.csv")

# creating survival object for katrina using the survive column as the event and fitting the survival function using no predictions
## ------------------------------------------------------------------------
kat_fit <- survfit(Surv(hour, survive == 0) ~ 1, data = katrina)
summary(kat_fit)

# get the plot of the survival function
## ------------------------------------------------------------------------
ggsurvplot(kat_fit, conf.int = TRUE, palette = "grey")

# finding summary statistics of each reason group
## ------------------------------------------------------------------------

# percentage of pumps which survived failure

katrina %>% 
  group_by(survive) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

# percentage of pumps in each failure type
katrina %>% 
  group_by(reason) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

# median surival time
kat_med <- survfit(Surv(hour, survive == 0) ~ 1, data = katrina)
which(kat_med$surv <= 0.5)
# median survival time is hour 45


# creating survival object for katrina using the survive column as the event and fitting the survival function using reason groups
## ------------------------------------------------------------------------
kat_fit <- survfit(Surv(hour, survive == 0) ~ reason, data = katrina, subset = reason != 0)
summary(kat_fit)

# get the plot of the survival functions of each group
## ------------------------------------------------------------------------
ggsurvplot(kat_fit, data = katrina, conf.int = TRUE, palette = "grey")

# are the reason groups statistically different
## ------------------------------------------------------------------------
survdiff(Surv(hour, survive == 0) ~ reason, data = katrina, subset = reason != 0)

## ------------------------------------------------------------------------
pairwise_survdiff(Surv(hour, survive == 0) ~ reason, data = katrina[katrina$reason != 0,])

# creating groups for failures as water or mechanical
## ------------------------------------------------------------------------
katrina <- katrina %>% 
  mutate(fail_type = if_else(reason == 1 | reason == 3, "water", "mech"))

# looking for difference between failure type groups
## ------------------------------------------------------------------------
survdiff(Surv(hour, survive == 0) ~ fail_type, data = katrina)
pairwise_survdiff(Surv(hour, survive == 0) ~ fail_type, data = katrina)

# fixing the last hour to ensure there are no censored events
## ------------------------------------------------------------------------
katrina <- katrina %>% 
  mutate(hour2 = ifelse(hour == 48 & reason == 0, 49, hour), 
         inv_surv = ifelse(survive == 1, 0, 1))

# creating a hazard object and plot
## ------------------------------------------------------------------------
kat_haz <- with(katrina[katrina$reason != 0,], kphaz.fit(time = hour, status = inv_surv,strata =  reason))
kphaz.plot(kat_haz)

# looking at cummulative hazard plot
## ------------------------------------------------------------------------
ggsurvplot(kat_fit, fun = "cumhaz", palette = "grey")







## ------------------------------------------------------------------------
# katrina %>% 
#   select(1:8, survive, hour, reason) %>% 
# psych::describeBy(., .$reason, mat = TRUE) 
# 
# reas_stats <- katrina %>% 
#   select(1:8, survive, hour, reason) %>% 
#   split(.$reason) %>% 
#   map(summary)
# 
# stats_df<- do.call(rbind.data.frame, reas_stats) %>% 
#   cbind(reason = rep(0:4, 330/5), .) %>% 
#   select(reason, var = Var2, stats = Freq)
# 
# data.frame(Reduce(rbind, reas_stats))
