#--------------------------#
#             HW1          #
#    Survival Analysis     #
#                          #
#         Orange 4         #
#--------------------------#
library(survival)
library(survminer)
library(muhaz)
library(dplyr)

katrina <- read.csv("survival/data/katrina.csv", header = TRUE)

dim(katrina)
summary(katrina)
View(katrina)

# -------------------- creating survival object and summary statistics --------- #
with(katrina, Surv(time = hour, event = survive == 0))

kat_fit <- survfit(Surv(hour, survive == 0) ~ 1, data = katrina)

# 0.41 of all survived the hurricane, median survival time is hour 45.
kat_fit
summary(kat_fit)

# group by reason and remove rows with reason=0
kat_reason <- survfit(Surv(hour, survive == 0) ~ reason, data = katrina[katrina$reason!=0,])
kat_reason

# finding summary statistics of each reason group
## ------------------------------------------------------------------------

# percentage of pumps which survived failure

katrina %>% 
  group_by(survive) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))

# average of each reasons factors
katrina %>% 
  group_by(reason) %>% 
  summarize_at(3:8, funs(mean), na.rm=TRUE) 

# percentage of pumps in each failure type
katrina %>% 
  group_by(reason)%>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n))


# median surival time
kat_med <- survfit(Surv(hour, survive == 0) ~ 1, data = katrina)
which(kat_med$surv <= 0.5)
# median survival time is hour 45

# creating groups for failures as water or mechanical
katrina <- katrina %>% 
  mutate(fail_type = if_else(reason == 1 | reason == 3, "water",if_else(reason ==0,"survive", "mech")))
table(katrina$reason,katrina$fail_type)

kat_failtype <- survfit(Surv(hour, survive == 0) ~ fail_type, data = katrina[katrina$fail_type!="survive",])

# ----------------- plotting survival curves by group -----------------------


# plot the survival curve for all pumps
ggsurvplot(kat_fit, data = katrina, conf.int = FALSE, palette = "grey")

# plot the stratified survival curves by reason
ggsurvplot(kat_reason, data = katrina, conf.int = FALSE, palette = "RdBu")

# combine the two graphs
fit = list(kat_reason,kat_fit)
ggsurv <- ggsurvplot_combine(fit, data = katrina, conf.int = FALSE, palette = "hue",
                             title="Figure 1: Pump Survival Curves by Failure Reason During Hurricanes",
                             legend="top",xlab="Hours", xlim = c(0, 48), ylab="Survival Probability",
                             legend.title="Failure Reasons",legend.labs=c("Flood","Motor","Surge","Jammed","All Pumps"))
ggsurv$plot + theme_bw() + theme(legend.text=element_text(size=rel(0.85))) #plot.title = element_text(hjust = 0.5)

# plot the stratified survival curves by failed type: water or mech
ggsurv2 = ggsurvplot(kat_failtype, data = katrina, conf.int = FALSE, palette = "hue",
                     title="Figure 2: Pump Survival Curves by Binned Failures During Hurricanes",
                     legend="top",xlab="Hours", xlim = c(0, 48), ylab="Survival Probability",
                     legend.title="Binned Failures",legend.labs=c("Mechanical","Water"))
ggsurv2$plot + theme_bw() + theme(legend.text=element_text(size=rel(0.85)))

# ------------------ log-rank tests --------------------

# both unweighted and weighted tests show the curves are stat significantly different.
survdiff(Surv(hour, survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason!=0,])
survdiff(Surv(hour, survive == 0) ~ reason, rho = 1, data = katrina[katrina$reason!=0,])

# all pairs are stat significantly different from each other.
pairwise_survdiff(Surv(hour, survive == 0) ~ reason, rho = 0, data = katrina[katrina$reason!=0,])

# --------------------- hazard functions -------------------

# recode the time component to get credits for subjects survived in the end
katrina$hour2 <- ifelse(katrina$hour == 48 & katrina$survive == 1, 49, katrina$hour)

# kphaz.fit() has the same arguments as Surv()
kat_haz <- with(katrina, kphaz.fit(hour2, ifelse(survive==0,1,0)))


# plotting hazard function
kphaz.plot(kat_haz, main = "Figure 3: Hazard Function of the 48-hour Hurricane Period")





# cumulative hazard function
ggsurvplot(kat_fit, fun = "cumhaz", palette = "grey")

