#############################
#                           #
#     MSA Class of 2019     #
#                           #
#    Survival Analysis:     #
#   Cox Regression Models   #
#                           #
#      Matthew Austin       #
#                           #
#############################

# need these packages
library(survival)
library(survminer)
library(visreg)

# load data
recid <- read.csv("/your/data/folder/recid.csv", header = TRUE)
# create an ID variable: we'll need it later and you should probably always
# have one for survival analysis anyway 
recid$ID <- 1:nrow(recid)

# fit proportional hazards model using coxph()
# same structure as everything else
fit <- coxph(Surv(week, arrest == 1) ~ fin + age + race + wexp + mar +
               paro + prio, data = recid)
summary(fit)

# plot survival curve
ggsurvplot(survfit(fit), data = recid, legend = "none", break.y.by = 0.1,
           xlab = "week", ylab = "survival probability")
# who is this reference population?
fit$means

# create a new dataset for some comparison of interest
newdata <- data.frame(fin = c(1, 0), age = 30, race = 0, wexp = c(1, 0),
                      mar = 0, paro = 0, prio = c(0, 4))
# subject-specific curves
ggsurvplot(survfit(fit, newdata), data = newdata, break.y.by = 0.1,
           palette = c("purple", "black"), ylab = "survival probability",
           xlab = "week", legend.labs = c("1", "2"), legend.title = "subject")