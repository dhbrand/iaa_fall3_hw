##############################
#                            #
#       Orange Team 4        #
#                            #
#        Homework 1          #
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

setwd("C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Survival/Data/")
katrina_raw = fread("katrina.csv")
hours = cbind('h1','h2','h3','h4','h5','h6','h7','h8','h9','h10','h11','h12','h13','h14','h15','h16','h17','h18','h19','h20','h21','h22','h23','h24','h25','h26','h27','h28','h29','h30','h31','h32','h33','h34','h35','h36','h37','h38','h39','h40','h41','h42','h43','h44','h45','h46','h47','h48')
katrina = subset(katrina_raw, select = !names(katrina_raw) %in% hours)

#Provide summary statistics for each type of pump station failure.

  #What percentage of pumps survived the hurricane?
mean(katrina$survive) #41.04% survived


  #break out pump failures
no_fail = katrina[which(katrina$reason == 0)] # 316/770 = 41.04%
fail    = katrina[which(katrina$reason != 0)] # 454/770 = 58.96%
flood   = katrina[which(katrina$reason == 1)] # 115/770 = 14.94%
motor   = katrina[which(katrina$reason == 2)] # 112/770 = 14.54%
surge   = katrina[which(katrina$reason == 3)] # 111/770 = 14.42%
jammed  = katrina[which(katrina$reason == 4)] # 116/770 = 15.06%


  #avg prices
summary(no_fail)  # 51% had backup pump to prevent flooding
                  # Average Age = 7.38; min = 6; max = 10.4
                  # 87.66% had bridge crane
                  # 61.39% had servomechanism
                  # 62.34% had trashrack
                  # Avg slope of 2.7; min = 0; max = 15
                  # Avg elevation of 3.538; min = 2; max = 6

summary(fail)     # 47.14% had backup pump to prevent flooding
                  # Average Age = 7.12; min = 5.3; max = 10
                  # 86.56% had bridge crane
                  # 58.37% had servomechanism
                  # 44.27% had trashrack
                  # Avg slope of 3.46; min = 0; max = 18
                  # Avg elevation of 3.28; min = 2; max = 6

summary(flood)    # Mean/Median hour fail = 26; min = 1; max = 48
                  # 41.7% had backup pump
                  # Average Age = 7.12; min = 6; max = 9.3
                  # 89.57% had bridge crane
                  # 45.22% had servomechanism
                  # 60.00% had trashrack
                  # Avg slope of 3.896; min = 0; max = 18
                  # Avg elevation of 3.313; min = 2; max = 6

summary(motor)    # Mean hour fail = 41; median = 45; min = 3; max = 48
                  # 46.43% had backup pump
                  # Average Age = 6.579; min = 5.3; max = 8.7
                  # 86.61% had bridge crane
                  # 60.71% had servomechanism
                  # 00.00% had trashrack
                  # Avg slope of 1.625; min = 0; max = 4
                  # Avg elevation of 3.554; min = 2; max = 6

summary(surge)    # Mean hour fail = 38; median = 42; min = 2; max = 48
                  # 53.15% had backup pump
                  # Average Age = 8.422; min = 7.4; max = 10
                  # 87.39% had bridge crane
                  # 83.78% had servomechanism
                  # 58.56% had trashrack
                  # Avg slope of 2.559; min = 0; max = 18
                  # Avg elevation of 3.495; min = 2; max = 6

summary(jammed)   # Mean hour fail = 21; median = 25; min = 5; max = 27
                  # 47.41% had backup pump
                  # Average Age = 6.386; min = 5.3; max = 8.5
                  # 82.76% had bridge crane
                  # 44.83% had servomechanism
                  # 57.76% had trashrack
                  # Avg slope of 5.672; min = 0; max = 18
                  # Avg elevation of 2.793; min = 2; max = 3

  # median failure time for each type
#flood  = 26
#motor  = 45
#surge  = 42
#jammed = 25



  #fit the survival and plot
all.fit    = survfit(Surv(hour, survive==0)~1, data = katrina)
fail.fit   = survfit(Surv(hour, survive==0)~1, data = fail)
flood.fit  = survfit(Surv(hour, survive==0)~1, data = flood)
motor.fit  = survfit(Surv(hour, survive==0)~1, data = motor)
surge.fit  = survfit(Surv(hour, survive==0)~1, data = surge)
jammed.fit = survfit(Surv(hour, survive==0)~1, data = jammed)

   # Combine survival curves
fit.list = list(
  all = all.fit, flood = flood.fit, motor = motor.fit, surge = surge.fit, jammed = jammed.fit
)
ggsurv = ggsurvplot(fit.list, data = katrina, censor = FALSE,
                     combine = TRUE, keep.data = TRUE)
ggsurv

  # find if survival for any are statistically different
survdiff(Surv(hour, survive==0)~reason,data=fail)
pairwise_survdiff(Surv(hour, survive==0)~reason,data=fail)


  # group water and mechanical failures
water = rbind(surge,flood)
mech  = rbind(jammed,motor)
water.fit = survfit(Surv(hour, survive==0)~1, data = water)
mech.fit  = survfit(Surv(hour, survive==0)~1, data = mech)

fit.list = list(
  all = all.fit, water = water.fit, mechanical = mech.fit
)
ggsurv = ggsurvplot(fit.list, data = katrina, censor = FALSE,
                    combine = TRUE, keep.data = TRUE)
ggsurv

survdiff(Surv(hour, survive == 0) ~ fail_type, data = katrina)
pairwise_survdiff(Surv(hour, survive == 0) ~ fail_type, data = katrina)


  # fix hour for censoring and create hazardplots
katrina$hour2 = ifelse(katrina$hour == 48 & katrina$reason == 0, 49, katrina$hour)
katrina$inv_surv = ifelse(katrina$survive == 1, 0, 1)

kat_haz = with(katrina, kphaz.fit(time = hour2, status = inv_surv))
kphaz.plot(kat_haz)
ggsurvplot(all.fit, fun = "cumhaz")













################################################
############### Unnecessary ####################
################################################
#   #median survival times for each failure type
# hours = cbind('h1','h2','h3','h4','h5','h6','h7','h8','h9','h10','h11','h12','h13','h14','h15','h16','h17','h18','h19','h20','h21','h22','h23','h24','h25','h26','h27','h28','h29','h30','h31','h32','h33','h34','h35','h36','h37','h38','h39','h40','h41','h42','h43','h44','h45','h46','h47','h48')
#     #motor
# prev=0
# tot=0
# for (i in hours){
#   #print(paste(i,sum(is.na(motor[[i]]))-prev))
#   tot=tot+sum(is.na(motor[[i]]))-prev
#   prev=sum(is.na(motor[[i]]))
# }
# print(tot)
# 
#     #jammed
# prev=0
# tot=0
# for (i in hours){
#   #print(paste(i,sum(is.na(jammed[[i]]))-prev))
#   tot=tot+sum(is.na(jammed[[i]]))-prev
#   prev=sum(is.na(jammed[[i]]))
# }
# print(tot)
# 
#     #surge
# prev=0
# tot=0
# for (i in hours){
#   #print(paste(i,sum(is.na(surge[[i]]))-prev))
#   tot=tot+sum(is.na(surge[[i]]))-prev
#   prev=sum(is.na(surge[[i]]))
# }
# print(tot)
# 
#     #flood  -- h29 is median failure
# prev=0
# tot=0
# for (i in hours){
#   #print(paste(i,sum(is.na(flood[[i]]))-prev))
#   tot=tot+sum(is.na(flood[[i]]))-prev
#   if (tot>(115/2)){
#     print(i)
#   } 
#   prev=sum(is.na(flood[[i]]))
# }
# #print(tot)



