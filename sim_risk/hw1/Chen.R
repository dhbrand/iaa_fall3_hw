#--------------------------#
#             HW1          #
#        Simulation        #
#                          #
#       Chenhui Zhang      #
#--------------------------#
#install.packages("ks")
library(readxl)
library(dplyr)
library("car")
library(triangle)
library(ks)

# ---------------- reading and cleaning the data ------------- #
path = "sim_risk/data/Analysis_Data.xlsx"
excel_sheets(path)
drill = read_excel(path,sheet = "Drilling Cost",skip = 2)
View(drill)

colnames(drill) = c("Year","Cost_per_Crude_Oil","Cost_per_Natural_Gas","Cost_per_Dry_Well"
                    ,"Oil_Return","Gas_Return","Well_Return")

drill = drill %>% mutate(Year= year(Year))

valid = drill[drill$Year >=1991 & drill$Year<=2006,]
hist_return = as.numeric(c(valid$Oil_Return,valid$Gas_Return,valid$Well_Return))
hist_cost = c(valid$Cost_per_Crude_Oil,valid$Cost_per_Natural_Gas,valid$Cost_per_Dry_Well)

# --------------- normal distribution simulation ---------

# QQ PLOT - passed normality assumption
qqnorm(hist_return)
qqline(hist_return, col = "steelblue", lwd = 2)
qqPlot(hist_return)

avg_ret = mean(hist_return)
sd_ret = sd(hist_return)

# avg cost of 2016
cost0 = sum(valid[valid$Year == 2006,2:4])/3

# lecture solution: 
# take one random draw for each year's return and multiply them to get to one possible 2019 cost
# --> repeat this process for 10000 times to get all the simulated 2019 costs

cost_norm <- rep(0,10000)
set.seed(303)

for (ii in 1:10000){
  
  # initial cost in 2006
  cost_t = cost0
  
  for(j in 2007:2012){
    r <- rnorm(n=1, mean=avg_ret, sd=sd_ret)
    cost_t <- cost_t*(1+r)
  }
  for(j in 2013:2015){
    r <- rtriangle(n=1, a = -0.22, b = -0.07, c = -0.0917)
    cost_t <- cost_t*(1+r)
  }
  for(j in 2016:2018){
    r <- rtriangle(n=1, a = 0.02, b = 0.06, c = .05)
    cost_t <- cost_t*(1+r)
  }
  
  cost_norm[ii]=cost_t
}

# check out the 10000 possible values for 2019
cost_norm
mean(cost_norm)
sd(cost_norm)

hist(cost_norm, breaks=50, main='Year 2019 Cost Distribution (Norm)', xlab='Final Cost')

# ------------------- kernel density function -------------------- #

density.ret = density(hist_return, bw="SJ-ste")
cost_kde <- rep(0,10000)
set.seed(303)

for (ii in 1:10000){
  
  # initial cost in 2006
  cost_t = cost0
  
  for(j in 2007:2012){
    r <- rkde(fhat=kde(hist_return, h=density.ret$bw), n=1)
    cost_t <- cost_t*(1+r)
  }
  for(j in 2013:2015){
    r <- rtriangle(n=1, a = -0.22, b = -0.07, c = -0.0917)
    cost_t <- cost_t*(1+r)
  }
  for(j in 2016:2018){
    r <- rtriangle(n=1, a = 0.02, b = 0.06, c = .05)
    cost_t <- cost_t*(1+r)
  }
  
  cost_kde[ii]=cost_t
}

# check out the 10000 possible values for 2019
cost_kde
mean(cost_kde)
sd(cost_kde)

hist(cost_kde, breaks=50, main='Year 2019 Cost Distribution (KDE)', xlab='Final Cost')


