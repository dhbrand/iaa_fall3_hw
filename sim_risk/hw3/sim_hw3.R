#--------------------------#
#             HW3          #
#        Simulation        #
#                          #
#       Chenhui Zhang      #
#--------------------------#

# repeat this process for 10000 times:
# for the number of wells simulated from a uniform distribution:
#   draw from random truncated normal distribution for hydrocarbon and reservior risks
#   calculate probability of being a producing well
#   use bernoulli distribution to decide if it's a dry or producing well
  
#install.packages("truncnorm")
library(truncnorm)

### ------------------- Simulation --------------------- ###
hydro = vector() # a vector containing all the simulated probabilities of having hydrocarbon in a well 
resv = vector() # a vector containing all the simulated probabilities of having a developed reservior for a well 
prop = vector() # a vector containing all the proportions of producing wells

for (ii in 1:10000){
  # number of wells is simulated from a uniform distribution
  num_wells = floor(runif(1,10,30)) 
  outcome_count = vector()
  for (jj in 1:num_wells){
    # draw from random truncated normal distribution for both hydrocarbon and reservior risks
    hydrocarbon = rtruncnorm(1, a=0, b=1, mean = 0.99, sd = 0.05)
    hydro = cbind(hydro,hydrocarbon)
    reservior = rtruncnorm(1,a=0, b=1, mean = 0.8, sd = 0.1)
    resv = cbind(resv,reservior)
    seal = 1
    struc = 1
    # calculate the probability of being a producing well
    prob_PW = hydrocarbon*reservior*seal*struc
    # use bernoulli distribution (size =1 and n=1) to decide if it's a dry or producing well
    outcome = rbinom(1, 1, prob_PW)
    outcome_count = cbind(outcome_count,outcome)
  }
  prop = cbind(prop, sum(outcome_count)/num_wells)
}

# check out the distribution
hist(hydro)
hist(resv)
hist(prop)

# calculate VaR and ES
VaR = quantile(prop,0.05) 
# 0.5882 for 1000 simulations; 0.5833 for 10000 simulations
tail_values = prop[prop <= VaR]
ES = mean(tail_values) 
# 0.52 for 1000 simulations; 0.5312 for 10000 simulations

qqnorm(prop);qqline(prop, col = 2)
# shapiro.test(prop)

# --------------- some exploration...-----------------
# truncated normal shape
hydrocarbon = rtruncnorm(100000, a=0, b=1, mean = 0.99, sd = 0.05)
hist(hydrocarbon)

# bernoulli distribution
hist(rbinom(10000, 1, 0.78))
