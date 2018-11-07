library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(viridis)
library(triangle)
library(ks)
## -----------------------------------------------------------------------------
cost <- read_excel("sim_risk/data/Analysis_Data.xlsx",
  sheet = "Drilling Cost",
  skip = 2
)
future <- read_excel("sim_risk/data/Analysis_Data.xlsx",
  sheet = "Price Projections",
  skip = 2
)
str(cost)
str(future)

## -----------------------------------------------------------------------------
# rename columns
cost %<>% select(
  date = Date,
  crud_oil_cost = `U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)`,
  nat_gas_cost = `U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)`,
  dry_cost = `U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)`,
  crud_oil_ret = `Arithmetic Return - Crude Oil`,
  nat_gas_ret = `Arithmetic Return - Natural Gas`,
  dry_ret = `Arithmetic Return - Dry Well`
)

# adding year column for date
cost %<>% mutate(year = lubridate::year(date))

# long data and round values to 2 decimal places
long_dat <- cost %>%
  gather(var, value, -c(date, year)) %>%
  modify_at("value", as.numeric) %>%
  mutate(value = round(value, 2))

## -----------------------------------------------------------------------------
# look at time series plots for all values
ggplot(long_dat, aes(year, value, color = var)) +
  geom_line() +
  facet_wrap(~var, scales = "free") +
  theme_bw() +
  theme(legend.position = "None") +
  scale_color_viridis_d()

## -----------------------------------------------------------------------------
# look at histograms of all the values
# look at time series plots for all values
ggplot(long_dat, aes(value)) +
  geom_histogram(aes(fill = var, color = "grey")) +
  geom_density(aes(y = ..density..), size = 1) +
  facet_wrap(~var, scales = "free") +
  theme_bw() +
  theme(legend.position = "None") +
  scale_color_viridis_d()

## -----------------------------------------------------------------------------
# summary stats of vars
long_dat %>%
  group_by(var) %>%
  summarise_at("value", funs(mean, median, sd,
    q25 = quantile(., .25),
    q75 = quantile(., .75)
  ),
  na.rm = TRUE
  )



## -----------------------------------------------------------------------------
# creating dataframe of 48 x 2 oberstations for 1991-2006 cost and return
sub_dat <- cost %>%
  filter(year >= 1991 & year <= 2006)

combined_cost <- with(sub_dat, c(crud_oil_cost, nat_gas_cost, dry_cost))
combined_ret <- with(sub_dat, c(crud_oil_ret, nat_gas_ret, dry_ret))

combined <- data.frame(year = sub_dat$year, cost = combined_cost, ret = as.numeric(combined_ret))

## -----------------------------------------------------------------------------
# building the simulations

# first using normal distr in years 2006-2012
set.seed(303)
avg_06 <- dplyr::select(combined, year, cost) %>% filter(year == 2006) %>% pull %>% mean
avg <- mean(combined$ret)
stdev <- sd(combined$ret)
P19 <- rep(0, 1e4)
for (i in 1:1e4) {

  #
  P0 <- avg_06
  r <- rnorm(n = 1, mean = avg, sd = stdev)

  Pt <- P0 * (1 + r)

  for (j in 1:5) {
    r <- rnorm(n = 1, mean = avg, sd = stdev)
    Pt <- Pt * (1 + r)
  }

  # 2013-2015
  for (j in 1:3) {
    r <- rtriangle(1, a = 0.07, b = 0.22, c = 0.0917)
    Pt <- Pt * (1 - r)
  }

  # 2016-2018
  for (j in 1:3) {
    r <- rtriangle(1, a = 0.02, b = 0.06, c = 0.05)
    Pt <- Pt * (1 + r)
  }

  P19[i] <- Pt
}

mean(P19)
sd(P19)
c(quantile(P19, .1), quantile(P19, .9))

# computation of the standard error of the mean
sem <- sd(P19) / sqrt(length(P19))
# 95% confidence intervals of the mean
c(mean(P19) - 1.96 * sem, mean(P19) + 1.96 * sem)

ggplot(tibble(pred = P19), aes(pred)) +
  geom_histogram(fill = 'lightblue', color = 'blue') + 
  annotate("text", x = avg_06, 
           y = 1825, label = "2006 Cost", color = 'red', fontface = 2) + 
  annotate("segment", x = avg_06, xend = avg_06, y = 0, yend = 1800, colour = "red", size = 1) + 
  theme_bw() +
  labs(x = "Final Cost Possibilities (in thousands of dollars)", 
       y = "Frequency of Possibilites", 
       title = "2019 Cost Change Distribution using Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14)) +
  scale_x_continuous(limits = c(0, 17000))

hist(P19, breaks = 35, main = "2006-2018 Cost Change Distribution", xlab = "Final Value")
abline(v = 1000, col = "red", lwd = 2)
mtext("2006 Cost", at = avg_06, col = "red")



## -----------------------------------------------------------------------------
# kde estimation for 2007-2012

set.seed(303)

den_obj<- density(combined$ret, bw="SJ-ste")
P19_kde <- rep(0, 10000)
for (i in 1:10000) {

  #
  P0 <- avg_06
  r <- rkde(fhat = kde(combined$ret, h = den_obj$bw), n = 1)

  Pt <- P0 * (1 + r)


  for (j in 1:5) {
    r <- r <- rkde(fhat = kde(combined$ret, h = den_obj$bw), n = 1)
    Pt <- Pt * (1 + r)
  }

  # 2013-2015
  for (j in 1:3) {
    r <- rtriangle(1, a = 0.07, b = 0.22, c = 0.0917)
    Pt <- Pt * (1 - r)
  }

  # 2016-2018
  for (j in 1:3) {
    r <- rtriangle(1, a = 0.02, b = 0.06, c = 0.05)
    Pt <- Pt * (1 + r)
  }

  P19_kde[i] <- Pt
}

mean(P19_kde)
sd(P19_kde)
max(P19_kde)
# computation of the standard error of the mean
sem_kde <- sd(P19_kde) / sqrt(length(P19_kde))
# 95% confidence intervals of the mean
c(mean(P19_kde) - 1.96 * sem_kde, mean(P19_kde) + 1.96 * sem_kde)

ggplot(tibble(pred = P19_kde), aes(pred)) +
  geom_histogram(fill = 'lightblue', color = 'blue') + 
  annotate("text", x = avg_06, 
           y = 1775, label = "2006 Cost", color = 'red', fontface = 2) + 
  annotate("segment", x = avg_06, xend = avg_06, y = 0, yend = 1750, colour = "red", size = 1) + 
  theme_bw() +
  labs(x = "Final Cost Possibilities (in thousands of dollars)", 
       y = "Frequency of Possibilites", 
       title = "2019 Cost Change Distribution using KDE") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 14)) +
  scale_x_continuous(limits = c(0, 17000))

hist(P19_kde, breaks = 35, main = "2006-2018 Cost Change Distribution using KDE", xlab = "Final Value")
abline(v = 1000, col = "red", lwd = 2)
mtext("2006 Cost", at = mean(combined$cost), col = "red")

quantile(P19, .99)
quantile(P19_kde, .99)


# Bulding qqplot to check normality of years 2006-2012
set.seed(303)
avg <- mean(combined$ret)
stdev <- sd(combined$ret)
P5 <- rep(0, 10000)
for (i in 1:10000) {
  
  #
  P0 <- mean(combined$cost)
  r <- rnorm(n = 1, mean = avg, sd = stdev)
  
  Pt <- P0 * (1 + r)
  
  for (j in 1:5) {
    r <- rnorm(n = 1, mean = avg, sd = stdev)
    Pt <- Pt * (1 + r)
  }
  P5[i] <- Pt
}

qqnorm(P5)
qqline(P5)
car::qqPlot(P5)

ggplot(combined, aes(sample = cost)) +
  stat_qq(color = 'blue', size = 2) + 
  stat_qq_line(color = 'darkred', size = 1.5) +
  theme_bw() +
  labs(x = "Theoretical Quantiles from Normal Distribution", 
       y = "Simulated Costs (in thousands of dollars)", 
       title = "Quantile - Quantile Plot for Costs between 2007-2012") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 12))


ggplot(tibble(pred = P5), aes(sample = pred)) +
  geom_qq(color = 'blue') + 
  geom_qq_line(color = 'darkred')
# den_07_12 <- density(P5, bw="SJ-ste")
# den_07_12
#
# est_07_12<- rkde(fhat=kde(P5, h=den_07_12$bw), n=1000)
# hist(est_07_12, breaks=50, main='Estimated 5 Year Cost Change Distribution', xlab='Final Value')
#
# ##-----------------------------------------------------------------------------
# # 2019 estimation
# # using normal distribution
# set.seed(303)
# prev <- rtriangle(1000, a = 0.02, b = 0.06, c = 0.05)
# prev_avg <- mean(prev)
# prev_sd <- sd(prev)
# r <- rnorm(n=10000, mean=prev_avg, sd=prev_sd)
# P18 <- mean(y16_y18)
# P19 <- P18*(1+r)
#
# mean(P19)
# sd(P19)
#
# hist(P19, breaks=50, main='Estimated 1 Year Cost Change Distribution', xlab='Final Value', include.lowest = TRUE)
# abline(v = P18, col="red", lwd=2)
# mtext("2018 Value", at=1000, col="red")
#
# ##-----------------------------------------------------------------------------
# # kde estimation
# den_19 <- density(y16_y18, bw="SJ-ste")
# den_19
#
# est_19<- rkde(fhat=kde(y16_y18, h=den_19$bw), n=1000)
# hist(est_19, breaks=30, main='Estimated 1 Year Cost Change Distribution', xlab='Final Value')





# ##-----------------------------------------------------------------------------
# avg_yearly_cost <- mean(combined$cost)
# sd_yearly_cost <- sd(combined$cost)
#
# ##-----------------------------------------------------------------------------
# new_years <- c('y2007', 'y2008', 'y2009', 'y2010', 'y2011', 'y2012')
# for (year in 2:7) {
#   avg <- mean( sim[,year - 1])
#   std <- sd(sim[,year - 1])
#   sim[,year] <- rnorm(n = samp, mean = avg, sd = std)
#   prev_avg <- avg_yearly_cost[year - 1]
#   avg_yearly_cost <- c(avg_yearly_cost, prev_avg * (1 + mean(sim[, year])))
# }
# colnames(sim)[2:7] <- new_years
#
# ##-----------------------------------------------------------------------------
# new_years2 <- c('y2013', 'y2014', 'y2015')
# for (year in 8:10){
#   sim[,year] <- rtriangle(samp, a = 0.07, b = 0.22, c = 0.0917)
#   prev_avg <- avg_yearly_cost[year - 1]
#   avg_yearly_cost <- c(avg_yearly_cost, prev_avg * (1 - mean(sim[, year])))
# }
# colnames(sim)[8:10] <- new_years2
#
# ##-----------------------------------------------------------------------------
# new_years3 <- c('y2016', 'y2017', 'y2018')
# for (year in 11:13){
#   sim[,year] <- rtriangle(samp, a = 0.02, b = 0.06, c = .05)
#   prev_avg <- avg_yearly_cost[year - 1]
#   avg_yearly_cost <- c(avg_yearly_cost, prev_avg * (1 + mean(sim[, year])))
# }
# colnames(sim)[11:13] <- new_years3
#
#
# avg_yearly_cost
