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
cost %<>% mutate(year = year(date))

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
sub_dat <- cost %>%
  filter(year >= 1991 & year <= 2006)

combined_cost <- with(sub_dat, c(crud_oil_cost, nat_gas_cost, dry_cost))
combined_ret <- with(sub_dat, c(crud_oil_ret, nat_gas_ret, dry_ret))

combined <- data.frame(cost = combined_cost, ret = as.numeric(combined_ret))

## -----------------------------------------------------------------------------
# building the simulations
samp <- 10000
sim <- data.frame(y2006 = rnorm(n = samp, mean = mean(combined$ret), sd = sd(combined$ret)))


## -----------------------------------------------------------------------------
# based on labarr's code
set.seed(303)
avg <- mean(combined$ret)
stdev <- sd(combined$ret)
P19 <- rep(0, 10000)
for (i in 1:10000) {

  #
  P0 <- mean(combined$cost)
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

# computation of the standard error of the mean
sem <- sd(P19) / sqrt(length(P19))
# 95% confidence intervals of the mean
c(mean(P19) - 2 * sem, mean(P19) + 2 * sem)

hist(P19, breaks = 35, main = "2006-2018 Cost Change Distribution", xlab = "Final Value")
abline(v = 1000, col = "red", lwd = 2)
mtext("2006 Cost", at = mean(combined$cost), col = "red")



## -----------------------------------------------------------------------------
# kde estimation for 2007-2012

set.seed(303)
P19_kde <- rep(0, 10000)
for (i in 1:10000) {

  #
  P0 <- mean(combined$cost)
  r <- rkde(fhat = kde(combined$ret, h = den_07_12$bw), n = 1)

  Pt <- P0 * (1 + r)


  for (j in 1:5) {
    r <- r <- rkde(fhat = kde(combined$ret, h = den_07_12$bw), n = 1)
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
# computation of the standard error of the mean
sem_kde <- sd(P19_kde) / sqrt(length(P19_kde))
# 95% confidence intervals of the mean
c(mean(P19_kde) - 2 * sem_kde, mean(P19_kde) + 2 * sem_kde)


hist(P19_kde, breaks = 35, main = "2006-2018 Cost Change Distribution using KDE", xlab = "Final Value")
abline(v = 1000, col = "red", lwd = 2)
mtext("2006 Cost", at = mean(combined$cost), col = "red")

quantile(P19, .99)
quantile(P19_kde, .99)

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
