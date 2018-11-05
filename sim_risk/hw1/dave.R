library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(viridis)
library(triangle)

cost <- read_excel("sim_risk/data/Analysis_Data.xlsx", 
                   sheet = 'Drilling Cost', 
                   skip = 2)
future <- read_excel('sim_risk/data/Analysis_Data.xlsx', 
                     sheet = 'Price Projections', 
                     skip = 2)
str(cost)
str(future)

# rename columns
cost %<>% select(date = Date, 
                 crud_oil_cost = `U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)`,
                 nat_gas_cost = `U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)`, 
                 dry_cost = `U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)`,
                 crud_oil_ret = `Arithmetic Return - Crude Oil`,
                 nat_gas_ret = `Arithmetic Return - Natural Gas`,
                 dry_ret = `Arithmetic Return - Dry Well`)

# adding year column for date
cost %<>% mutate(year = year(date))

# long data and round values to 2 decimal places
long_dat <- cost %>% 
  gather(var, value, -c(date, year)) %>% 
  modify_at('value', as.numeric) %>% 
  mutate(value = round(value, 2))
# look at time series plots for all values
ggplot(long_dat, aes(year, value, color = var)) + 
  geom_line() + 
  facet_wrap(~var, scales = 'free') +
  theme_bw() + 
  theme(legend.position = 'None') +
  scale_color_viridis_d()

# look at histograms of all the values
# look at time series plots for all values
ggplot(long_dat, aes(value)) + 
  geom_histogram(aes(fill = var, color = 'grey')) + 
  geom_density(aes(y=..density..), size = 1) + 
  facet_wrap(~var, scales = 'free') +
  theme_bw() + 
  theme(legend.position = 'None') +
  scale_color_viridis_d()

# summary stats of vars
long_dat %>% 
  group_by(var) %>% 
  summarise_at('value', funs(mean, median, sd, 
                             q25 = quantile(., .25), 
                             q75 = quantile(., .75)),
                             na.rm = TRUE)
sub_dat <- cost %>% 
  filter(year >= 1991 & year <= 2006)

combined_cost <- with(sub_dat, c(crud_oil_cost, nat_gas_cost, dry_cost))
combined_ret <- with(sub_dat, c(crud_oil_ret, nat_gas_ret, dry_ret))

combined <- data.frame(cost = combined_cost, ret = as.numeric(combined_ret)) 

# Introduction to Simulation #
set.seed(112358)
samp <- 10000
sim <- data.frame(y2006 = rnorm(n=samp, mean=mean(combined$ret), sd=sd(combined$ret)))

new_years <- c('y2007', 'y2008', 'y2009', 'y2010', 'y2011', 'y2012')
for (year in 2:7){
  avg <- mean( sim[,year - 1])
  std <- sd(sim[,year - 1])
  sim[,year] <- rnorm(n = samp, mean = avg, sd = std)
}
colnames(sim)[2:7] <- new_years

new_years2 <- c('y2013', 'y2014', 'y2015')
for (year in 8:10){
  sim[,year] <- rtriangle(samp, a = 0.07, b = -0.22, c = -0.0917)
}
colnames(sim)[8:10] <- new_years2

new_years3 <- c('y2016', 'y2017', 'y2018')
for (year in 11:13){
  avg <- mean(sim[, year - 1]) * 1.05
  sim[,year] <- rtriangle(samp, a = 0.02, b = 0.06, c = avg)
}
colnames(sim)[11:13] <- new_years3


avg <- mean(sim[, 1]) * (1-0.0917)
sim_test <- rtriangle(samp, a = 0.02, b = 0.06, c = avg)
