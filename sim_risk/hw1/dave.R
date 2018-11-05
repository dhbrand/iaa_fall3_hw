library(tidyverse)
library(readxl)
library(magrittr)
library(lubridate)
library(viridis)

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



