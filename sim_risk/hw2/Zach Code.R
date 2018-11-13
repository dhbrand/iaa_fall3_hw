#--------------------------#
#           HW2            #
#       Simulation         #
#                          #
#     Zach Wasielewski     #
#--------------------------#

library(readxl)
library(dplyr)
library(car)
library(triangle)
library(ks)
library(lubridate)
library(ggplot2)

## turn off exponential notation
  options(scipen = 999)

## Miscellaneous Functions Used
  standardize <- function(x){
    x.std = (x - mean(x))/sd(x) #standardizing data due to correlation matrix being 1 on the diagonal
    return(x.std)
  }
  
  destandardize <- function(x.std, x){
    x.old = (x.std * sd(x)) + mean(x) #destandardizing data to be non-standardized and put results in real terms
    return(x.old)
  }

## Reading and Cleaning Data
  path = "C:/Users/zacha/Documents/MSA/Fall 2018/Analytic Methods/Simulation/Data/Analysis_Data.xlsx"
  excel_sheets(path)
  drill = read_excel(path,sheet = "Drilling Cost",skip = 2)
  colnames(drill) = c("Year","Cost_per_Crude_Oil","Cost_per_Natural_Gas","Cost_per_Dry_Well"
                      ,"Oil_Return","Gas_Return","Well_Return")
  drill = drill %>% mutate(Year= year(Year))
  
  price_proj = read_excel(path,sheet = "Price Projections",skip = 2)
  colnames(price_proj) = c("Year","High_Price","Low_Price","Reference_Price")



## Dry Well Cost Simulation:
  cost_dry = rep(0,10000)
  set.seed(303)
  
  for (ii in 1:10000){
    acre_cost = 960*rnorm(n=1, mean=600, sd=50)
    seismic_cost = 43000*rnorm(n=1, mean=3, sd=0.35)
    staff_cost = rtriangle(n=1, a = 172000, b = 279500, c = 215000)
    total_cost = acre_cost+seismic_cost+staff_cost
    
    cost_dry[ii] = total_cost
  }
  hist(cost_dry)



## Wet Well Cost Simulation:
  
#setup components of simulation
  num=10000
  cost_wet = rep(0,num)
  rev_wet = rep(0,num)
  set.seed(404)
  R <- matrix(data=cbind(1, 0.64, 0.64, 1), nrow=2)
  U <- t(chol(R))
  high_vec = price_proj$High_Price[which(price_proj$Year >= 2019 & price_proj$Year <= 2033)]
  low_vec = price_proj$Low_Price[which(price_proj$Year >= 2019 & price_proj$Year <= 2033)]
  mid_vec = price_proj$Reference_Price[which(price_proj$Year >= 2019 & price_proj$Year <= 2033)]

  
#random generate 10000 correlated initial and decline rates
  #initial rates
    init_rate = rlnorm(n=num, meanlog=6, sdlog=0.28)
    decline_rate = runif(n=num, min=0.15, max=0.32)
  #correlate initial rates
    Both.r <- cbind(standardize(init_rate), standardize(decline_rate)) # standardizing random draws
    Corr.r <- U %*% t(Both.r) # multiplying U by the matrix with both standardized random draws
    Corr.r <- t(Corr.r) # transpose matrix
    final.Corr.r <<- cbind(destandardize(Corr.r[,1], init_rate), destandardize(Corr.r[,2], decline_rate)) # de-standardize
  #set initial rates for 2019
    init_rate_all = final.Corr.r[,1] # first column is initial rates
    decline_rate_all = final.Corr.r[,2] # second column is decline rates (CORRELATED!!)
  

#calculate costs and revenues for each year (15 year forecast)
  for (ii in 1:10000){
    ## Year 0 Costs:
      acre_cost = 960*rnorm(n=1, mean=600, sd=50)
      seismic_cost = 43000*rnorm(n=1, mean=3, sd=0.35)
      staff_cost = rtriangle(n=1, a = 172000, b = 279500, c = 215000)
      setup_cost = rnorm(n=1, mean=390000, sd=50000)
      init_cost = acre_cost+seismic_cost+staff_cost+setup_cost
    
    ## Year 1 & Beyond
    #set initial rates
      royalty_rate = rnorm(n=1, mean=0.25, sd=0.02) #stays the same each year
      init_rate = init_rate_all[ii] #updates every year based on end_rate equation
      decline_rate = decline_rate_all[ii] #stays the same each year
    
    #build revenue & cost compounding for each year
      total_cost = init_cost
      total_rev =0
    
    #for loop for all 15 years
    for (jj in 1:15){
      #find end rate and oil volume for this year
        end_rate = (1-decline_rate)*init_rate
        oil_vol= 365*(init_rate+end_rate)/2
        
      #obtain price from sheet (high, low, mean could be moved outside for loop for time saving)
        oil_price = rtriangle(n=1, a=low_vec[jj], b=high_vec[jj], c=mid_vec[jj]) 
        
      #calculate revenue
        revenue= oil_vol*oil_price
        
      #calculate royalties on revenue
        NRI = revenue*royalty_rate
        new_rev = revenue-NRI
        
      #calculate operating costs
        op_cost_per_barrel = rlnorm(n=1, mean=2.25, sd=0.3)
        op_cost=oil_vol*op_cost_per_barrel
        
      #calculate tax
        tax = new_rev*0.046
        
      #final cost & revenue for year
        cost=tax+NRI+op_cost
        total_cost=total_cost+cost
        final_rev = revenue-cost
        total_rev=total_rev+final_rev
        
      #reset initial
        init_rate = end_rate
    }
    cost_wet[ii]=total_cost
    rev_wet[ii]=total_rev-total_cost
  }
  
  
  hist(rev_wet, 
       breaks=100,
       main="Histogram of Wet Well Revenue Possibilities",
       xlab="Revenue",
       border="blue",
       col="grey",
       xlim=c(-5000000, 20000000),
       las=1,
  )
  axis(side=1, at=seq(-5000000, 20000000, 5000000))
  abline(v=mean(rev_wet), col="red")
