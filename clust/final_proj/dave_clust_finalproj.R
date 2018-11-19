library(splines)
library(mclust)


times <- seq(1,295)/100 # Observations in 1/100th of a second X <- bs(times,intercept=TRUE,df=60) #create a spline to
#model the data betas <- matrix(0,ncol=60,nrow = 6792)
########################################################### # run a linear regression on each data set
# here I am manipulating my data you I can cluster ########################################################### for (ii in 1:6792){
temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
betas[ii,] <- coefficients(temp) }
cdata <- cbind(final_data[,1:5],betas)
#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE) cdata$ASTHMA <- as.numeric(cdata$EVER_SMOKE) cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)
