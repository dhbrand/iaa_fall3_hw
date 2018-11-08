load(url("http://www.users.miamioh.edu/baileraj/research/database.RData")) 

####################################
# Perform an analysis on a dataset
# Dose-response dataset "clustering" 
# Dose-response data
####################################
library(dplyr)

idx <- final.data %>% group_by(ID) %>% count(ID) %>% filter(nn >= 4)

runs <- final.data %>% right_join(idx,'ID')      %>% group_by(ID) %>%
                   mutate(failure = n - obs) %>%
          do(mod = glm(cbind(obs,failure)~ r.dose  , data = .,family = "binomial")) %>%
                   filter(mod[10]$deviance > 0.11)
               
##########################################
results <- matrix(0,ncol=2,nrow=nrow(runs))
##########################################
#extract fit information
for (ii in 1:nrow(results)){
 results[ii,1:2]  <- coef(runs$mod[[ii]]) #coefficients
 #results[ii,3]    <- runs$mod[[ii]][10]$deviance  # deviance
}

clPairs(results)
              
# do this two different ways
# kmeans and mixture modeling

library(factoextra)
temp.results <- scale(results)

fviz_nbclust(temp.results, kmeans, method = "wss")
fviz_nbclust(temp.results, kmeans, method = "gap")
fviz_nbclust(temp.results, kmeans, method = "silhouette")

#Gap is junk, lets use WSS
kmeans_3<-kmeans(temp.results,3)
cmeans <- colMeans(results)
csd   <- apply(results,2,sd)

# we scaled everything now we have a problem
# how do we find the centroid

clusts <- kmeans_3$centers*matrix(csd,byrow=TRUE,nrow=3,ncol=2) + matrix(cmeans,byrow=TRUE,nrow=3,ncol=2)
b <- clusts[1,]
p1 <- 1/(1+exp(-b[1]-b[2]*d))
b <- clusts[2,]
p2 <- 1/(1+exp(-b[1]-b[2]*d))
b <- clusts[3,]
p3 <- 1/(1+exp(-b[1]-b[2]*d))
d <- seq(0,1,0.01)
plot(d,p1,xlab="Dose",ylab="Probability",type='l',col = 1,lwd=2,ylim=c(0,1))
lines(d,p2,col=2,lwd=2)
lines(d,p3,col=3,lwd=2)

# Mixture Modeling
library(mclust)
mm_bic <- mclustBIC(results)
plot(mm_bic)
mclustF <- Mclust(results,G=4)
plot(mclustF)
###########################################
#plot the regressions for my four clusters
d <- seq(0,1,0.01)
b <- mclustF$parameters$mean[,1]
p1 <- 1/(1+exp(-b[1]-b[2]*d))
b <- mclustF$parameters$mean[,2]
p2 <- 1/(1+exp(-b[1]-b[2]*d))
b <- mclustF$parameters$mean[,3]
p3 <- 1/(1+exp(-b[1]-b[2]*d))
b <- mclustF$parameters$mean[,4]
p4 <- 1/(1+exp(-b[1]-b[2]*d))

plot(d,p1,xlab="Dose",ylab="Probability",type='l',col = 1,lwd=2,ylim=c(0,1))
lines(d,p2,col=2,lwd=2)
lines(d,p3,col=3,lwd=2)
lines(d,p4,col=4,lwd=2)
mclustF$parameters$mean[1,]

##################################
#
# Now you do it with linear regression
# and the following dataset
#
##################################
library(fda) #not needed except for the dataset

gchart <- t(cbind(gw$hgtm,gw$hgtf))

plot(growth$age,gchart[1,],ylab = "CM",xlab="AGE")
plot(growth$age,gchart[2,],ylab = "CM",xlab="AGE")
age <- growth$age

temp <- as.numeric(t(gchart))
ID = rep(1:nrow(gchart),each=length(age))
ages = rep(age,nrow(gchart))
growth.df <- data.frame(ID = ID, CM = temp, ages=ages)

#now create the dataset!!! I am leaving some things empty
# so you can fix it and go to town
runs <- growth.df %>%  group_by(ID) %>%
          mutate(age.2 = age^2) %>% mutate(age.3 = age^3) %>%
          do(mod = lm(  ~    , data = .))
  



