#--------------------------#
#     Main Concepts of     #
#        Simulation        #
#                          #
#         Dr LaBarr        #
#--------------------------#

# Needed Libraries for Analysis #
library(graphics)
library(ks)

# Introduction to Simulation #
set.seed(112358)
r <- rnorm(n=10000, mean=0.0879, sd=0.1475)
P0 <- 1000
P1 <- P0*(1+r)

mean(P1)
sd(P1)

hist(P1, breaks=50, main='One Year Value Distribution', xlab='Final Value')
abline(v = 1000, col="red", lwd=2)
mtext("Initial Inv.", at=1000, col="red")

# Distribution Selection - Kernel Estimation #
Density.P1 <- density(P1, bw="SJ-ste")
Density.P1

Est.P1 <- rkde(fhat=kde(P1, h=Density.P1$bw), n=1000)
hist(Est.P1, breaks=50, main='Estimated One Year Value Distribution', xlab='Final Value')

# Multiple Input Probability Distributions #
P30 <- rep(0,10000)
for(i in 1:10000){
  P0 <- 1000
  r <- rnorm(n=1, mean=0.0879, sd=0.1475)
  
  Pt <- P0*(1 + r)

  for(j in 1:29){
    r <- rnorm(n=1, mean=0.0879, sd=0.1475)
    Pt <- Pt*(1+r)
  }
  P30[i] <- Pt
}

mean(P30)
sd(P30)

hist(P30, breaks=50, main='30 Year Value Distribution', xlab='Final Value')
abline(v = 1000, col="red", lwd=2)
mtext("Initial Inv.", at=1000, col="red")

# Correlated Inputs #
Value.r <- rep(0,10000)
R <- matrix(data=cbind(1,-0.2, -0.2, 1), nrow=2)
U <- t(chol(R))
Perc.B <- 0.7
Perc.S <- 0.3
Initial <- 1000

standardize <- function(x){
  x.std = (x - mean(x))/sd(x)
  return(x.std)
}

destandardize <- function(x.std, x){
  x.old = (x.std * sd(x)) + mean(x)
  return(x.old)
}

for(j in 1:10000){
  
  S.r <- rnorm(n=30, mean=0.0879, sd=0.1475)
  B.r <- rnorm(n=30, mean=0.04, sd=0.07)
  Both.r <- cbind(standardize(S.r), standardize(B.r))
  SB.r <- U %*% t(Both.r)
  SB.r <- t(SB.r)
  
  final.SB.r <- cbind(destandardize(SB.r[,1], S.r), destandardize(SB.r[,2], B.r))
  
  Pt.B <- Initial*Perc.B
  Pt.S <- Initial*Perc.S
  for(i in 1:30){
    Pt.B <- Pt.B*(1 + final.SB.r[i,2])
    Pt.S <- Pt.S*(1 + final.SB.r[i,1])
  }
  Value.r[j] <- Pt.B + Pt.S
}

hist(Value.r, breaks=50, main='30 Year Value Distribution', xlab='Final Value')
abline(v = 1000, col="red", lwd=2)
mtext("Initial Inv.", at=1000, col="red")


# Choleski Decomp Testing #
for(j in 1:10000){
  
  S.r <- runif(n=30, min=1, max=5)
  B.r <- rnorm(n=30, mean=0.04, sd=0.07)
  Both.r <- cbind(standardize(S.r), standardize(B.r))
  SB.r <- U %*% t(Both.r)
  SB.r <- t(SB.r)
  
  final.SB.r <<- cbind(destandardize(SB.r[,1], S.r), destandardize(SB.r[,2], B.r))
  
  Pt.B <- Initial*Perc.B
  Pt.S <- Initial*Perc.S
  for(i in 1:30){
    Pt.B <- Pt.B*(1 + final.SB.r[i,2])
    Pt.S <- Pt.S*(1 + final.SB.r[i,1])
  }
  Value.r[j] <- Pt.B + Pt.S
}

max(final.SB.r[,1])
min(final.SB.r[,1])

