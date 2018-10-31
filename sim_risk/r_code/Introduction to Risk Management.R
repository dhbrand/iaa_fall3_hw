#------------------------------#
#     Introduction to Risk     #
#           Management         #
#                              #
#         Dr Aric LaBarr       #
#------------------------------#

# Needed Libraries for Analysis #
library(graphics)
library(triangle)
library(ks)
library(MASS)
library(rgl)

# Monte Carlo Simulation #
simulation.size <- 10000

Units <- rtriangle(simulation.size, a=500, b=2000, c=1500)
Var.Cost <- 1 + 0.004*Units + rnorm(simulation.size, mean=0, sd=sqrt(0.8))
Fixed.Cost <- 2500
Price <- rtriangle(simulation.size, a=8, b=11, c=10)

Net.Revenue <- (Price - Var.Cost)*Units - Fixed.Cost

hist(Net.Revenue, breaks=50, main='Sampling Distribution of Net Revenue', xlab='Net Revenue')

# Tornado plot
net.rev <- function(p,vc,q,fc){rev <- (p-vc)*q-fc; return(rev)}

data <- matrix(c(1000,-500,750,-1500,1500,-1500,1500,-2000), ncol = 4)
rownames(data) <- c('BEST','WORST')                     
colnames(data) <- c('FC', 'VC', 'P','Q')      
x <- seq(-2000,2000, length=500)                          

barplot(data[1,], horiz = T, las=1, xlim = c(-2000,2000), xaxt='n', ylab = '',
        beside=T, col=c('springgreen'))
barplot(data[2,], horiz = T, las=1, xlim = c(-2000,2000), xaxt='n', ylab = '',
        beside=T, col=c('indianred2'), add = TRUE)
axis(1, at=pretty(x),  lab=paste0("$",pretty(x)), las=TRUE)


# Kernel Estimate of Net Revenues #
D.Net.Revenue <- density(Net.Revenue)

hist(Net.Revenue, breaks=50, prob=TRUE, main='Sampling Distribution of Net Revenue', xlab='Net Revenue')
lines(D.Net.Revenue, col="blue", lwd=2)

# Kernel Estimate of Units to Price #
D.Price.Units <- kde2d(Price, Units)

persp3d(D.Price.Units, col="blue", xlab="Price", ylab="Units")

# Kernel Estimate of Units to Variable Cost #
D.VC.Units <- kde2d(Var.Cost, Units)

persp3d(D.VC.Units, col="blue", xlab="Variable Cost", ylab="Units")
