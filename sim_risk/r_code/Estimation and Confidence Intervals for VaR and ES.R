#----------------------------------#
#     Estimation and Confidence    #
#      Intervals for VaR & ES      #
#                                  #
#           Dr Aric LaBarr         #
#----------------------------------#

# Needed Libraries for Analysis #
library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)


# Load Stock Data & Calculate Returns#
tickers = c("AAPL", "MSFT")

getSymbols(tickers)

stocks <- cbind(last(AAPL[,4], '500 days'), last(MSFT[,4], '500 days'))

stocks$msft_r <- ROC(stocks$MSFT.Close)
stocks$aapl_r <- ROC(stocks$AAPL.Close)

# Stock Information #
msft.holding <- 1700
aapl.holding <- 2500
VaR.percentile <- 0.01

# Calculate Needed Variances and Covariances #
var.msft <- var(stocks$msft_r, na.rm=TRUE)
var.aapl <- var(stocks$aapl_r, na.rm=TRUE)
cov.m.a <- cov(stocks$msft_r, stocks$aapl_r, use="pairwise.complete.obs")
cor.m.a <- cor(stocks$msft_r, stocks$aapl_r, use="pairwise.complete.obs")

# Calculate Current Price of Holdings (Portfolio) #
msft.p <- as.vector(stocks$MSFT.Close[length(stocks$MSFT.Close)])
aapl.p <- as.vector(stocks$AAPL.Close[length(stocks$AAPL.Close)])

# Delta-Normal Calculations #
AAPL.inv <- 100000
MSFT.inv <- 200000

VaR.DN.AAPL <- AAPL.inv*qnorm(VaR.percentile)*sqrt(var.aapl)
dollar(VaR.DN.AAPL)

var.port <- (MSFT.inv/(MSFT.inv+AAPL.inv))^2*var.msft + (AAPL.inv/(MSFT.inv+AAPL.inv))^2*var.aapl + 2*(AAPL.inv/(MSFT.inv+AAPL.inv))*(MSFT.inv/(MSFT.inv+AAPL.inv))*cov.m.a
var.port

VaR.DN.port <- (AAPL.inv+MSFT.inv)*qnorm(VaR.percentile)*sqrt(var.port)
dollar(VaR.DN.port)

# Confidence Intervals for Portfolio Standard Deviation #
sigma.low <- sqrt(var.port*(length(stocks$AAPL.Close)-1)/qchisq((1-(VaR.percentile/2)),length(stocks$AAPL.Close)-1) )
sigma.up <- sqrt(var.port*(length(stocks$AAPL.Close)-1)/qchisq((VaR.percentile/2),length(stocks$AAPL.Close)-1) )

# Calculate Portfolio's Value at Risk, VaR CI, and Conditional Value at Risk #
VaR.DN.port <- (AAPL.inv+MSFT.inv)*qnorm(VaR.percentile)*sqrt(var.port)
VaR.L <-  (AAPL.inv+MSFT.inv)*qnorm(VaR.percentile)*(sigma.low)
VaR.U <- (AAPL.inv+MSFT.inv)*qnorm(VaR.percentile)*(sigma.up)

dollar(VaR.L)
dollar(VaR.DN.port)
dollar(VaR.U)

ES.DN.port <- (0 - sqrt(var.port)*exp(-(qnorm(VaR.percentile)^2)/2)/(VaR.percentile*sqrt(2*pi)))*(AAPL.inv+MSFT.inv)
dollar(ES.DN.port)

# Historical Simulation Approach #
head(order(stocks$aapl_r), 6)

VaR.H.AAPL <- AAPL.inv*stocks$aapl_r[head(order(stocks$aapl_r), 6)[6]]
VaR.H.AAPL

stocks$port_v <- MSFT.inv*stocks$msft_r + AAPL.inv*stocks$aapl_r
write.csv(stocks, file = "C:/Users/adlabarr/Documents/Courses/IAA/Simulation and Risk/Data/stocks.csv")

head(order(stocks$port_v), 6)

VaR.H.port <- stocks$port_v[order(stocks$port_v)[6]]
dollar(as.numeric(VaR.H.port))

ES.H.port <- mean(stocks$port_v[head(order(stocks$port_v), 5)])
dollar(as.numeric(ES.H.port))

stocks_stressed <- cbind(AAPL[, 4], MSFT[, 4])
stocks_stressed$msft_r <- ROC(stocks_stressed$MSFT.Close)
stocks_stressed$aapl_r <- ROC(stocks_stressed$AAPL.Close)
stocks_stressed$port_v <- MSFT.inv*stocks_stressed$msft_r + AAPL.inv*stocks_stressed$aapl_r

stocks_stressed$ma <- SMA(stocks_stressed$port_v, 500)
stocks_stressed <- stocks_stressed[seq(order(stocks_stressed$ma)[1]-499,order(stocks_stressed$ma)[1],1)]

head(order(stocks_stressed$port_v), 6)
stressed.VaR.H.port <- stocks_stressed$port_v[order(stocks_stressed$port_v)[6]]
dollar(as.numeric(stressed.VaR.H.port))

i <- 1:500
lambda <- 0.995
stocks$weights <- (1-lambda)*lambda^(500-i)
stocks[head(order(stocks$port_v), 6)]

# Monte Carlo Simulation Approach #
n.simulations <- 10000
R <- matrix(data=cbind(1,cor.m.a, cor.m.a, 1), nrow=2)
U <- t(chol(R))

msft.r <- rnorm(n=n.simulations, mean=0, sd=sqrt(var.msft))
aapl.r <- rnorm(n=n.simulations, mean=0, sd=sqrt(var.aapl))
Both.r <- cbind(msft.r, aapl.r)
port.r <- U %*% t(Both.r)
port.r <- t(port.r)
  
value <- msft.holding*(exp(msft.r + log(msft.p))) + aapl.holding*(exp(aapl.r + log(aapl.p)))
value.change = value - (msft.holding*msft.p + aapl.holding*aapl.p)

VaR <- quantile(value.change, VaR.percentile, na.rm=TRUE)
VaR.label <- dollar(VaR)

hist(value.change/1000, breaks=50, main='1 Day Value Change Distribution', xlab='Value Change', col="lightblue")
breaks = c(-20, -10, 0, 10, 20, 30)
axis(1, at = breaks, labels = paste("$", breaks, "K", sep = ""))
abline(v = VaR/1000, col="red", lwd=2)
mtext(paste("Value at Risk",VaR.label, sep=" = "), at=VaR/1000, col="red")

ES <- mean(value.change[value.change < VaR], na.rm=TRUE)
dollar(ES)

h <- hist(value.change/1000, breaks=50, plot=FALSE)
cuts <- cut(h$breaks, c(-Inf, VaR/1000, Inf))
plot(h, col = cuts, main='1 Day Value Change Distribution', xlab='Value Change')
breaks = c(-20, -10, 0, 10, 20, 30)
axis(1, at = breaks, labels = paste("$", breaks, "K", sep = ""))
abline(v = VaR/1000, col="red", lwd=2)
mtext(paste("Value at Risk",VaR.label, sep=" = "), at=VaR/1000, col="red")


# Confidence Intervals for Value at Risk & Expected Shortfall - Bootstrap Approach #
n.bootstraps <- 1000
sample.size <- 1000

VaR.boot <- rep(0,n.bootstraps)
ES.boot <- rep(0,n.bootstraps)
for(i in 1:n.bootstraps){
  bootstrap.sample <- sample(value.change, size=sample.size)
  VaR.boot[i] <- quantile(bootstrap.sample, VaR.percentile, na.rm=TRUE)
  ES.boot[i] <- mean(bootstrap.sample[bootstrap.sample < VaR.boot[i]], na.rm=TRUE)
}

VaR.boot.U <- quantile(VaR.boot, 0.975, na.rm=TRUE)
VaR.boot.L <- quantile(VaR.boot, 0.025, na.rm=TRUE)
dollar(VaR.boot.L)
dollar(VaR)
dollar(VaR.boot.U)

hist(value.change/1000, breaks=50, main='1 Day Value Change Distribution', xlab='Value Change', col="lightblue")
breaks = c(-20, -10, 0, 10, 20)
axis(1, at = breaks, labels = paste("$", breaks, "K", sep = ""))
abline(v = VaR/1000, col="red", lwd=2)
mtext(paste("Value at Risk",VaR.label, sep=" = "), at = VaR/1000, col="red")
abline(v = VaR.boot.L/1000, col="blue", lwd=2, lty="dashed")
abline(v = VaR.boot.U/1000, col="blue", lwd=2, lty="dashed")

ES.boot.U <- quantile(ES.boot, 0.975, na.rm=TRUE)
ES.boot.L <- quantile(ES.boot, 0.025, na.rm=TRUE)
dollar(ES.boot.L)
dollar(ES)
dollar(ES.boot.U)

