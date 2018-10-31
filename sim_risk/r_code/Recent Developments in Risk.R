#----------------------------------#
#    Recent Developments in Risk   #
#                                  #
#           Dr Aric LaBarr         #
#----------------------------------#

# Needed Libraries for Analysis #
library(graphics)
library(quantmod)
library(TTR)
library(ks)
library(scales)
library(fExtremes)

# Load Stock Data & Calculate Returns#
tickers = c("AAPL", "MSFT")

getSymbols(tickers)

stocks <- cbind(last(AAPL[,4], '500 days'), last(MSFT[,4], '500 days'))

stocks$msft_r <- ROC(stocks$MSFT.Close)
stocks$aapl_r <- ROC(stocks$AAPL.Close)

AAPL.inv <- 100000
MSFT.inv <- 200000

stocks$port_v <- MSFT.inv*stocks$msft_r + AAPL.inv*stocks$aapl_r

# Estimating Pareto Distribution #
pareto.fit <-gpdFit(as.numeric(stocks$port_v[-1])*-1, type = c("mle"))
tailRisk(pareto.fit, prob = 0.99)
dollar(tailRisk(pareto.fit, prob = 0.99)[,2]*-1)
dollar(tailRisk(pareto.fit, prob = 0.99)[,3]*-1)

hist(-1*rgpd(n = 10000, mu = pareto.fit@parameter$u, beta = pareto.fit@fit$par.ests[2], xi = pareto.fit@fit$par.ests[1]), breaks = 50, main = "Comparison of Simulated tail to Normal", xlab = "Portfolio Value", freq = FALSE, yaxt = "n", ylab = "", col = "lightblue")
curve(dnorm(x, mean(stocks$port_v, na.rm = TRUE), sd(stocks$port_v, na.rm = TRUE)), add = TRUE, col = 'red', lwd = 2)
