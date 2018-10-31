#-----------------------------#
# Theory and Model Assessment #
#      Through Simulation     #
#                             #
#           Dr LaBarr         #
#-----------------------------#

# Needed Libraries for Analysis #
library(graphics)

# Theory Assessment - CLT #
sample.size <- 10
simulation.size <- 10000

X1 <- matrix(data=rnorm(n=(sample.size*simulation.size), mean=2, sd=5), nrow=simulation.size, ncol=sample.size, byrow=TRUE)
X2 <- matrix(data=runif(n=(sample.size*simulation.size), min=5, max=105), nrow=simulation.size, ncol=sample.size, byrow=TRUE)
X3 <- matrix(data=(rexp(n=(sample.size*simulation.size)) + 3), nrow=simulation.size, ncol=sample.size, byrow=TRUE)

Mean.X1 <- apply(X1,1,mean)
Mean.X2 <- apply(X2,1,mean)
Mean.X3 <- apply(X3,1,mean)

hist(Mean.X1, breaks=50, main='Sample Distribution of Means for Normal Distribution', xlab='Sample Means')
hist(Mean.X2, breaks=50, main='Sample Distribution of Means for Uniform Distribution', xlab='Sample Means')
hist(Mean.X3, breaks=50, main='Sample Distribution of Means for Exponential Distribution', xlab='Sample Means')

# Target Shuffling - Student Grade Example 
x <- c(75, 85, 87, 95)

y.all <- data.frame(t(permutations(4,4,x)), input = 1:4)

my_lms <- lapply(1:24, function(x) lm(y.all[,x] ~ y.all$input))
summaries <- lapply(my_lms, summary)
rsq <- sapply(summaries, function(x) c(r_sq = x$r.squared))

plot(y.all$input, y.all$X3, main = "Hours vs. Grades - Actual", xlab = "Hours Studied", ylab = "Exam Grade", xlim = c(0,5), ylim = c(70,100), pch=16, col = "blue")
plot(y.all$input, y.all$X1, main = "Hours vs. Grades - Shuffle 1", xlab = "Hours Studied", ylab = "Exam Grade", xlim = c(0,5), ylim = c(70,100), pch = 16, col = "blue")
plot(y.all$input, y.all$X24, main = "Hours vs. Grades - Shuffle 2", xlab = "Hours Studied", ylab = "Exam Grade", xlim = c(0,5), ylim = c(70,100), pch = 16, col = "blue")

summary(lm(y.all$X3 ~ y.all$input))


# Target Shuffling - Fake Data #
# set.seed(1234)
Fake <- data.frame(matrix(rnorm(n=(100*8)), nrow=100, ncol=8))
Err <- rnorm(n=100, mean=0, sd=8)
Y <- 5 + 2*Fake$X2 - 3*Fake$X8 + Err
Fake <- cbind(Fake, Err, Y)

sim <- 1000

Y.Shuffle <- matrix(0, nrow=100, ncol=sim)
for(j in 1:sim){
  Uniform <- runif(100)
  Y.Shuffle[,j] <- Y[order(Uniform)]
}

Y.Shuffle <- data.frame(Y.Shuffle)
colnames(Y.Shuffle) <- paste('Y.',seq(1:sim),sep="")

Fake <- data.frame(Fake, Y.Shuffle)

R.sq.A <- rep(0,sim)
for(i in 1:sim){
  R.sq.A[i] <- summary(lm(Fake[,10+i] ~ Fake$X1 + Fake$X2 + Fake$X3 + Fake$X4
                          +                           + Fake$X5 + Fake$X6 + Fake$X7 + Fake$X8))$adj.r.squared
}
True.Rsq.A <- summary(lm(Fake$Y ~ Fake$X1 + Fake$X2 + Fake$X3 + Fake$X4
                         +                          + Fake$X5 + Fake$X6 + Fake$X7 + Fake$X8))$adj.r.squared

hist(c(R.sq.A,True.Rsq.A), breaks=50, col = "blue", main='Distribution of Adjusted R-Squared Values', xlab='Adjusted R-Squared')
abline(v = True.Rsq.A, col="red", lwd=2)
mtext("True Model", at=True.Rsq.A, col="red")

P.Values <- NULL
for(i in 1:sim){
  P.V <- summary(lm(Fake[,10+i] ~ Fake$X1 + Fake$X2 + Fake$X3 + Fake$X4
                    +                           + Fake$X5 + Fake$X6 + Fake$X7 + Fake$X8))$coefficients[,4]
  P.Values <- rbind(P.Values, P.V)
}

Sig <- P.Values < 0.05

colSums(Sig)
table(rowSums(Sig)-1)

hist(rowSums(Sig)-1, breaks=25, col = "blue", main='Count of Significant Variables Per Model', xlab='Number of Sig. Variables')
abline(v = 2, col="red", lwd=2)
mtext("True Model", at=2, col="red")

