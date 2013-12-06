library(MASS)

#Code largely copied from http://quant.stackexchange.com/questions/1027/how-are-correlation-and-cointegration-related

#The input data
nsim <- 250  #Number of data points
mu_a <- 0.0002  #Mu_a growth rate for stock a
sigma_a <- 0.010   #Sigma_a volatility for stock a
mu_b <- 0.0005  #Mu_a growth rate for stock a
sigma_b <- 0.005   #Sigma_a volatility for stock a
corxy <- 0.8    #Correlation coeficient for xy

#Calculate a correlated return series
#Build the covariance matrix and generate the correlated random results
(covmat <- matrix(c(sigma_a^2, corxy*sigma_a*sigma_b, corxy*sigma_a*sigma_b, sigma_b^2), nrow=2))
res <- mvrnorm(nsim, c(mu_a, mu_b), covmat)    #Calculate multivariate normal distribution
plot(res[,1], res[,2])

#Calculate the stats of res[] so they can be checked with the input data
mean(res[,1])
sd(res[,1])
mean(res[,2])
sd(res[,2])
cor(res[,1], res[,2])

path_a <- exp(cumsum(res[,1]))
path_b <- exp(cumsum(res[,2]))
spread <- path_a - path_b
#Set the plotting area to a 2 by 1 grid
layout(rbind(1,2))
#Plot the two price series that have correlated returns
plot(path_a, main="Two Price Series with Correlated Returns", ylab="Price", type="l", col="red")
lines(path_b, col="blue")
plot(spread, type="l")


##Cointegrated pair
#The input data
nsim <- 250  #Number of data points
mu_a <- 0.0002  #Mu_a growth rate for stock a
sigma_a <- 0.010   #Sigma_a volatility for stock a
mu_b <- 0.0002  #Mu_a growth rate for stock a
sigma_b <- 0.005   #Sigma_a volatility for stock a
coea <- 0.0200    #Co-integration coefficient for x
coeb <- 0.0200    #Co-integration coefficient for y

#Generate the noise terms for x and y
rana <- rnorm(nsim, mean=mu_a, sd=sigma_a) #White noise for a
ranb <- rnorm(nsim, mean=mu_b, sd=sigma_b) #White noise for b

#Generate the co-integrated series x and y
a <- numeric(nsim)
b <- numeric(nsim)
a[1] <- 0
b[1] <- 0
for (i in 2:nsim) {
  #Logic here is that is b>a then we add on the difference so that
  #a starts to catch up with b, hence causing the spread to close
  a[i] <- a[i-1] + (coea * (b[i-1] - a[i-1])) + rana[i-1]
  b[i] <- b[i-1] + (coeb * (a[i-1] - b[i-1])) + ranb[i-1]
}

#Plot a and b as prices
ylim <- range(exp(a), exp(b))
path_a <- exp(a)
path_b <- exp(b)
spread <- path_a - path_b

dev.new()
layout(rbind(1,2))
plot(path_a, ylim=ylim, type="l", main=paste("Co-integrated Pair (coea=",coea,",  coeb=",coeb,")", sep=""), ylab="Price", col="red")
lines(path_b, col="blue")
legend("bottomleft", c("exp(a)", "exp(b)"), lty=c(1, 1), col=c("red", "blue"), bg="white")

plot(spread,type="l")