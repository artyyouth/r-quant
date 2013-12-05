# Ref video: http://youtu.be/O33dF532pRo
# R script to read prices from Yahoo and construct a portfolio
# updated 08/19/2012
# get libraries of routines – these packages need to be installed
library(timeSeries)
library(fPortfolio) # may also require installing the package require(slam)
library(quantmod)
library(caTools)
# create list of stock tickers – replace the tickers here with those you want to use in your portfolio
TickerList <- c("DELL", "GOOG","CSCO", "MSFT", "JNPR")
# read closing prices from Yahoo keeping only the closing prices
ClosingPricesRead <- NULL
for (Ticker in TickerList)
ClosingPricesRead <- cbind(ClosingPricesRead,
getSymbols.yahoo(Ticker, from="1950-01-01", verbose=FALSE, auto.assign=FALSE)[,6]) # [,6] = keep the adjusted prices
# keep only the dates that have closing prices for all tickers
ClosingPrices <- ClosingPricesRead[apply(ClosingPricesRead,1,function(x) all(!is.na(x))),]

# convert prices to daily returns
returns <- as.timeSeries(tail(ClosingPrices,-1) / as.numeric(head(ClosingPrices,-1)) - 1)

# calculate the efficient frontier
Frontier <- portfolioFrontier(returns)

# plot frontier
plot(Frontier,1) # can also call the plot routine so it only plots the frontier: plot(Frontier,1)

###########################################################################################
####### addtional code to get a better look at the portfolios – annualize the returns and risk
# get the means and covariance matrix of the price returns
getStatistics(Frontier)$mean # data input into the efficient frontier calculator
cor(returns)

# execute the next commands to plot annualized returns and risk
# convert from daily to annual returns and risk for points on the efficient frontier
# plot efficient frontier using annualized return and risk
riskReturnPoints <- frontierPoints(Frontier) # get risk and return values for points on the efficient frontier
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
targetReturn=riskReturnPoints[,"targetReturn"] * 252)

plot(annualizedPoints)

# plot Sharpe ratios for each point on the efficient frontier
riskFreeRate <- 0
plot((annualizedPoints[,"targetReturn"] – riskFreeRate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab="Sharpe ratio")

# plot the allocation to each stock for each point on the efficient frontier
# weightsPlot(Frontier)
allocations <- getWeights(Frontier@portfolio) # get allocations for each instrument for each point on the efficient frontier
colnames(allocations) <- TickerList
barplot(t(allocations), col=rainbow(ncol(allocations)+2), legend=colnames(allocations))
allocations

############################################################################################
# examine the efficient frontier for portfolios with different constraints
constraints <- "minW[1:length(TickerList)]=-1"
Frontier <- portfolioFrontier(returns, constraints = constraints)
Frontier.LongOnly <- portfolioFrontier(returns)
riskReturnPoints <- frontierPoints(Frontier)
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
targetReturn=riskReturnPoints[,"targetReturn"] * 252)
riskReturnPoints.LongOnly <- frontierPoints(Frontier.LongOnly)
annualizedPoints.LongOnly <- data.frame(targetRisk=riskReturnPoints.LongOnly[, "targetRisk"] * sqrt(252),
targetReturn=riskReturnPoints.LongOnly[,"targetReturn"] * 252)
xlimit <- range(annualizedPoints[,1], annualizedPoints.LongOnly[,1])
ylimit <- range(annualizedPoints[,2], annualizedPoints.LongOnly[,2])
plot(annualizedPoints.LongOnly, xlim=xlimit, ylim=ylimit, pch=16, col="blue")
points(annualizedPoints, col="red", pch=16)
legend("right", legend=c("long only","constrained"), col=c("blue","red"), pch=16)

######
# other constraints
constraints <- c("minW[1:length(TickerList)]=.10","maxW[1:length(TickerList)]=.60")
# write data to csv file to import into excel
write.csv(allocations, "allocations.csv")