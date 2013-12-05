# required packages: financeR, tseries
require("financeR")
require("xts")
require("fPortfolio")

#============== Portfolio Creation ======================

# load a prepared scenario set from package financeR
data(djia2012w)

# analyze scenario data
names(data)
scenarios <- dim(data)[1]
assets <- dim(data)[2]

# convert xts to TimeSeries for fPortfolio
data_ts <- as.timeSeries(data)

# fPortfolio specification: solver and efficient fronier
spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
setNFrontierPoints(spec) <- 20

# fPortfolio constraints
constraints <- c('LongOnly')
portfolioConstraints(data_ts, spec, constraints)

# perform optimization
frontier <- portfolioFrontier(data_ts, spec, constraints)
print(frontier)

# plot efficient frontier
tailoredFrontierPlot(object=frontier)

# plot weights
weightsPlot(frontier, col=rainbow(assets))

# extended constraints: add upper investment limits
constraints <- c('minW[1:assets]=0', 'maxW[1:assets]=0.5')
portfolioConstraints(data_ts, spec, constraints)
frontier <- portfolioFrontier(data_ts, spec, constraints)

tailoredFrontierPlot(object=frontier)

weightsPlot(frontier, col=rainbow(assets))

#=================== Portfolio Optimization ======================
# required packages: financeR, xts, tseries
require("financeR")
require("xts")
require("tseries")

# load a prepared scenario set from package financeR
data(djia2012w)

# analyze scenario data
names(data)
scenarios <- dim(data)[1]
assets <- dim(data)[2]

# apply the basic Markowitz portfolio optimizer from tseries
result <- portfolio.optim(data)
portfolio <- result$pw

# check the portfolio
print(portfolio)

# clean-up portfolio (numerical issues of the optimizer)
prec <- 6
portfolio <- round(portfolio, prec)

# check portfolio again
print(portfolio)

pie_labels <- names(data)
pie_labels[which(portfolio == 0)] <- NA
pie(portfolio, labels=pie_labels, col=rainbow(assets))
