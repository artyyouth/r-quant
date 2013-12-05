library("quantmod")
library("fPortfolio")

ind <- function(x) {
  # Divide each column by the first non-NA value
  # (There may already be a function to do that.)
  coredata(x) <- t(t(coredata(x)) / apply(coredata(x),2,function(u){ c(u[!is.na(u)&u!=0],NA)[1] }))
  x
}

xts2df <- function(x) {
  y <- data.frame(Date=index(x),coredata(x))
  y$Date <- as.character(y$Date)
  return(y)
} 

df2xts <- function(x) {
  y <- zoo(data.matrix(x[-1]),x$Date)
  index(y) <- as.POSIXct(index(y))
  return(as.xts(y))
}

calc_port_val <- function(ds, th, wt) {
  # slice dataset if necessary
  p <- ds[th]
  
  # inital val/shr of portfolio
  dt <- t_start
  shr <- wt/sum(wt)*init_val
  p0 <- as.numeric(p[dt,])
  
  # Initial number of shares
  shr0 <- shr/p0
  
  # Compute daily value of positions
  port <- p;
  n_days <- length(p[,1])
  for (i in 1:n_days)
  {
    for (j in 1:n_stocks)
    {
      port[i,j] <- p[i,j]*shr0[j]
    }
  }
  
  # Compute daily portfolio value
  port$val <- rowSums(port)
  
  port
}

# data fetch range
d_start <- "2008-11-30"
d_end <- "2013-11-30"
d_horizon <- paste0(d_start, "/", d_end)
# data learning range
l_start <- "2008-11-01"
l_end <- "2010-11-01"
l_horizon <- paste0(l_start, "/", l_end)
# portfolio life range
t_start <- "2010-11-01"
t_end <- "2013-11-30"
t_horizon <- paste0(t_start, "/", t_end)
# portfolio initial value
init_val <- 10000000 # 10 million

# Read portfolio defination file
p_def <- read.csv('smallcap_port.csv',header=T)[-1]
stocks <- as.character(p_def[,2])
weights <- as.numeric(p_def[,1])
n_stocks <- length(stocks)

# Fetch data one time call (only useful for S&P500 or DJIA)
tickers <- getSymbols(stocks, src="yahoo", from = d_start, to = d_end, verbose=TRUE)

# combine the adjusted close values in one (xts) data.frame
# dataset <- na.omit(dataset)
dataset <- Ad(get(tickers[1]))
for (i in 2:length(tickers)) {
  dataset <- merge(dataset, Ad(get(tickers[i])))
}

names(dataset) <- stocks

# save(dataset, file = "smallcap_port_2008-11-01_2013-11-30.rda")
# load(file = "smallcap_port_2008-11-01_2013-11-30.rda")

#====================== Raw portfolio performance ========================
port <- calc_port_val(dataset, t_horizon, weights)

# Plot the portfolio value over time
# plot.xts(port$val/1e6,minor.ticks=F,main='Portfolio Value',ylab="$MM")

# Portfolio write file
write.zoo(port,file='port.csv',sep=',')

# Compute the daily and weekly returns: note that the first obs is dropped
nam <- names(port)
p.wkly.rtn <- weeklyReturn(port[,1])[-1]
names(p.wkly.rtn) <- nam[1]
for(i in 2:ncol(port)) {
  g <- weeklyReturn(port[,i])[-1]
  names(g) <- nam[i]
  p.wkly.rtn <- merge(p.wkly.rtn,g)
}
write.zoo(p.wkly.rtn,file='port.wkly.csv',sep=',')

#============================ Create/Optimize the portfolio =======================
# prepare the learning period data
data_spline <- na.spline(dataset[l_horizon])  # cubic spline interpolation

# calculate returns
return_lag <- 1  # (crude) weekly returns
l_data <- na.omit(ROC(data_spline, return_lag, type = "discrete"))
names(l_data) <- stocks

scenarios <- dim(l_data)[1]
assets <- dim(l_data)[2]

# convert xts to TimeSeries for fPortfolio
data_ts <- as.timeSeries(l_data)

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
constraints <- c(constraints, 'minW[1:assets]=0', 'maxW[1:assets]=0.5')
portfolioConstraints(data_ts, spec, constraints)
frontier <- portfolioFrontier(data_ts, spec, constraints)

tailoredFrontierPlot(object=frontier)

weightsPlot(frontier, col=rainbow(assets))

#====================== Algorithmic optimization ============================
# apply the basic Markowitz portfolio optimizer from tseries
result <- portfolio.optim(l_data)
portfolio <- result$pw

# check the portfolio
print(portfolio)

# clean-up portfolio (numerical issues of the optimizer)
prec <- 6
portfolio <- round(portfolio, prec)

# check portfolio again
print(portfolio)

pie_labels <- names(l_data)
pie_labels[which(portfolio == 0)] <- NA
pie(portfolio, labels=pie_labels, col=rainbow(assets))

# calculate portfolio with optimized weights
opt_port <- calc_port_val(dataset, t_horizon, portfolio)

# Portfolio write file
write.zoo(port,file='opt_port.csv',sep=',')

#============================ Plot with index ===============================
benchmark <- getSymbols(c("^RUT"), from = t_start, to = t_end)
benchmark <- Ad(get(benchmark))
pv <- port$val
colnames(pv)[1] <- "Portfolio"
optv <- opt_port$val
colnames(optv)[1] <- "Optimized"
x <- cbind(benchmark, pv, optv)

library(ggplot2)
library(reshape2)
d <- data.frame( date = index(x), coredata(ind(x)) )
names(d) <- gsub("\\..*", "", names(d))
d <- melt(d, id.vars="date")
ggplot(d, aes(date, value, color=variable)) + geom_line(size=2)

# Calculate the yearly return
cat("Pre-optimized portfolio return: \n")
as.data.frame(periodReturn(port$val, 'yearly', subset="2011/"))
cat("Benchmark return: \n")
as.data.frame(periodReturn(benchmark, 'yearly', subset="2011/"))
cat("Optimized portfolio return: \n")
as.data.frame(periodReturn(opt_port$val, 'yearly', subset="2011/"))
