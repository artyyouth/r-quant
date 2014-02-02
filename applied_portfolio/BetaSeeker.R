library(quantmod)
library(tseries)
library(timeDate)
library(RcppArmadillo)

setwd("~/workplace/quant/r-quant")

# load(file = "djia_20120101_20131130.rda")
# load(file = "djia_20131119.csv_1992-01-01_2013-11-30.rda")
# load(file = "sp100_20131119.csv_1992-01-01_2013-11-30.rda")
# load(file = "russell2000_20120625.csv_2012-01-01_2013-11-30.rda")
load(file = "sp500_20131119.csv_2010-01-01_2014-01-31.rda")
stocks <- names(dataset)
nrStocks <- length(stocks)

beta <- vector(length = nrStocks)

t_start <- "2012-01-01"
t_end <- "2014-01-31"
t_horizon <- paste0(l_start, "/", l_end)
# subset the dataset
dataset <- dataset[t_horizon]
nDays <- length(dataset[,1])

benchmark <- getSymbols(c("SPY"), from = t_start, to = t_end)
benchmark <- Ad(get(benchmark))

for (i in 1:nrStocks)
{
  tmp_ds <- cbind(dataset[,i], benchmark)
  tmp_ds <- na.omit(tmp_ds)
  if (length(tmp_ds[,1]) == 0)
  {
    beta[i] <- NaN
    next
  }
  
  m <- fastLm(tmp_ds[ ,1] ~ tmp_ds[ ,2] + 0)
  beta[i] <- coef(m)[1]
}

output <- cbind(stocks, beta)
write.csv(output, file="sp500_beta.csv", sep=",")