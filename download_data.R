# for windows
# setwd("c:\\workplace\\private\\r-quant\\")

# for mac
setwd("~/WorkPlace/quant/r-quant/")

require("quantmod")

symbols <- read.csv("djia_20131119.csv", header = FALSE, stringsAsFactors = FALSE)
stocks <- symbols[,1]

date_begin <- as.Date("2012-01-01")
date_end <- as.Date("2013-11-30")

tickers <- getSymbols(stocks, from = date_begin, to = date_end)

# combine the adjusted close values in one (xts) data.frame
dataset <- Ad(get(tickers[1]))
for (i in 2:length(tickers)) {
	dataset <- merge(dataset, Ad(get(tickers[i])))
}

names(dataset) <- stocks

# handle NA values (four common alternatives)
data_omit <- na.omit(dataset)  # omit values with NA values
# data_locf <- na.locf(dataset)  # last observation carried forward
# data_approx <- na.approx(dataset)  # linear approximation
# data_spline <- na.spline(dataset)  # cubic spline interpolation

# save data as RData
save(dataset, file = "djia_20120101_20131130.rda")
