# for windows
# setwd("c:\\workplace\\private\\r-quant\\")

# for mac
setwd("~/WorkPlace/quant/r-quant/")

require("quantmod")

# ticker_file <- "djia_20131119.csv"
# ticker_file <- "sp100_20131119.csv"
# ticker_file <- "russell2000_20120625.csv"
ticker_file <- "sp500_20131119.csv"
date_begin <- as.Date("2010-01-01")
date_end <- as.Date("2014-01-31")

symbols <- read.csv(ticker_file, header = FALSE, stringsAsFactors = FALSE)
stocks <- symbols[,1]

# one time call (only useful for S&P500 or DJIA)
# tickers <- getSymbols(c(stocks), src="yahoo", from = date_begin, to = date_end, verbose=TRUE)

# manually re-try download, useful for Russell 2000
tickers <- getSymbols(stocks[1], from = date_begin, to = date_end)
for (i in 2:length(stocks))
{
  tmp <- try(getSymbols(stocks[i], from = date_begin, to = date_end))
  fetched <- TRUE
  if (isTRUE(class(tmp) == "try-error"))
  {
    fetched <- FALSE
    # redo it for 5 times if error happens
    for (j in 1:5)
    {
      Sys.sleep(1)
      tmp <- try(getSymbols(stocks[i], from = date_begin, to = date_end))
      if (isTRUE(class(tmp) == "try-error"))
      {
        next
      }
      else
      {
        fetched <- TRUE
        break
      }
    }
  }

  Sys.sleep(1)
  if (fetched == TRUE) { tickers <- c(tickers, tmp) }
}

# combine the adjusted close values in one (xts) data.frame
dataset <- Ad(get(tickers[1]))
for (i in 2:length(tickers)) {
	dataset <- merge(dataset, Ad(get(tickers[i])))
}

names(dataset) <- tickers

# handle NA values (four common alternatives)
# data_omit <- na.omit(dataset)  # omit values with NA values
# data_locf <- na.locf(dataset)  # last observation carried forward
# data_approx <- na.approx(dataset)  # linear approximation
# data_spline <- na.spline(dataset)  # cubic spline interpolation

# save data as RData
save(dataset, file = paste0(ticker_file, "_", date_begin, "_", date_end, ".rda"))
