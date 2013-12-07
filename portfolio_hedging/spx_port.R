require(quantmod)

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

getSymbols(c("SPX"), from = "2011-05-02", to = "2013-11-08")

# Read from portfolio file
p <- read.csv('port.csv', header=T)
colnames(p)[1] <- "Date"
sv <- xts2df(Ad(SPX))
x <- merge(p, sv, by='Date')
p <- df2xts(x)
pv <- p[,'val']
colnames(pv)[1] <- "Portfolio"
# Calculate the yearly return of unhedged portfolio
as.data.frame(periodReturn(pv, 'yearly', subset="2011/"))
sv <- p[,'SPX.Adjusted']
x <- cbind(sv, pv)

# Read from hedged portfolio file
h <- read.csv('hedged.port.csv', header=T)
h$Date <- as.character(as.POSIXct(h$Date,format='%m/%d/%Y'))
hv <- df2xts(h)
# Calculate the yearly return of hedged portfolio
as.data.frame(periodReturn(hv, 'yearly', subset="2011/"))
x <- cbind(x, hv)

library(ggplot2)
library(reshape2)
d <- data.frame( date = index(x), coredata(ind(x)) )
names(d) <- gsub("\\..*", "", names(d))
d <- melt(d, id.vars="date")
ggplot(d, aes(date, value, color=variable)) + geom_line(size=2)