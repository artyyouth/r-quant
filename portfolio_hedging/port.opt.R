require("xts")
require("fPortfolio")
require("quantmod")

start_date <- as.POSIXct("2009-01-01")
end_date <- as.POSIXct("2013-11-08")
c_date <- as.POSIXct("2011-05-02")
init_value <- 100000000

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

# Read company market cap files
m <- read.csv('mktcap.csv',header=T)[-1]
nam <- names(m)
m <- as.numeric(m)

# Extract the data from Yahoo: "f" = daily price, "d" = dividend
f <- getYahooData(nam[1],start_date,end_date,adjust=F)
d <- getYahooData(nam[1],start_date,end_date,type='split',adjust=F)
p <- f[,4]
names(p) <- nam[1]
d <- merge(p,d[,1])
d <- d[,-1]
names(d) <- nam[1]
for(i in 2:9) {
  f <- getYahooData(nam[i],start_date,end_date,adjust=F)
  v <- getYahooData(nam[i],start_date,end_date,type='split',adjust=F)
  f <- f[,4]
  v <- v[,1]
  names(f)  <- names(v) <- nam[i]
  p <- merge(p,f)
  d <- merge(d,v)
}

# Convert NAs to zero
d[is.na(d)] <- 0
dt <- c_date

shr <- m/sum(m)*init_value
p0 <- as.numeric(p[dt,])

# Initial number of shares
shr0 <- shr/p0
p$id <- 1:nrow(p)