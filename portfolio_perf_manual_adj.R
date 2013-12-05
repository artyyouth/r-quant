library("quantmod")

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

# data fetch range
d_begin <- 20090101
d_stop <- 20131130
# portfolio life range
t_start <- "2010-11-01"
t_end <- "2013-11-29"
t_horizon <- paste0(t_start, "/", t_end)
# portfolio initial value
init_val <- 10000000 # 10 million

# Read portfolio defination file
p_def <- read.csv('smallcap_port.csv',header=T)[-1]
stocks <- as.character(p_def[,2])
weights <- as.numeric(p_def[,1])
n_stocks <- length(stocks)

# Extract the data from Yahoo: "f" = daily price, "d" = dividend
f <- getYahooData(stocks[1],d_begin,d_stop,adjust=F)
d <- getYahooData(stocks[1],d_begin,d_stop,type='split',adjust=F)
p <- f[,4]
names(p) <- stocks[1]
d <- merge(p,d[,1])
d <- d[,-1]
names(d) <- stocks[1]
for(i in 2:9) {
  f <- getYahooData(stocks[i],d_begin,d_stop,adjust=F)
  v <- getYahooData(stocks[i],d_begin,d_stop,type='split',adjust=F)
  f <- f[,4]
  v <- v[,1]
  names(f)  <- names(v) <- stocks[i]
  p <- merge(p,f)
  d <- merge(d,v)
}

# Convert NAs to zero
d[is.na(d)] <- 0
dt <- t_start
shr <- weights/sum(weights)*init_val
p0 <- as.numeric(p[dt,])

# Initial number of shares
shr0 <- shr/p0
p$id <- 1:nrow(p)

# Set index for today: lower values are history and higher values are future
d0 <- as.numeric(p$id[dt])
p$id <- NULL
port <- s <- p

# Zero out values
for(i in 1:n_stocks) s[,i]  <- port[,i] <- 0

# Initalize share
for(i in 1:n_stocks) s[d0,i] <- shr0[i]

# Make historical adjustments to number of shares
if ((d0 - 1) > 0)
{
  for(i in (d0-1):1) {
    for(j in 1:n_stocks)
      s[i,j] <- as.numeric(s[i+1,j])/(1 + as.numeric(d[i,j])/as.numeric(p[i,j]))
  }
}

# Make future adjustment to number of shares in portfolio
for(i in d0:nrow(p)) {
  for(j in 1:n_stocks)
    s[i,j] <- as.numeric(s[i-1,j])*(1 + as.numeric(d[i-1,j])/as.numeric(p[i-1,j]))
}

# Compute daily value of positions
port <- s*p

# Compute daily portfolio value
port$val <- rowSums(port)

# Plot the portfolio value over time
plot.xts(port$val/1e6,minor.ticks=F,main='Portfolio Value',ylab="$MM")

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
