library(quantmod)
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
f <- getYahooData(nam[1],20090101,20131108,adjust=F)
d <- getYahooData(nam[1],20090101,20131108,type='split',adjust=F)
p <- f[,4]
names(p) <- nam[1]
d <- merge(p,d[,1])
d <- d[,-1]
names(d) <- nam[1]
for(i in 2:9) {
  f <- getYahooData(nam[i],20090101,20131108,adjust=F)
  v <- getYahooData(nam[i],20090101,20131108,type='split',adjust=F)
  f <- f[,4]
  v <- v[,1]
  names(f)  <- names(v) <- nam[i]
  p <- merge(p,f)
  d <- merge(d,v)
}

# Convert NAs to zero
d[is.na(d)] <- 0
dt <- '2011-05-02'
shr <- m/sum(m)*100000000
p0 <- as.numeric(p[dt,])

# Initial number of shares
shr0 <- shr/p0
p$id <- 1:nrow(p)

# Set index for today: lower values are history and higher values are future
d0 <- as.numeric(p$id[dt])
p$id <- NULL
port <- s <- p

# Zero out values
for(i in 1:9) s[,i]  <- port[,i] <- 0

# Initalize 5/2/11 share
for(i in 1:9) s[d0,i] <- shr0[i]

# Make historical adjustments to number of shares
for(i in (d0-1):1) {
  for(j in 1:9)
    s[i,j] <- as.numeric(s[i+1,j])/(1 + as.numeric(d[i,j])/as.numeric(p[i,j]))
}

# Make future adjustment to number of shares in portfolio
for(i in d0:nrow(p)) {
  for(j in 1:9)
    s[i,j] <- as.numeric(s[i-1,j])*(1 + as.numeric(d[i-1,j])/as.numeric(p[i-1,j]))
}

# Compute daily value of positions
port <- s*p

# Compute daily portfolio value
port$val <- rowSums(port)

# Plot the portfolio value over time
plot.xts(port$val/1e6,minor.ticks=F,main='Portfolio Value',ylab="$MM")

# Read futures price files
es <- read.csv('ES.generic.csv',header=T)
es$Date <- as.character(as.POSIXct(es$Date,format='%m/%d/%Y'))
ss <- xts2df(read.zoo('SelSector.generic.csv',format='%m/%d/%Y',sep=',',header=T))

# Merge ES with portfolio and write file
port <- xts2df(port)
all <- merge(port,es,by='Date')
ssec <- all
all <- df2xts(all)
write.zoo(all['201105/'],file='port.csv',sep=',')

# Compute the daily and weekly returns: note that the first obs is dropped
nam <- names(all)
p.wkly.rtn <- weeklyReturn(all[,1])[-1]
names(p.wkly.rtn) <- nam[1]
for(i in 2:ncol(all)) {
  g <- weeklyReturn(all[,i])[-1]
  names(g) <- nam[i]
  p.wkly.rtn <- merge(p.wkly.rtn,g)
}
write.zoo(p.wkly.rtn['200904/201104'],file='port.wkly.csv',sep=',')

# Merge Select Sectors with port/ES.  Time series is shorter than port/es
ssec <- merge(ssec,ss,by='Date')
ssec <- df2xts(ssec)
write.zoo(ssec,file='port.ssec.csv',sep=',')

s.wkly.rtn <- weeklyReturn(ssec[,1])[-1]
nam <- names(ssec)
names(s.wkly.rtn) <- nam[1]
for(i in 2:ncol(ssec)) {
  g <- weeklyReturn(ssec[,i])[-1]
  names(g) <- nam[i]
  s.wkly.rtn <- merge(s.wkly.rtn,g)
}
write.zoo(s.wkly.rtn['/201303'],file='ssec.wkly.csv',sep=',')
