require(quantmod)

getSymbols(c("SPX","^VIX"), from = "2008-1-1", to = Sys.Date())

ind <- function(x) {
  # Divide each column by the first non-NA value
  # (There may already be a function to do that.)
  coredata(x) <- t(t(coredata(x)) / apply(coredata(x),2,function(u){ c(u[!is.na(u)&u!=0],NA)[1] }))
  x
}
x <- cbind( Ad(SPX), Ad(VIX) )
# x <- x["2010-3::"]

# Using base graphics
matplot( 
  index(x), coredata(ind(x)), 
  xlab="", ylab="", main="",
  type="l", lty=1, lwd=3, axes=FALSE 
)
abline(h=1, lty=3, col="lightgrey")
axis(2, las=1)
axis.Date(1, index(x))
box()
legend( "topleft", gsub("\\..*", "", names(x)), lty=1, lwd=3, col=1:2 )

# If you prefer ggplot2
library(ggplot2)
library(reshape2)
d <- data.frame( date = index(x), coredata(ind(x)) )
names(d) <- gsub("\\..*", "", names(d))
d <- melt(d, id.vars="date")
ggplot(d, aes(date, value, color=variable)) + geom_line(size=2)
