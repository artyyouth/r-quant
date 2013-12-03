library(quantmod)
library(tseries)
library(timeDate)
library(fUnitRoots)

ind <- function(x) {
  # Divide each column by the first non-NA value
  # (There may already be a function to do that.)
  coredata(x) <- t(t(coredata(x)) / apply(coredata(x),2,function(u){ c(u[!is.na(u)&u!=0],NA)[1] }))
  x
}

# load(file = "djia_20120101_20131130.rda")
# load(file = "sp100_20131119.csv_2012-01-01_2013-11-30.rda")
load(file = "russell2000_20120625.csv_2012-01-01_2013-11-30.rda")
stocks <- names(dataset)
nrStocks <- length(stocks)

ds_old <- dataset;
nDays <- length(dataset[,1])

# seting learning and testing periods
testPeriod <- 63 # 252/4, a quarter
learningPeriod <- 252 # a year

testDates <- (nDays-testPeriod):nDays
learningDates <- (nDays - testPeriod - learningPeriod):(nDays - testPeriod)

learning_ds <- dataset[learningDates,]
test_ds <- dataset[testDates,]

# prepare variables
ht <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
p_ratio <- list()

# here we go! let's find the cointegrated pairs
for (j in 1:(nrStocks-1)) {
  for (i in (j+1):nrStocks) {
    
    cat("Calculating price ratio ", j, " - ", i, "\n")
    if (length(na.omit(learning_ds[, i])) == 0 || length(na.omit(learning_ds[, j])) == 0) {
      beta[j,i] <- NA
      ht[j,i] <- NA
      next
    }
    
    tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
    # price i / price j
    tmp_ds <- ind(tmp_ds)
    p_ratio <- coredata(tmp_ds[,2]/tmp_ds[,1])
    p_ratio[is.finite(p_ratio)] <- NA
    p_ratio <- na.omit(p_ratio)
    
    # The ht object contains the p-value from the ADF test.
    # The p-value is the probability that the spread is NOT
    # mean-reverting.  Hence, a small p-value means it is very
    # improbable that the spread is NOT mean-reverting
    ht[j,i] <- adfTest(na.omit(coredata(p_ratio)), type="nc")@test$p.value
  }
}

zscore <- 0;
rscore <- matrix(data = NA, ncol = 5, nrow = (nrStocks^2)/2)
pairSummary <- matrix(data = NA, ncol = 5, nrow = (nrStocks^2)/2)

idx <- 1;

# lets evaluate the spreads
for (j in 1:(nrStocks-1)) {
  for (i in (j+1):nrStocks) {
    
    # if no data, skip
    if (is.na(ht[j, i])) {
      next
    }
    
    # is spread stationary (i.e. pair is co-integrated)
    # p-value is the smaller the better
    if (ht[j, i] < 0.02) {
      
      tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
      tmp_ds <- ind(tmp_ds)
      # price i / price j
      p_ratio <- coredata(tmp_ds[,2]/tmp_ds[,1])
      p_ratio[is.finite(p_ratio)] <- NA
      p_ratio <- na.omit(p_ratio)
      
      # calculate z-score
      zscore <- sum(abs(scale(p_ratio)))/length(p_ratio)
      rscore[idx, 3] <- sd(p_ratio)
      rscore[idx, 4] <- zscore
      rscore[idx, 5] <- mean(p_ratio)
      rscore[idx, 1] <- j
      rscore[idx, 2] <- i
      
      # pairSummary[idx, ] = summary(coredata(sprd))[1:6]
      pairSummary[idx, ] = fivenum(coredata(p_ratio))[1:5]
      idx <- idx + 1
    }
  }
  
  cat("Calculating ", j, "\n")
}

# clean up na rows
rscore <- na.remove(rscore)
pairSummary <- na.remove(pairSummary)

"
# set up boundaries for 1st and 3rd quartiles
badSD_up <- 2.5
badSD_down <- -2.5

# re-order spreads
order_id <- order(rscore[,3], decreasing = T)
rscore <- rscore[order_id,]
pairSummary <- pairSummary[order_id,]

goodSprd_id <- (pairSummary[, 2] >  badSprd_down) & (pairSummary[, 4] <  badSprd_up)

backup <- rscore

rscore <- rscore[goodSprd_id, ]
pairSummary <- pairSummary[goodSprd_id, ]
"

sddist <- 2
boundary <- 4.5

cat("Found ", length(rscore[,1]), " good pairs!")

for (pos in 1:length(rscore[,1])) {
  j <- rscore[pos, 1]
  i <- rscore[pos, 2]
  
  sprd <- na.omit(learning_ds[,j] - beta[j, i]*learning_ds[,i])
  sprdTest <- na.omit(test_ds[,j] - beta[j, i]*test_ds[,i])
  
  sprd_mean = mean(sprd, na.rm = T)
  sprd_sd = sd(sprd, na.rm = T)
  
  lb = sprd_mean - boundary*sprd_sd
  ub = sprd_mean + boundary*sprd_sd
  
  par(mfrow=c(3,1))
  plot(learning_ds[, j], type = "l", main = "")
  lines(learning_ds[, j], col="blue")
  title(main = paste(stocks[rscore[pos, 1]], "&", stocks[rscore[pos, 2]]))
  points(beta[j, i]*learning_ds[, i], type = "l", col = "red")
  
  plot(sprd, ylim = c(lb, ub))
  abline(h = (sprd_mean - sddist*sprd_sd), col = "red")
  abline(h = (sprd_mean + sddist*sprd_sd), col = "red")
  
  plot(sprdTest, , ylim = c(lb, ub))
  abline(h = (sprd_mean - sddist*sprd_sd), col = "red")
  abline(h = (sprd_mean + sddist*sprd_sd), col = "red")
  
  #Sys.sleep(1)
  readline()
}
