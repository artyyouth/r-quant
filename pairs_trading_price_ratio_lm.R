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

load(file = "djia_20120101_20131130.rda")
# load(file = "sp100_20131119.csv_2012-01-01_2013-11-30.rda")
# load(file = "russell2000_20120625.csv_2012-01-01_2013-11-30.rda")
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
beta <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
p_ratio <- list()

# here we go! let's find the cointegrated pairs
for (j in 1:(nrStocks-1)) {
  for (i in (j+1):nrStocks) {
    
    cat("Calculating price ratio ", j, " - ", i, "\n")
    tmp_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
    if (length(tmp_ds) == 0) 
    {
      beta[j,i] <- NA
      ht[j, i] <- NA
      next
    }

    # The lm function builds linear regression models using OLS.
    # We build the linear model, m, forcing a zero intercept,
    # then we extract the model's first regression coefficient.
    #
    m <- lm(learning_ds[, j] ~ learning_ds[, i] + 0)
    beta[j,i] <- coef(m)[1]
    
    # price i / price j
    # tmp_ds <- ind(tmp_ds)
    p_ratio <- (tmp_ds[,2]/(tmp_ds[,1] * beta[j, i]))
    p_ratio[is.infinite(p_ratio)] <- NA
    p_ratio <- na.omit(p_ratio)
    
    # The ht object contains the p-value from the ADF test.
    # The p-value is the probability that the spread is NOT
    # mean-reverting.  Hence, a small p-value means it is very
    # improbable that the spread is NOT mean-reverting
    p <- try(adfTest(na.omit(coredata(p_ratio)), type="nc")@test$p.value)
    if (isTRUE(class(p) == "try-error"))
    {
      ht[j, i] <- NA
      next
    }
    ht[j, i] <- p
  }
}

# save(ht, file = paste0(ticker_file, "_", date_begin, "_", date_end, "_ADF.rda"))

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
      if (length(tmp_ds) == 0) 
      {
        next
      }
      # tmp_ds <- ind(tmp_ds)
      # price i / price j
      p_ratio <- (tmp_ds[,2]/(tmp_ds[,1] * beta[j, i]))
      p_ratio[is.infinite(p_ratio)] <- NA
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

# save(ht, file = paste0(ticker_file, "_", date_begin, "_", date_end, "_ht.rda"))
# save(rscore, file = paste0(ticker_file, "_", date_begin, "_", date_end, "_rscore.rda"))
# save(pairSummary, file = paste0(ticker_file, "_", date_begin, "_", date_end, "_pairSummary.rda"))

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

if (length(rscore[,1]) == 0) { stop("No good pair found!") }
  
for (pos in 1:length(rscore[,1])) {
  j <- rscore[pos, 1]
  i <- rscore[pos, 2]
  # if (ht[j,i] > 0.01) { next }
  name_j <- stocks[j]
  name_i <- stocks[i]
  
  l_ds <- na.omit(cbind(learning_ds[,j], learning_ds[,i]))
  if (length(tmp_ds) == 0) 
  {
    next
  }
  
  # price i / price j
  l_pr <- (l_ds[,2]/(l_ds[,1] * beta[j, i]))
  l_pr[is.infinite(l_pr)] <- NA
  l_pr <- na.omit(l_pr)
  
  l_ds_j <- tmp_ds[,1]
  l_ds_i <- tmp_ds[,2] * beta[j, i]
  
  t_ds <- na.omit(cbind(test_ds[,j], test_ds[,i]))
  if (length(tmp_ds) == 0)
  {
    next
  }
  
  # price i / price j
  t_pr <- (t_ds[,2]/(t_ds[,1] * beta[j, i]))
  t_pr[is.infinite(t_pr)] <- NA
  t_pr <- na.omit(t_pr)
  
  pr_mean = mean(l_pr, na.rm = T)
  pr_sd = sd(l_pr, na.rm = T)
  
  lb = pr_mean - boundary*pr_sd
  ub = pr_mean + boundary*pr_sd
  
  par(mfrow=c(3,1))
  plot(l_ds_j, type = "l", main = "")
  lines(l_ds_j, col="blue")
  title(main = paste(name_j, "&", name_i))
  points(l_ds_i, type = "l", col = "red")
  
  plot(l_pr, ylim = c(lb, ub))
  abline(h = (pr_mean - sddist*pr_sd), col = "red")
  abline(h = (pr_mean + sddist*pr_sd), col = "red")
  
  plot(t_pr, ylim = c(lb, ub))
  abline(h = (pr_mean - sddist*pr_sd), col = "red")
  abline(h = (pr_mean + sddist*pr_sd), col = "red")
  
  #Sys.sleep(1)
  cmd <- readline()
  if (cmd == 'c') break
}

