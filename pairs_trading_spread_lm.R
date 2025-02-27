library(quantmod)
library(tseries)
library(timeDate)
library(fUnitRoots)
library(RcppArmadillo)

# load(file = "djia_20120101_20131130.rda")
# load(file = "djia_20131119.csv_1992-01-01_2013-11-30.rda")
load(file = "sp100_20131119.csv_1992-01-01_2013-11-30.rda")
# load(file = "russell2000_20120625.csv_2012-01-01_2013-11-30.rda")
stocks <- names(dataset)
nrStocks <- length(stocks)

ht <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
beta <- matrix(data = NA, ncol = nrStocks, nrow = nrStocks)
sprd <- list()

ds_old <- dataset;
t_horizon <- "2010-01-01/2013-11-30"
# subset the dataset
dataset <- dataset[t_horizon]
nDays <- length(dataset[,1])

# seting learning and testing periods
testPeriod <- 252 # 252/4, a quarter
learningPeriod <- 252 * 2 # a year

testDates <- (nDays-testPeriod):nDays
learningDates <- (nDays - testPeriod - learningPeriod):(nDays - testPeriod)

learning_ds <- dataset[learningDates,]
test_ds <- dataset[testDates,]

# here we go! let's find the cointegrated pairs
for (j in 1:(nrStocks-1)) {
  for (i in (j+1):nrStocks) {
    
    cat("Calculating ", j, " - ", i , "\n")
    if (length(na.omit(learning_ds[, i])) == 0 || length(na.omit(learning_ds[, j])) == 0) {
      beta[j,i] <- NA
      ht[j,i] <- NA
      next
    }
    
    # The lm function builds linear regression models using OLS.
    # We build the linear model, m, forcing a zero intercept,
    # then we extract the model's first regression coefficient.
    #
    m <- fastLm(learning_ds[, j] ~ learning_ds[, i] + 0)
    beta[j,i] <- coef(m)[1] # beta or most fitted "hedge ratio"
    sprd <- resid(m)
    
    # The ht object contains the p-value from the ADF test.
    # The p-value is the probability that the spread is NOT
    # mean-reverting.  Hence, a small p-value means it is very
    # improbable that the spread is NOT mean-reverting
    ht[j,i] <- adfTest(na.omit(coredata(sprd)), type="nc")@test$p.value
  }
}

# prepare variables
zscore <- 0;
rscore <- matrix(data = NA, ncol = 4, nrow = (nrStocks^2)/2)
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
    if (ht[j, i] < 0.011) {
      
      sprd <- learning_ds[,j] - beta[j, i]*learning_ds[,i]
      sprd <- na.omit(sprd)
      
      # calculate z-score
      zscore <- sum(abs(scale(sprd)))/length(sprd)
      rscore[idx, 3] <- sd(sprd)
      rscore[idx, 4] <- zscore
      rscore[idx, 1] <- j
      rscore[idx, 2] <- i
      
      # pairSummary[idx, ] = summary(coredata(sprd))[1:6]
      pairSummary[idx, ] = fivenum(coredata(sprd))[1:5]
      idx <- idx + 1
    }
  }
  
  cat("Calculating ", j, "\n")
}

# set up boundaries for 1st and 3rd quartiles
badSprd_up <- 1
badSprd_down <- -1

# re-order spreads
rscore <- na.remove(rscore)
pairSummary <- na.remove(pairSummary)

order_id <- order((rscore[,3]), decreasing = T)
rscore <- rscore[order_id,]
pairSummary <- pairSummary[order_id,]

# 1st quartile > -1 sd & 3rd quartile < 1 sd
goodSprd_id <- (pairSummary[, 2] >  badSprd_down) & (pairSummary[, 4] <  badSprd_up)

backup <- rscore

rscore <- rscore[goodSprd_id, ]
pairSummary <- pairSummary[goodSprd_id, ]

sddist <- 2
boundary <- 4.5

cat("Found ", length(rscore[,1]), " good pairs!")

if (length(rscore[,1]) == 0) { stop("No good pair found!") }

for (pos in 1:length(rscore[,1])) {
  j <- rscore[pos, 1]
  i <- rscore[pos, 2]
  name_j <- stocks[j]
  name_i <- stocks[i]
  
  img_name <- paste0("~/SkyDrive/MBA/Investment/presentation/pairstrading/sprd_exp/sprd_", name_j, "_", name_i, "_", j, "_", i, ".png")
  # png(img_name, width=900, height=1800, units="px", res=300)
  
  sprd <- na.omit(learning_ds[,j] - beta[j, i]*learning_ds[,i])
  sprdTest <- na.omit(test_ds[,j] - beta[j, i]*test_ds[,i])
  
  sprd_mean = mean(sprd, na.rm = T)
  sprd_sd = sd(sprd, na.rm = T)
  
  lb = sprd_mean - boundary*sprd_sd
  ub = sprd_mean + boundary*sprd_sd
  
  # plot price
  par(mfrow=c(6,1))
  plot(learning_ds[, j], type = "l", main = "")
  lines(learning_ds[, j], col="blue")
  title(main = paste(name_j, "&", name_i, "(", j, "-", i, ") Learning"))
  points(beta[j, i]*learning_ds[, i], type = "l", col = "red")
  # points(learning_ds[, i], type = "l", col = "red")
  
  # plot spread in learning period
  plot(sprd, ylim = c(lb, ub))
  abline(h = sprd_mean, col = "green")
  abline(h = (sprd_mean - sprd_sd), col = "brown")
  abline(h = (sprd_mean + sprd_sd), col = "brown")
  abline(h = (sprd_mean - sddist*sprd_sd), col = "red")
  abline(h = (sprd_mean + sddist*sprd_sd), col = "red")
  
  plot(test_ds[, j], type = "l", main = "")
  lines(test_ds[, j], col="blue")
  title(main = paste(name_j, "&", name_i, "(", j, "-", i, ") Test"))
  points(beta[j, i]*test_ds[, i], type = "l", col = "red")
  
  # plot spread in test period
  plot(sprdTest, ylim = c(lb, ub))
  abline(h = sprd_mean, col = "green")
  abline(h = (sprd_mean - sprd_sd), col = "brown")
  abline(h = (sprd_mean + sprd_sd), col = "brown")
  abline(h = (sprd_mean - sddist*sprd_sd), col = "red")
  abline(h = (sprd_mean + sddist*sprd_sd), col = "red")
  
  # plot with new mean, beta and sd
  test_m <- fastLm(test_ds[, j] ~ test_ds[, i] + 0)
  test_beta <- coef(test_m)[1] # beta or most fitted "hedge ratio"
  test_sprd <- resid(test_m)
  test_sprd_mean = mean(test_sprd, na.rm = T)
  test_sprd_sd = sd(test_sprd, na.rm = T)
  
  plot(test_ds[, j], type = "l", main = "")
  lines(test_ds[, j], col="blue")
  title(main = paste(name_j, "&", name_i, "(", j, "-", i, ") Test Adjusted"))
  points(test_beta*test_ds[, i], type = "l", col = "red")
  
  test_lb = test_sprd_mean - boundary*test_sprd_sd
  test_ub = test_sprd_mean + boundary*test_sprd_sd
  
  plot(test_sprd, type="l", ylim = c(test_lb, test_ub))
  abline(h = test_sprd_mean, col = "green")
  abline(h = (test_sprd_mean - test_sprd_sd), col = "brown")
  abline(h = (test_sprd_mean + test_sprd_sd), col = "brown")
  abline(h = (test_sprd_mean - sddist*test_sprd_sd), col = "red")
  abline(h = (test_sprd_mean + sddist*test_sprd_sd), col = "red")
  
  # dev.off()
  cat(paste0(name_j, " & ", name_i, "\t(", j, " - ", i, " )\t", ht[j, i], "\n"))
  
  #Sys.sleep(1)
  cmd <- readline()
  if (cmd == 'c') break
}
