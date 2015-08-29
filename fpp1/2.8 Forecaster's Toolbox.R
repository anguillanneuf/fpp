setwd("./fpp")
require("fpp"); require("fma")
data(package="fma")

# For each of the following series (from the fma package), 
# make a graph of the data. If transforming seems appropriate, 
# do so and describe the effect.

# Monthly total of people on unemployed benefits in Australia 
# (January 1956-July 1992). data("dole")
plot(dole, ylab="Count", xlab="Year", 
     main="Unemployment benefits in Australia")

# use BoxCox.lambda when variance increases or decreases
# with the level of the series. A good lambda makes the size
# of the seasonal variation about the same across the series
BoxCox.lambda(dole)
plot(BoxCox(dole, BoxCox.lambda(dole)), ylab="Count", xlab="Year",
     main="Unemployment benefits in Australia")
# how to transform this back??????

# Monthly total of accidental deaths in the United States 
# (January 1973-December 1978).
plot(usdeaths, ylab="Count", xlab="Year",
     main="Accidental deaths in the United States")

seasonplot(usdeaths,ylab="Count", xlab="Year", 
           main="Accidental deaths in the United States",
           year.labels=TRUE, year.labels.left=TRUE, col=1:6, pch=19)

# xaxt = "n" suppresses plotting of the x axis. 
monthplot(usdeaths,ylab="Count",xlab="Month",xaxt="n",
          main="Seasonal deviation plot: accidental deaths in the U.S.")
axis(1,at=1:12,labels=month.abb,cex=0.8)

# Quarterly production of bricks (in millions of units) at 
# Portland, Australia (March 1956-September 1994).
plot(bricksq, ylab="Count", xlab="Year",
     main="Quarterly clay brick production in Australia")
BoxCox.lambda(bricksq)
plot(BoxCox(bricksq, BoxCox.lambda(bricksq)), ylab="Count", xlab="Year",
     main="Quarterly clay brick production in Australia - Transformed")

# too many lines! bad choice!
seasonplot(bricksq, ylab="Count", xlab="Quarter", 
           main = "Quarterly brick production in Australia", 
           season.labels = TRUE, year.labels = TRUE, year.labels.left = TRUE, 
           col=rainbow(39), pch=19)

# monthplot also plots by quarter!
monthplot(bricksq, ylab="Count", xlab="Quarter", xaxt="n",
          main="Quarterly brick production in Australia",
          col.base=4, lty.base=4, lwd.base=1)
axis(1, at=1:4, labels = c("Q1", "Q2", "Q3", "Q4"), cex=0.8)

# Use the Dow Jones index (data set dowjones) to do the following:
# Produce a time plot of the series.
tsdisplay(dowjones)
plot(dowjones, ylab="Index", xlab="Day", main="Dow-Jones Index")

# Produce forecasts using the drift method and plot them.
rwf(dowjones, h=10, drift = TRUE)
plot(dowjones, ylab="Index", xlab="Day", main="Dow-Jones Index", 
     xlim=c(0, 100))
lines(rwf(dowjones, h=10, drift = TRUE)$mean, col=3, lwd=1, lty=1)

# Show that the graphed forecasts are identical to 
# extending the line drawn between the first and last observations.
segments(x0=1, y0=dowjones[1], x1=78, y1=dowjones[78], col=3, lwd=1, lty=4)

# Try some of the other benchmark functions to forecast 
# the same data set. Which do you think is best? Why?
djfit1 <- meanf(dowjones, h=10)
djfit2 <- naive(dowjones, h=10)
# seasonal naive method produces exactly the same result as naive method
djfit3 <- snaive(dowjones, h=10)
identical(djfit2$mean, djfit3$mean)
lines(djfit1$mean, col=2)
lines(djfit2$mean, col=4)
legend("topleft", lty=1, col=c(3, 4, 2), 
       legend=c("Drift method", "Naive method", "Mean method"))

# the best benchmark method is probably the drift method 
# because it captures the overall trend 
# accuracy() can be used if indices for the next 10 days are provided

# Consider the daily closing IBM stock prices (data set ibmclose).
# Produce some plots of the data in order to become familiar with it.
plot(ibmclose, ylab="Stock price", xlab="Day",
     main="IBM Stock Price")
tsdisplay(ibmclose)

# Split the data into a training set of 300 observations 
# and a test set of 69 observations.
training <- window(ibmclose, 1, 300)
ibmclose2 <- window(ibmclose, 1, 300)
test <- window(ibmclose, 301, 369)

# Try various benchmark methods to forecast the training set and 
# compare the results on the test set. Which method did best?
fit1 <- meanf(training, h=69) # mean method
fit2 <- rwf(training, h=69) # naive method
fit3 <- snaive(training, h=69) # seasonal naive method
fit4 <- rwf(training, h=69, drift = TRUE) # drift method
plot(training, xlim=c(0, 375), main="IBM Stock Price", 
     xlab="Day", ylab="Stock price")
lines(fit1$mean, col=2)
lines(fit2$mean, col=3, lwd = 2)
lines(fit3$mean, col=4, lty=2)
lines(fit4$mean, col=5)
lines(test)
legend("topright", lty=c(1,1,2,1), col=c(2,3,4,5), lwd=c(1,2,1,1), 
       legend = c("Mean method", "Naive method", 
                  "Seasonal naive method", "Drift method"), 
       cex=0.5, xjust=1, yjust=1)
# visually speaking, the drift method produces the best result

accuracy(fit1, test)[2,]
accuracy(fit2, test)[2,]
accuracy(fit3, test)[2,]
accuracy(fit4, test)[2,] # Mean Absolute Scared Error: 2.74
# The best benchmark method is the drift method (regardless of which
# accuracy measure is used)
# Taking a further look at the residuals of the drift method (h=1 by default):
res <- residuals(rwf(ibmclose2, h=1, drift=TRUE))
plot(res, main="Residuals from the Drift Method", ylab="", xlab="Day")
        Acf(res, main="ACF of residuals")
# for a good forecasting method, the residuals are uncorrelated
# 95% of the spikes lie within the bounds, the residuals are white noise
# white noise = no autocorrelation
mean(res, na.rm = TRUE)
# the residuals have zero mean
# the residuals have constant variance
hist(res, nclass = "FD", main="Histogram of Residuals")
# the residuals are normally distributed

# portmanteau test, test if the first h autocorrelations
# are significantly different from a white noise series
# if autocorrelation come from a white noise series
# then Q and Q* would follow a chi-squared distribtion
# to test for this distribution
# use Box.test(res, lag=h, fitdf=K)

# fitdf=K=0, lag=h=10 for non-seasonal data and zero parameter
Box.test(res, lag = 10, fitdf = 0)
Box.test(res, lag = 10, fitdf = 0, type = "Lj")
# p-value = 0.183 and 0.170, large, the results not significant, 
# do not reject null hypothesis, do not reject residuals are white noise

# Consider the sales of new one-family houses in the USA, 
# Jan 1973 - Nov 1995 (data set hsales).
# Produce some plots of the data in order to become familiar with it.
plot(hsales, ylab="Sales", xlab="Year", 
     main="Monthly sales of new one-family houses in USA")
# transformation: calendar adjustment
monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),23)
monthdays <- monthdays[-275]
monthdays[38 + (4*12)*(0:4)] <- 29
plot(hsales/monthdays, ylab="Sales", xlab="Years",
     main="Averaage monthly sales of new houses in USA")

# Split the hsales data set into a training set and a test set, 
# where the test set is the last two years of data.
training <- window(hsales, start=1973, end=1994-1/12)
test <- window(hsales, start=1994)

# Try various benchmark methods to forecast the training set 
# and compare the results on the test set. Which method did best?
plot(training, ylab="Sales", xlab="Year",
     main="Monthly sales of new houses in USA", xlim =c(1973, 1995))
fit1 <- meanf(training, h=23)
fit2 <- rwf(training, h=23)
fit3 <- snaive(training, h=23)
fit4 <- rwf(training, h=23, drift=TRUE)
lines(fit1$mean, col=2)
lines(fit2$mean, col=3)
lines(fit3$mean, col=4)
lines(fit4$mean, col=5)
accuracy(fit1, test)
accuracy(fit2, test)
accuracy(fit3, test)
accuracy(fit4, test)
lines(test)
# seasonal naive is the best method based on its MASE value