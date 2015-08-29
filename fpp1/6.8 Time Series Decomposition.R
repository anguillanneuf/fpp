# Show that a 3×5 MA is equivalent to a 7-term weighted moving average 
# with weights of 0.067, 0.133, 0.200, 0.200, 0.200, 0.133, and 0.067.

a <- c(rep(.2, 5), 0, 0)
b <- c(0, rep(.2, 5), 0)
c <- c(0, 0, rep(.2, 5))
round((a+b+c)*1/3,3)
rm(list=ls())

# The data below represent the monthly sales (in thousands) of product A 
# for a plastics manufacturer for years 1 through 5 (data set plastics).
# Plot the time series of sales of product A. 
# Can you identify seasonal fluctuations and/or a trend?

plot(plastics, ylab="Quantity", xlab="Year",
     main="Monthly Sales of Product A")

# Use a classical multiplicative decomposition to calculate 
# the trend-cycle and seasonal indices. m=12

fit <- decompose(plastics, type = "multiplicative")
fit$seasonal
plot(fit$seasonal)
fit$trend
plot(fit$trend)

# Do the results support the graphical interpretation from part (a)? [yes]

# Compute and plot the seasonally adjusted data.

seasadj(fit) # Tt*Et
plot(seasadj(fit))

# Change one observation to be an outlier (e.g., add 500 to one observation), 
# and recompute the seasonally adjusted data. What is the effect of the outlier?

plastics2 <- plastics
plastics2[30] <- plastics[30]+500
fit2 <- decompose(plastics2, type = "multiplicative")
plot(seasadj(fit2))

# Does it make any difference if the outlier is near the end 
# rather than in the middle of the time series?

plastics3 <- plastics
plastics3[3] <- plastics[3]+500
fit3 <- decompose(plastics3, type = "multiplicative")
plot(seasadj(fit3))

plastics4 <- plastics
plastics4[55] <- plastics[55]+500
fit4 <- decompose(plastics4, type = "multiplicative")
plot(seasadj(fit4))

# Use a random walk with drift to produce forecasts of 
# the seasonally adjusted data.

rm(list=ls())
fit <- decompose(plastics, type = "multiplicative")
# seasadj() can be applied to decomposed time series using decompose() or stl()
s <- seasadj(fit)
# the seasonally adjusted component can be forecast using any non-seasonal 
# forecasting method, like naive(), rwf(drift=TRUE), holt's, ARIMA, etc.
# here, a random walk drift method is used
# == drift forecasts of seasonally adjusted data ==
fit.final <- rwf(s, h=12, drift = TRUE)
plot(rwf(s, h=12, drift = TRUE), 
     main="Random Walk Drift forecast of seaonally adjusted data")

# Reseasonalize the results to give forecasts on the original scale.

# Add the seaonal naive forecasts of the seaonal component
# fit$seasonal[1]*seasadj(fit)[1] == plastics[1]
plot(plastics, xlim=c(1,7), ylim=c(600, 2000),
     main="Prediction of monthly sales of plastics", 
     ylab="Sales", xlab="Year")
sn <- snaive(fit$seasonal, h=12) # seasonal naive forecasts of seasonal component
middle <- fit.final$mean * sn$mean
lower80 <- fit.final$lower[,1] * sn$mean #80% lower
lower95 <- fit.final$lower[,2] * sn$mean #95% lower
upper80 <- fit.final$upper[,1] * sn$mean #80% upper
upper95 <- fit.final$upper[,2] * sn$mean #95% upper
lines(middle, col=3, lwd=2)
lines(lower80, col=5) #blue
lines(lower95, col=7) #yellow
lines(upper80, col=5) #blue
lines(upper95, col=7) #yellow
legend("topleft", lwd=c(1,2,1), col=c(5, 3, 7), 
       legend=c("80% Lower/Upper", "Mean", "95% Lower/Upper"),
       cex=0.5, xjust=1, yjust=1)

# how do I plot shaded prediction interval?

# season length isn't constant

# STL is for additive models
fit2 <- stl(plastics, s.window = "periodic", robust = TRUE)
eeadj <- seasadj(fit2)
plot(eeadj)
fit2.final <- rwf(eeadj, h=12, drift=TRUE)
plot(fit2.final)
plot(plastics, xlim=c(1,7))
forecast1 <- fit2.final$mean + fit2$time.series[,1][1:12]
lines(forecast1, col=4)
plot(forecast(fit2, method="naive"), ylab="Sales", xlab="Year")
lines(fit2.final$mean, col=5)

# Figure 6.13 shows the result of decomposing the number of persons 
# in the civilian labor force in Australia each month from 
# February 1978 to August 1995.
# Write about 3-5 sentences describing the results of the seasonal adjustment. 
# Pay particular attention to the scales of the graphs in making your 
# interpretation.

# The seasonal component is not constant, its variance changes. 
# a small s.window is probably used to capture rapid changes
# I set s.window = "periodic" and that smooths out the seasonal component
# 1. The seasonal adjustment is relatively small. 
# 2. The seasonal adjustment is not constant, probably due to 
# a s.window value that's set to a small value like 5. 

# Is the recession of 1991/1992 visible in the estimated components?
# Yes, it is visible in the remainder component, but not in the
# seaonal and trend components. This is a good sign. 

