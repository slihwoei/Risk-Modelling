library("Rcpp")
library("ggplot2")
library("stats")
library("forecast")

path <- file.path("/Users/siowlihwoei/Desktop", "Programming/Year 3/Y3 Sem 2/F70TS", fsep="//")
setwd(path)
dataset.raw <- read.csv("DataN2O.csv")
dataset.ts <- ts(data = dataset.raw[,2], start = c(2001, 1), frequency=12)

##
##QUESTION 1##
par(mfrow=c(1,1))
#The data, {xm}, against the observation times {m}
plot(dataset.ts, main="Original Series", xlab="Observations Time", ylab="Concentration of Nitrous Oxide")

#The first differences of the data, {xm − xm−1}, against the observation time m.
dataset.diff <- diff(dataset.ts)
plot(dataset.diff, main="1st Order Differencing Series", xlab="Observations Time", ylab="First Differences of Data")

##
##QUESTION 2##
k <- 20
par(mfrow=c(2,1))
# Plot ACF and PACF for the original data
acf(ts(dataset.ts, start = 1), lag.max=k, main="ACF for Original Series")
Pacf(ts(dataset.ts, start = 1), lag.max=k, main="PACF for Original Series")

# Plot ACF and PACF for the first differences
acf(ts(dataset.diff, start = 1), lag.max=k, main="ACF for Time Series of First Differences")
pacf(ts(dataset.diff, start = 1), lag.max=k, main="PACF for Times Serios of First Differences")

##
##QUESTION 4##
#Fir the chosen ARIMA(4,1,3) model to the data
dataset.estm <- Arima(dataset.ts, order = c(4,1,1), include.drift=TRUE)
summary(dataset.estm)

# Extract parameters
phi1 <- round(dataset.estm$coef['ar1'], 3)
phi2 <- round(dataset.estm$coef['ar2'], 3)
phi3 <- round(dataset.estm$coef['ar3'], 3)
phi4 <- round(dataset.estm$coef['ar4'], 3)
theta1 <- round(dataset.estm$coef['ma1'], 3)
theta2 <- round(dataset.estm$coef['ma2'], 3)
theta3 <- round(dataset.estm$coef['ma3'], 3)
mu <- round(dataset.estm$coef['drift'], 3)

# Extract AIC
aic_value <- round(dataset.estm$aic, 3)

# Write out the model
cat("ARIMA(4,1,1) Model Equation:\n")
cat("(1 -", phi1, "*B -", phi2, "*B^2 -", phi3, "*B^3 -", phi4, "*B^4)(1 - B)^1(X_t -", mu, ") =", 
    "(1 +", theta1, "*B +", theta2, "*B^2 +", theta3, "*B^3) * e_t\n")
cat("AIC:", aic_value, "\n")

##
##QUESTION 5##
par(mfrow=c(1,1))

#Plot the forecasted values
forecasted_values <- forecast(dataset.estm, h = 36)
plot(forecasted_values, main = "Forecast the N2O concentration for the next three years", xlab = "Observation Time", ylab = "Concentration of Nitrous Oxide", col="blue")

# Add the original time series data to the plot
lines(dataset.ts, col="red")

#Add the forecast interval
lines(forecasted_values$upper[,2], col="blue", lty=2) # Upper 95% interval
lines(forecasted_values$lower[,2], col="blue", lty=2) # Lower 95% interval

#Add the legend
legend("topleft", legend=c("Observed", "Forecast", "95% Prediction Interval"), 
       col=c("red", "blue", "blue"), lty=c(1, 1, 2), cex=0.8)

