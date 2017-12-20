library(forecast)
library(TSA)
library(tseries)
library(readxl)


########################################################
# Read and clean the data
########################################################

# Load in the excel file
data <- read_excel('co2-atmospheric-mlo-monthly-scripps.xls', range = 'Monthly & Annual CO2 Data!B8:M64', col_names = FALSE, col_types = 'numeric', na = '0')

# Transpose the data and convert to vector
data <- t(data)
data <- try(as.vector(data))

# Exclude the data with missing values
data <- data[-which(is.na(data))]

# Training data
train <- data[1:622]

# Test data
test <- data[623:679]

# Change the data to time series objects
# Training data include observations from 1958 to 2009
# Test data are from 2010 to 2014
train <- ts(train, start = c(1958, 3), end = c(2009, 12), frequency = 12)
test <- ts(test, start = c(2010, 1), end = c(2014, 9), frequency = 12)


########################################################
# Exploratory Data Analysis by plotting 
# and summarizing the data
########################################################

# Plot the data trend
plot(train, type = 'l', main = 'Trend of CO2 levels across the years', ylab = 'Parts per million(ppm)', xlab = 'Year')
axis(4, mean(train))
abline(a = mean(train), b = 0)

# Summary of the data
summary(train)
### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
### 312.4   326.4   343.6   346.0   363.8   390.2 

# ACF(Autocorrelation function) of first differencing
acf(as.vector(diff(train)), lag.max = 36, main = 'Sample ACF of first differences of CO2 levels', ylab = 'ACF of first differences')

# Plot of first and seasonal differences of the data
plot(window(diff(diff(train), lag = 12)), main = 'Plot of first and seasonal differences of CO2 levels', ylab = 'Differences of CO2 levels', xlab = 'Year')


########################################################
# Frequency Analysis
########################################################

# Plot the peridogram and estimated spectral density
par(mfrow = c(2,1))
periodogram(diff(diff(as.vector(train)), lag = 12), main = 'Periodogram of the transformed data')
spec.ar(diff(diff(as.vector(train)), lag = 12), ylim = c(.15,15), lty = 'dotted', xlab = 'Frequency', ylab = 'Estimated Spectral Density', main = 'AR Spectral Estimates of the transformed data')

# ACF and PACF(Partial autocorrelation function) of the transformed data
acf(diff(diff(as.vector(train)), lag = 12), ci.type = 'ma', lag.max = 36, main = 'Sample ACF of differences of CO2 levels')
pacf(diff(diff(as.vector(train)), lag = 12), lag.max = 36, main = 'Sample PACF of differences of CO2 levels')

# Shapiro and runs test of the transformed data
shapiro.test(diff(diff(as.vector(train)), lag = 12))
###	Shapiro-Wilk normality test
###
### data:  train
### W = 0.99646, p-value = 0.1954

runs(diff(diff(as.vector(train)), lag = 12))
### $pvalue
### [1] 9.98e-07
###
### $observed.runs
### [1] 366
###
### $expected.runs
### [1] 305.4007
###
### $n1
### [1] 310
###
### $n2
### [1] 299
###
### $k
### [1] 0


########################################################
# Model fitting and diagnostics
########################################################

# Fit ARIMA(0,1,1)(0,1,1)12 model to the data
auto.arima(train)
fit <- arima(train, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))

# Model diagnostics
tsdiag(fit)

# QQ plot of the residuals
par(mfrow = c(1,1))
qqnorm(fit$resid, main = 'QQ Plot of the residuals of SARIMA model'); qqline(fit$resid)

shapiro.test(fit$resid)
### Shapiro-Wilk normality test
###
### data:  fit$resid
### W = 0.99665, p-value = 0.2205

runs(fit$resid)
### $pvalue
### [1] 0.366
###
### $observed.runs
### [1] 299
###
### $expected.runs
### [1] 310.7138
###
### $n1
### [1] 291
###
### $n2
### [1] 331
###
### $k
### [1] 0

# Periodogram of residuals
periodogram(rstandard(fit), main = 'Periodogram of the residuals of SARIMA model')

# Examine the need of ARCH/GARCH model
# Looks like it is not necessary
acf(abs(window(rstandard(fit), start = c(1958, 3))), main = 'ACF of absolute value of the resiudals of SARIMA model')
acf((window(rstandard(fit), start = c(1958, 3))^2), main = 'ACF of squared value of the resiudals of SARIMA model')

# McLeod-Li Test
McLeod.Li.test(y = (fit$resid), main = 'McLeod-Li test for the residuals of SARIMA model')


########################################################
# Forecast and evaluate the model
########################################################

# Plot and save the predicted and actual data
png('monthlyCO2_prediction.png',width = 580, height = 480 )
val <- plot(fit, n1 = c(2000, 12), n.ahead = 57, xlab = 'Year', type = 'o', ylab = 'CO2 Levels(ppm)',main = 'Forecast of the monthly CO2 level')
points(test, col = 'red', pch = 16, cex = 0.8)
legend(2010, 380, c('95% prediction bounds'), lty = c('dotted'), bty = 'n')
legend(2010.7, 376, 'Predicted value', pch  = 1, bty = 'n')
legend(2010.7, 372, 'Actual value', pch  = 16, col = 'red', bty = 'n')
dev.off()

# Calculate the percentage of actual values that fall within the 95% prediction limits
k <- 0
for(i in 1:length(test)){
  if(test[i] <= val$upi[i]&&test[i] >= val$lpi[i]){
    k <- k+1
  }
}

k/length(test)
### [1] 0.9649123

# Compute the mean sqaured error
1/length(test)*(sum((val$pred-test)^2))
### [1] 1.302504
