#install.packages('tseries')
#install.packages('ggplot2')
#install.packages('forecast')
#install.packages('lmtest')

library(tseries)
library(ggplot2)
library(forecast)
library(ggplot2)
library(lmtest)

#**********ARIMA********************************************
oats <- read.csv("/Users/omkar/Downloads/Price_Of_Oats.csv")
#Define the time series
oats_ts <- ts(oats$Price, start=c(2008), frequency = 1)
#Exploring the data
class(oats_ts)
str(oats_ts)
start(oats_ts)
end(oats_ts)
frequency(oats_ts)
cycle(oats_ts)
#Exploring the data using visuals
plot(oats_ts)
abline(reg=lm(oats_ts~time(oats_ts)))
autoplot(oats_ts)

#Making the data stationary by homogenizing the variance
plot(log(oats_ts))
oats_ts <- log(oats_ts)

#Test to check whether time series is stationary or not.
adf.test(oats_ts, alternative="stationary", k=0)

#Differentiating the data to homogenize the mean 
oats_ts_diff <- diff(oats_ts, differences=4)
adf.test(oats_ts_diff, k=0)
plot(oats_ts_diff)

#*******ARIMA***********************************

#Moving Average
acf(oats_ts_diff)    #q=0

#Autocorrealtion 
pacf(oats_ts_diff)   #p=0

#ARIMA model
oats_def_mod <- (auto.arima(oats_ts, seasonal = FALSE))
oats_def_mod
oats_mod <- arima(oats_ts, c(2,4,0)) 
oats_mod
print(oats_mod)
resid <- residuals(oats_mod)
tsdisplay(residuals(oats_mod), lag.max=15, main='Residuals from ARIMA(2,4,0)')
Box.test(resid, lag=10, type="Ljung-Box")
coeftest(oats_mod)
accuracy(oats_mod)

#Forecasting
forecast(oats_mod, h=3)
autoplot(forecast(oats_mod, h=3))

#Predicting on update time series
data <- ts(oats$Price, frequency = 1, start=c(2008), end=c(2017))
oats_mod1 <- Arima(y=data, order=c(0,2,0)) 
pred <- predict(oats_mod1, n.ahead=2)
#Comparing the predicted value by model and acutal value in the original dataset
head(pred)


#********Simple exponential smoothing**************
alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  oats_ses <- ses(oats_ts_diff, alpha = alpha[i], h = 3)
  RMSE[i] <- accuracy(oats_ses)[2]
}
print(RMSE)
plot(alpha, RMSE)
oats_ses_mod <- ses(oats_ts_diff , h=3)
oats_ses_mod
summary(oats_ses_mod)
autoplot(oats_ses_mod)
accuracy(oats_ses_mod)

#******Holt's model********************************
alpha <- seq(.01, .99, by = .01)
beta <- seq(.01, .99, by = .01)
RMSE <- NA
oats_holt <- holt(oats_ts_diff, h=3)
autoplot(oats_holt)
accuracy(oats_holt) 
summary(oats_holt)
predict(oats_holt)


