case5 <- as.ts(scan("case5.txt"))
par(mfrow=c(1,3))
plot(case5)
test1 <- acf(case5)
pacf(case5)

library(forecast)
library(tseries)
adf.test(case5, k=1)
adf.test(case5, k=2)

adf.test(diff(case5), k=2)

acf(diff(case5))
pacf(diff(case5))

adf.test(diff(case5, differences = 2), k=1)

fit1 <- arima(case5, order = c(0,1,0))
fit1
checkresiduals(fit1)

fit2 <- arima(case5, order = c(1,1,0))
fit2

fit3 <- auto.arima(case5)
fit3
checkresiduals(fit3)


# library(urca)
# test=ur.df(y = case5, type = "none", lags = 3)
# summary(test)
# adf.test(case5, k=3)

par(mfrow=c(1,3))
plot(forecast(fit1, h=50))
plot(forecast(fit2, h=50))
plot(forecast(fit3, h=50))




# Case 1 ------------------------------------------------------------------
case1=as.ts(scan("case1.txt"))
case1
par(mfrow=c(1,3))
plot(case1)
acf(case1)
pacf(case1)

fit <- arima(case1,order=c(1,0,0))
fit

adf.test(case1, k=1)
adf.test(case1, k=2)


par(mfrow=c(1,3))
plot(forecast(fit,h=50),ylim=c(-40,40))
fit2 <- arima(case1,order=c(0,1,0))
fit2
fit


plot(forecast(fit2,h=50),ylim=c(-40,40))
fitauto <- auto.arima(case1)
fitauto
plot(forecast(fitauto,h=50),ylim=c(-40,40))
