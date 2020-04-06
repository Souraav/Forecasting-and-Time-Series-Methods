library(forecast)
# Case 1 ------------------------------------------------------------------

case1=as.ts(scan("case1.txt"))
case1
par(mfrow=c(1,3))
plot(case1)
acf(case1)
pacf(case1)
fit <- arima(case1,order=c(1,0,0))
fit

par(mfrow=c(1,2))
plot(forecast(fit,h=30))
fitauto <- auto.arima(case1)
fitauto
plot(forecast(fitauto,h=30))

par(mfrow=c(1,1))
plot.ts(case1)
points(fitted(fit),pch=20,col="grey")
points(fitted(fit),type="l",col="grey")

points(fitted(fitauto),pch=20,col="red")
points(fitted(fitauto),type="l",col="red")

# Case 2 ------------------------------------------------------------------

case2=as.ts(scan("case2.txt"))
case2
par(mfrow=c(1,3))
plot(case2)
acf(case2)
pacf(case2)

fit1 <- arima(case2, order = c(1, 0, 0))
fit2 <- arima(case2, order = c(1, 0, 5))
fit3 <- arima(case2, order = c(1, 0, 6))
fit3
fit4 <- arima(case2, order = c(1, 0, 2))
fit4
fit5 <- arima(case2, order = c(1, 0, 1))
fit6 <- arima(case2, order = c(1, 0, 2))

fit_auto <- auto.arima(case2)
fit_auto
# Case 3 ------------------------------------------------------------------
case3=as.ts(scan("case3.txt"))
case3
par(mfrow=c(1,3))
plot(case3)
acf(case3)
pacf(case3)


fit1 <- arima(case3, order = c(2, 0, 0))
fit1
fit2 <- arima(case3, order = c(2, 0, 6))
fit2
fit3 <- arima(case3, order = c(2, 1, 3))
fit3

fit4 <- arima(case3, order = c(1, 1, 2))
fit4
BIC(fit4)
fit_auto <- auto.arima(case3)
fit_auto
par(mfrow=(c(1,2)))
plot(case3)
plot(diff(case3))

fit_auto <- auto.arima(case3)
fit_auto
