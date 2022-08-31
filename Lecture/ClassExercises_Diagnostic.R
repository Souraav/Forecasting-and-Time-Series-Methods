case1=as.ts(scan("case1.txt"))
case1

fit <- arima(case1,order=c(1,0,0))
fit

plot(fit$residuals)
acf(fit$residuals,ylim=c(-1,1))
pacf(fit$residuals,ylim=c(-1,1))

library(forecast)
checkresiduals(fit)

Box.test(resid(fit), lag = 10, type = "Ljung-Box", fitdf = 1)

acf(resid(fit))


# Case 2 ------------------------------------------------------------------

case2=as.ts(scan("case2.txt"))
par(mfrow=c(1,3))
plot(case2)
acf(case2)
pacf(case2)

fit <- arima(case2, order = c(1,0,0))
fit

checkresiduals(fit)

Box.test(resid(fit), lag = 3, type = "Ljung-Box", fitdf = 1)

fit2 <- arima(case2, order = c(1,0,2))
fit2
checkresiduals(fit2)

Box.test(resid(fit2), lag = 3, type = "Ljung-Box", fitdf = 1)

summary(fit2)

fit3 <- arima(case2, order=c(1,0,2), fixed=c(NA,0,NA,NA))
fit3
Box.test(resid(fit3), lag = 3, type = "Ljung-Box", fitdf = 1)

fit2
