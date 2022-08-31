case9=ts(scan("case9.txt"), start=c(1969,1), end=c(1978,12), frequency=12)
case9
library(forecast)
case9 %>% ggtsdisplay(lag.max=40)

case9 %>% log() %>% diff() %>% ggtsdisplay(lag.max=80)

case9 %>% log() %>% diff() %>% diff(lag=12) %>% ggtsdisplay(lag.max=40)

# case9 %>% log() %>% diff(lag=12) %>% diff() %>% ggtsdisplay(lag.max=40)

(case9 %>% log() %>% diff() %>% diff(lag=12) %>% 
    Arima(order=c(0,0,1), seasonal=c(0,0,1),include.constant = FALSE)) %>% AIC()

# Arima(order=c(0,1,1), seasonal=c(0,1,1),include.constant = FALSE)) %>% AIC()


fit_auto <- auto.arima(log(case9))
fit_auto2 <- auto.arima(case9)

checkresiduals(fit_auto)

fit_auto %>% forecast(h=36) %>% autoplot()
