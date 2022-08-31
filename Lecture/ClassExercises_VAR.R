library(fpp2)
data(uschange)

autoplot(uschange[,1:2], facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumption and personal income")

fit0 <- auto.arima(uschange[,"Consumption"])
fit0
fit <- auto.arima(uschange[,"Consumption"], xreg=uschange[,"Income"])
fit

cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)

acf(residuals(fit, type="regression"))
pacf(residuals(fit, type="regression"))

checkresiduals(fit)

fit_lm <- lm(Consumption~ Income + Production, data = uschange)
fit_lm  

GasPrices<- readxl::read_xlsx(path = "Gas_prices_1.xlsx")
head(GasPrices)
colnames(GasPrices)

fit <- auto.arima(GasPrices$Unleaded, 
                  xreg=as.matrix(GasPrices[,c("L1_Unleaded", 
                                              "L1_Crude_price",
                                              "L1_CPI")]))
fit
