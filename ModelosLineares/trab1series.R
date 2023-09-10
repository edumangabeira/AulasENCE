library(readxl)
serie_trab1 <- read_excel("serie_trab1.xlsx")
serie <- serie_trab1["Eduardo"]
library(forecast)
library(lmtest)
plot(ts(serie))
ggtsdisplay(serie)
fit  <- forecast::Arima(serie, c(4,0,0), include.constant = F)
coeftest(fit)