library(sem)
data(Klein)
Klein$P.lag <- c(NA, Klein$P[-22])
Klein$X.lag <- c(NA, Klein$X[-22])
summary(tsls(C ~ P + P.lag + I(Wp + Wg), instruments=~1 + G + T + Wg + I(Year - 1931) + K.lag + P.lag + X.lag, data=Klein))

lm(C ~ P + P.lag + I(Wp + Wg), data = Klein)