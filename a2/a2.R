library(MASS)
library(ISLR)

attach(Auto)
lm.fit = lm(mpg~horsepower)
summary(lm.fit)

predict(lm.fit, data.frame(horsepower=98), interval="confidence")
predict(lm.fit, data.frame(horsepower=98), interval="prediction")

predict(lm.fit, data.frame(horsepower=95), interval="confidence")
predict(lm.fit, data.frame(horsepower=95), interval="prediction")

plot(horsepower, mpg)
abline(lm.fit, col="red")

which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)
