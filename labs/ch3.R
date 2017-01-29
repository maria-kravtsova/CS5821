LoadLibraries = function() {
library(MASS)
library(ISLR)
print("The libraries have been loaded")
  }

LoadLibraries()

# medv (median house value) for 506 neighborhoods around Boston
# rm (average number of rooms per house)
# age (average age of houses)
# lstat (percent of households with low socioeconomic status)

attach(Boston)
fix(Boston)
names(Boston)

lm.fit = lm(medv~lstat)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

# Confidence interval and prediction
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="prediction")

plot(lstat, medv, pch=20)
abline(lm.fit, lwd=3, col="red")

par(mfrow=c(2,2)) # split plots screen 2 x 2
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# non-linearity
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression
lm.fit = lm(medv~lstat+age)
summary(lm.fit)

lm.fit = lm(medv~., data=Boston)
summary(lm.fit)
lm.fit1 = update(lm.fit, ~.-age)

# Interaction Terms
summary(lm(medv~lstat*age, data=Boston))

# Non-linear Transformations of the Predictors
lm.fit2 = lm(medv~lstat + I(lstat^2))
summary(lm.fit2)

lm.fit = lm(medv~lstat)
anova(lm.fit, lm.fit2) # hypothesis test
plot(lm.fit2)

lm.fit5 = lm(medv~poly(lstat, 5))
summary(lm.fit5)
plot(lm.fit5)

summary(lm(medv~log(rm)))


# Qualitative Predictorss using Carseats dataset
fix(Carseats)
names(Carseats)
attach(Carseats)
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)

?contrasts
contrasts(ShelveLoc) # dummy variebles
