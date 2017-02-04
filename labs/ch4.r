library(ISLR)
attach(Smarket)
summary(Smarket)
pairs(Smarket)

cor(Smarket[,-9]) #Year and volume have strong correlations
plot(Volume) #volume is increasing overtime

# Logistic Regression
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef[,4]
glm.probs = predict(glm.fit, type="response")
glm.probs[1:10]
contrasts(Direction)

glm.pred = rep("Down", 1250)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred, Direction)
mean(glm.pred==Direction)
train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fit = glm(Direction~Lag1+Lag2, family=binomial, subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

predict(glm.fit, newdata = data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1, -0.8)), type = "response")

# Linear Discriminant Analysis
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, subset=train)
lda.fit
lda.pred = predict(lda.fit, Smarket.2005)
lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior[,1] >= 0.5)
sum(lda.pred$posterior[,1] < 0.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]

# Quadratic Discriminant Analysis
qda.fit = qda(Direction~Lag1+Lag2, subset=train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

# K Nearest Neighbors
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# KNN with Caravan Insurance Data
dim(Caravan)
attach(Caravan)
summary(Purchase)

# standardize data 
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])


# split in data sets
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]

# predict
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=5)
mean(test.Y != knn.pred)
mean(test.Y != "No")

table(knn.pred, test.Y)

# Fit logistic Regression model
glm.fit = glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs = predict(glm.fit, Caravan[test,], type="respones")
glm.pred = rep("No", 1000)
glm.pred[glm.pred > 0.25] = "Yes"
table(glm.pred, test.Y)
