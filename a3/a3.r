library(ISLR)
library(corrplot)
library(MASS)
attach(Weekly)
# a.
summary(Weekly)
q <- cor(Weekly[c(-9)])
corrplot(q, method="number")
plot(Volume)
plot(Year, Volume)
# b.
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial)
summary(fit.glm)
# c.
glm.probs = predict(glm.fit, type="response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)
# d.
train = (Year < 2009)
Weekly.0910 = Weekly[!train,]
glm.fit = glm(Direction~Lag2, family=binomial, subset=train)
glm.probs = predict(glm.fit, Weekly.0910, type="response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.0910 = Direction[!train]
table(glm.pred, Direction.0910)
mean(glm.pred == Direction.0910)
# e. 
lda.fit = lda(Direction ~ Lag2, subset=train)
lda.pred = predict(lda.fit, Weekly.0910)
table(lda.pred$class, Direction.0910)
mean(lda.pred$class == Direction.0910)
# f.
qda.fit = qda(Direction~Lag2, data=Weekly, subset=train)
qda.class = predict(qda.fit, Weekly.0910)$class
table(qda.class, Direction.0910)
mean(qda.class == Direction.0910)
# g.
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)
# i.
knn.pred = knn(train.X, test.X, train.Direction, k=10)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

knn.pred = knn(train.X, test.X, train.Direction, k=50)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

knn.pred = knn(train.X, test.X, train.Direction, k=100)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)

qda.fit = qda(Direction~Lag2+sqrt(abs(Lag2)), subset=train)
qda.class = predict(qda.fit, Weekly.0910)$class
table(qda.class, Direction.0910)
mean(qda.class == Direction.0910)

lda.fit = lda(Direction ~ Lag2:Lag1, subset=train)
lda.pred = predict(lda.fit, Weekly.0910)
mean(lda.pred$class == Direction.0910)

glm.fit = glm(Direction~Lag2:Lag1, family=binomial, subset=train)
glm.probs = predict(glm.fit, Weekly.0910, type="response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs>.5] = "Up"
Direction.0910 = Direction[!train]
table(glm.pred, Direction.0910)
mean(glm.pred == Direction.0910)

