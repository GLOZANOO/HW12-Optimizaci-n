# Tarea 12 problema 2 Warm Up - subproblema 4.14

#a)
library(ISLR)
summary(Auto)

attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)

#b)
cor(Auto[, -9])
pairs(Auto)

#c)
train <- (year%%2 == 0)
test <- !train
Auto.train <- Auto[train, ]
Auto.test <- Auto[test, ]
mpg01.test <- mpg01[test]

#d)
library(MASS)
lda.fit <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
lda.pred <- predict(lda.fit, Auto.test)

mean(lda.pred$class != mpg01.test) #0.1263

#e)
qda.fit <- qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              subset = train)
qda.pred <- predict(qda.fit, Auto.test)

mean(qda.pred$class != mpg01.test) #0.1318

#f)
# Logistic regression
glm.fit <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, 
              family = binomial, subset = train)
glm.probs <- predict(glm.fit, Auto.test, type = "response")
glm.pred <- rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] <- 1

mean(glm.pred != mpg01.test) #0.1208

#g)
library(class)
train.X <- cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X <- cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 <- mpg01[train]
set.seed(1)

# KNN k=1
knn.pred <- knn(train.X, test.X, train.mpg01, k = 1)
mean(knn.pred != mpg01.test) #0.1538

# KNN k=5
knn.pred <- knn(train.X, test.X, train.mpg01, k = 5)
mean(knn.pred != mpg01.test) #0.1483

# KNN k=10
knn.pred <- knn(train.X, test.X, train.mpg01, k = 10)
mean(knn.pred != mpg01.test) #0.1538

# KNN k=20
knn.pred <- knn(train.X, test.X, train.mpg01, k = 20)
mean(knn.pred != mpg01.test) #0.1428
