# Tarea 12 problema 2 Warm Up - subproblema 4.13

#a)
install.packages('ISLR')
library(ISLR)
summary(Weekly)

pairs(Weekly)

cor(Weekly[, -9])

#En las graficas de pairs unicamente se observa una patr?n en las variables "year" y "volume"

#b)
attach(Weekly)
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, 
              family = binomial)
summary(glm.fit)
#En Lag 2 se observa significancia estadistica con 3%

#c)
glm.probs <- predict(glm.fit, type = "response")
glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction)
#La regresi?n logistica predice e buena manera cuando tenemos incremento pero cuando
#desciende no tiene una buena predicci?n

#d)
train <- (Year < 2009)
Weekly.2009 <- Weekly[!train, ]
glm.fit <- glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Weekly.2009, type = "response")
glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] <- "Up"
Direction.2009 <- Direction[!train]
table(glm.pred, Direction.2009)

mean(glm.pred == Direction.2009)

#e)
library(MASS)
lda.fit <- lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.pred <- predict(lda.fit, Weekly.2009)
table(lda.pred$class, Direction.2009)

mean(lda.pred$class == Direction.2009)

#f)
qda.fit <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.class <- predict(qda.fit, Weekly.2009)$class
table(qda.class, Direction.2009)

mean(qda.class == Direction.2009)

#g)
library(class)
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Direction <- Direction[train]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2009)

mean(knn.pred == Direction.2009)

#h
#Regresi?n logistica y LDA tienen tasas de error muy similares (~0.625)

#i)
#Regresi?n logistica con Lag 1 + Lag 2
glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Weekly.2009, type = "response")
glm.pred <- rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] <- "Up"
Direction.2009 <- Direction[!train]
table(glm.pred, Direction.2009)

mean(glm.pred == Direction.2009) #0.5769

#LDA con Lag1 + Lag2
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Weekly, subset = train)
lda.pred <- predict(lda.fit, Weekly.2009)

mean(lda.pred$class == Direction.2009) #0.5769

#KNN k=5
knn.pred <- knn(train.X, test.X, train.Direction, k = 5)
table(knn.pred, Direction.2009)

mean(knn.pred == Direction.2009) #0.5384

#KNN k=10
knn.pred <- knn(train.X, test.X, train.Direction, k = 10)
table(knn.pred, Direction.2009)

mean(knn.pred == Direction.2009) #0.5576

#KNN k=20
knn.pred <- knn(train.X, test.X, train.Direction, k = 20)
table(knn.pred, Direction.2009)

mean(knn.pred == Direction.2009) #0.5865

#KNN k=50
knn.pred <- knn(train.X, test.X, train.Direction, k = 50)
table(knn.pred, Direction.2009)

mean(knn.pred == Direction.2009) #0.5769
