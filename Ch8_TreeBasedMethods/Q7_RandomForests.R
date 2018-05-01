library(MASS)
library(randomForest)

set.seed(1)
p <- ncol(Boston) - 1
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
y <- which(names(Boston) == "medv")
Boston.train <- Boston[train, -y]
Boston.test <- Boston[-train, -y]
Y.train <- Boston[train, y]
Y.test <- Boston[-train, y]


# Using m = p (bagging)
rf.boston1 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = p, ntree = 500)

# Using m = p/2
rf.boston2 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = p / 2, ntree = 500)

# Using m = sqrt(p)
rf.boston3 <- randomForest(Boston.train, y = Y.train, xtest = Boston.test, ytest = Y.test, mtry = sqrt(p), ntree = 500)

# Plots
plot(1:500, rf.boston1$test$mse, col = "green", type = "l", xlab = "Number of Trees", ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston3$test$mse, col = "blue", type = "l")
legend("topright", c("m = p", "m = p/2", "m = sqrt(p)"), col = c("green", "red", "blue"), cex = 1, lty = 1)