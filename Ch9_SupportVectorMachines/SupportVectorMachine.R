library(e1071)
#______________________________________________________________________________

# Fit an SVM using a non-linear kernel.
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2

y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))

plot(x, col = y)
train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train,])
#   There appears to be a fair number of training errors in this SVM fit.
#   The boundary appears wholly elliptical.
summary(svmfit)

# Reduce errors by increasing the cost value.
svmfit <- svm(y ~ ., data = dat[train,], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train,])
#   Increasing cost causes the boundary to become irregular with a greater risk
#   of overfitting.

# Select the best gamma via CV.
set.seed(1)
tune.out <- tune(svm, y ~ ., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)
#> - best parameters:
#>  cost gamma
#>     1     2
table(true = dat[-train, "y"], pred = predict(tune.out$best.model,
      newdata = dat[-train,]))
#>     pred
#> true  1  2
#>    1 74  3
#>    2  7 16

#   10% of test observations are misclassified by this SVM.