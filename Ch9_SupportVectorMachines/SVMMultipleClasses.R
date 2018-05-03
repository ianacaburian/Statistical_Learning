library(e1071)
library(ROCR)
#______________________________________________________________________________

set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))

# Generate a third class of observations for svm() to perform 
# multi-class classification using the one-versus-one approach.
set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))

# Fit an SVM to this data.
svmfit <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
#   Note that svm() can perform support vector regression if the response
#   is numerical.
plot(svmfit, dat)
#   Three regions are now assigned.